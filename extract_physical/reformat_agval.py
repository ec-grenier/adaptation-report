"""

In command line:  python fix_nc4.py /path/to/projection_root

Reformats agval outputs to be readable by quantiles.py. Parallelizable with WORKERS argument.

"""

import os
import re
import sys
import argparse
from pathlib import Path
from itertools import product
from concurrent.futures import ProcessPoolExecutor, as_completed

import pandas as pd
import xarray as xr

ESTIMATE = "montecarlo"                 # "montecarlo" or "median"
QUERY = "exists==True"                  # pandas query over the path table
TARGET_NAMES = ("disaggregated_damages.nc4",)
WORKERS = 10
DRY_RUN = bool(int(os.environ.get("DRY_RUN", "0"))) 
DRY_RUN_PREVIEW = int(os.environ.get("DRY_RUN_PREVIEW", "20"))

RCP_LIST = ["rcp85", "rcp45"]
GCM_LIST = [
    "ACCESS1-0", "CCSM4", "GFDL-CM3", "IPSL-CM5A-LR", "MIROC-ESM-CHEM",
    "bcc-csm1-1", "CESM1-BGC", "GFDL-ESM2G", "IPSL-CM5A-MR", "MPI-ESM-LR",
    "BNU-ESM", "CNRM-CM5", "GFDL-ESM2M", "MIROC5", "MPI-ESM-MR",
    "CanESM2", "CSIRO-Mk3-6-0", "inmcm4", "MIROC-ESM", "MRI-CGCM3",
    "NorESM1-M", "surrogate_GFDL-CM3_89", "surrogate_GFDL-ESM2G_11",
    "surrogate_CanESM2_99", "surrogate_GFDL-ESM2G_01",
    "surrogate_MRI-CGCM3_11", "surrogate_CanESM2_89",
    "surrogate_GFDL-CM3_94", "surrogate_MRI-CGCM3_01",
    "surrogate_CanESM2_94", "surrogate_GFDL-CM3_99",
    "surrogate_MRI-CGCM3_06", "surrogate_GFDL-ESM2G_06",
]
MODEL_LIST = ["high", "low"]
SSP_LIST = ["SSP3"]

# Invalid (rcp, gcm) pairs to exclude entirely
INVALID_COMBOS = {("rcp85", "ACCESS1-0")}

def _parse_projection_filesys(input_path, estimate="median", query="exists==True"):
    """Build DataFrame with `path` to leaves: <root>/<batch>/<rcp>/<gcm>/<model>/<ssp>."""
    if estimate == "montecarlo":
        input_path = os.path.join(input_path, "montecarlo")
        root = Path(input_path)
        if not root.exists():
            raise NotADirectoryError(f"Not found: {root}")
        batch_folders = [
            re.search(r"batch\d+", d.name).group(0)
            for d in root.iterdir()
            if d.is_dir() and re.search(r"batch\d+", d.name)
        ]
        if not batch_folders:
            raise RuntimeError(f"No batch* folders under {root}")
    elif estimate == "median":
        batch_folders = ["median"]
    else:
        raise ValueError("estimate must be 'montecarlo' or 'median'")

    df = pd.DataFrame(
        list(product(batch_folders, RCP_LIST, GCM_LIST, MODEL_LIST, SSP_LIST)),
        columns=["batch", "rcp", "gcm", "model", "ssp"],
    )

    bad = pd.Series(list(INVALID_COMBOS), dtype="object")
    if not bad.empty:
        mask_bad = False
        for rcp_bad, gcm_bad in INVALID_COMBOS:
            mask_bad |= ((df["rcp"] == rcp_bad) & (df["gcm"] == gcm_bad))
        df = df.loc[~mask_bad].copy()

    df["path"] = df.apply(
        lambda x: os.path.join(input_path, x.batch, x.rcp, x.gcm, x.model, x.ssp),
        axis=1,
    )
    df["exists"] = df.path.apply(lambda p: Path(p).exists())
    return df.query(query)


def list_nc4_from_projection_tree(root_dir, estimate=ESTIMATE, query=QUERY, target_names=TARGET_NAMES):
    """
    ONLY return files named in TARGET_NAMES, searched **recursively** under each leaf.
    """
    df = _parse_projection_filesys(root_dir, estimate=estimate, query=query)
    files = []
    for base in df["path"].astype(str):
        base = Path(base)
        for name in target_names:
            for cand in base.rglob(name):
                if cand.is_file():
                    files.append(cand)

    seen, out = set(), []
    for f in files:
        if f not in seen:
            out.append(f)
            seen.add(f)
    return out

def fix_nc4_inplace(src):
    """
    This makes agval system outputs compatible with the prospectus-tools library

    Squeeze singletons; rename variable 'region'->'regions'; demote 'regions' coord -> data var.
    Overwrites *in place* via temp file + atomic replace.
    Returns (Path, 'ok'|'nochange'|'error:...').
    """

    src = Path(src)
    if not src.exists():
        return src, "error:missing"
    if not src.is_file():
        return src, "error:not-a-file"

    tmp = src.with_suffix(src.suffix + ".tmp")
    try:
        ds = xr.open_dataset(src, decode_cf=False, mask_and_scale=False)
        try:
            ds2 = ds.squeeze(drop=True)

            if "region" in ds2.variables:
                ds2 = ds2.rename_vars({"region": "regions"})
            if hasattr(ds2, "xindexes") and "regions" in ds2.xindexes:
                ds2 = ds2.drop_indexes("regions")
            if "regions" in ds2.coords:
                ds2 = ds2.reset_coords(names=["regions"], drop=False)

            changed = "regions" in ds2.data_vars

            ds2.to_netcdf(tmp, engine="netcdf4")
            os.replace(tmp, src)  # atomic replace
            return src, ("ok" if changed else "nochange")
        finally:
            ds.close()
    except Exception as e:
        try:
            if tmp.exists():
                tmp.unlink()
        except Exception:
            pass
        return src, f"error:{type(e).__name__}: {e}"

def run(root_dir):
    files = list_nc4_from_projection_tree(root_dir)
    if not files:
        print("No candidate disaggregated_damages.nc4 files found.")
        return 2

    total_mb = sum(p.stat().st_size for p in files) / (1024**2)
    print(f"Transforming {len(files)} files (~{total_mb:.1f} MB) with {WORKERS} workers...")
    print("Targets: disaggregated_damages.nc4 only; overwrite=in-place; invalid combos filtered")

    if DRY_RUN:
        print("\nDRY RUN (no files will be modified). Preview:")
        for p in files[:DRY_RUN_PREVIEW]:
            print("  -", p)
        if len(files) > DRY_RUN_PREVIEW:
            print(f"  ... and {len(files) - DRY_RUN_PREVIEW} more")
        return 0

    counts = {"ok": 0, "nochange": 0, "error": 0}
    errors = []

    with ProcessPoolExecutor(max_workers=WORKERS) as ex:
        futs = {ex.submit(fix_nc4_inplace, str(p)): p for p in files}
        for fut in as_completed(futs):
            src_hint = futs[fut]
            try:
                src, status = fut.result()
            except Exception as e:
                status = f"error:{type(e).__name__}: {e}"
                src = src_hint

            kind = status.split(":", 1)[0]
            counts[kind] = counts.get(kind, 0) + 1
            if kind == "error":
                errors.append((src, status))
                print(f"[error   ] {src}")
            else:
                print(f"[{kind:8}] {src}")

    print("\nSummary")
    for k in ("ok", "nochange", "error"):
        print(f"  {k:8}: {counts.get(k,0)}")
    if errors:
        print("\nErrors (first 10):")
        for p, msg in errors[:10]:
            print(f"  {p} -> {msg}")

    return 1 if errors else 0


def main():
    ap = argparse.ArgumentParser(
        description="Fix only 'disaggregated_damages.nc4' files in the projection tree (in place)."
    )
    ap.add_argument("root", help="Projection root (e.g., /project/cil/gcp/outputs/agriculture/impacts-mealy)")
    args = ap.parse_args()
    return run(args.root)


if __name__ == "__main__":
    sys.exit(main())