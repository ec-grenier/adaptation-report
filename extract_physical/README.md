# Extract physical unit projections

These scripts exists to pull physical unit projection data from each impact sector. 

- Activate the `risingverse-py27` environment.
- Check your config
- run in terminal `bash extract_{sector}.sh`

### Note on running extractions for ag
For ag, we run the agval repo, picking the variable `delta_no_reallocation` for **fulladapt** only (comment out costs in line 173 of `agval/agval.py`.). We pull out impacts in calories (for references, see the ECON 101 graph near the end of the ag paper--we take Q2-Q0.) We then run agval again, only pulling monetized costs, picking variable `wc_no_reallocation` for **costs** only (comment out `df.fulladapt -` in line 173 of `agval/agval.py`). If you haven't run agval yet, see [the agval repo](https://gitlab.com/ClimateImpactLab/Impacts/agval).

Then, run `reformat_agval.py` on the root directories to reformat each netcdf file according to a format that `quantiles.py` will be able to read.

All of the ag steps can be run with the agval environment.