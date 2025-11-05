packages = c("glue", "tidyverse", "data.table", "reticulate", "scales")
invisible(lapply(packages, function(pkg) {
  suppressPackageStartupMessages(library(pkg, character.only = TRUE))
}))
rm(packages)

df = fread('/project/cil/home_dirs/egrenier/cil-comms/adaptation_report/data/analysis_ready/mortality/combined-fulladapt-ir_level-rates-3_c-midc-SSP2-low-hot-fixed.csv')
df2 = fread("/project/cil/home_dirs/egrenier/cil-comms/adaptation_report/data/analysis_ready/mortality/combined-fulladapt-ir_level-rates-3_c-midc-SSP2-low-hot.csv")
median = fread("/project/cil/home_dirs/egrenier/cil-comms/adaptation_report/data/analysis_ready/mortality/combined-fulladapt-ir_level-rates-3_c-midc-SSP2-low-cold.csv") %>% rename(q50.cold=q50)
df = df %>% select(region, q50) %>% rename(q50.db = q50)
df2 = df2 %>% select(region, q50)
compare = df2 %>% left_join(df)
compare = compare %>% left_join(median)
ggplot(compare, aes(x = q50, y = q50.db)) +
  geom_point(alpha = 0.3, size = 0.5) +  # transparency for overplotting
  geom_abline(slope = 1, intercept = 0, color = "red", linetype = "dashed") +
  labs(x = "median", y = "deltabeta", 
       title = "Comparison across 25k Regions") +
  theme_minimal()

diff = compare$q50.db - compare$q50
compare$diff = diff
rel_diff = (compare$q50.db - compare$q50) / compare$q50 * 100  # percent difference
compare$rel_diff = rel_diff

compare$sign_flip = ifelse(compare$q50 * compare$q50.db < 0, 1, 0)

# Summary statistics
summary_stats <- data.frame(
  metric = c("Mean difference", "Median difference",
             "Correlation", 
             "Mean percent difference", "Median percent difference"),
  value = c(
    mean(diff, na.rm=T),
    median(diff, na.rm=T),
    cor(compare$q50, compare$q50.db, use = "pairwise.complete.obs"),
    mean(rel_diff, na.rm = TRUE),
    median(rel_diff, na.rm = TRUE)
  )
)

print(summary_stats)
›