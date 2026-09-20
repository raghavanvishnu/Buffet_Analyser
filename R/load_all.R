library(rvest)
library(dplyr)

files <- c(
  "clean_num.R",
  "get_buffett_metrics.R",
  "buffett_checklist.R",
  "stage_a_initial_return.R",
  "stage_b_roe_method.R",
  "stage_c_eps_method.R",
  "stage_d_verdict.R",
  "analyze_stock.R"
)

for (f in files) {
  source(file.path("R", f))
}