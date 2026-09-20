library(rvest)
library(dplyr)
library(tidyquant)

files <- c(
  "clean_num.R",
  "get_buffett_metrics.R",
  "buffett_checklist.R",
  "stage_a_initial_return.R",
  "stage_b_roe_method.R",
  "stage_c_eps_method.R",
  "stage_d_verdict.R",
  "get_historical_pe.R",
  "stage_e_pe_scenarios.R",
  "project_dividends.R",
  "analyze_stock.R",
  "stage_f_total_return.R",
  "stage_g_total_verdict.R",
  "get_company_profile.R"
  
)

for (f in files) {
  source(file.path("R", f))
}