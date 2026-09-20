analyze_stock <- function(ticker, years = 10) {
  url <- paste0("https://www.screener.in/company/", ticker, "/consolidated/")
  page <- read_html(url)
  
  metrics <- get_buffett_metrics(ticker)
  checklist <- buffett_checklist(metrics)
  stage_a <- stage_a_initial_return(metrics, page)
  stage_b <- stage_b_roe_method(metrics, page, years = years)
  stage_c <- stage_c_eps_method(metrics, page, years = years)
  stage_d <- stage_d_verdict(stage_b, stage_c)
  stage_e <- stage_e_pe_scenarios(ticker, metrics, stage_b, stage_c, years = years)
  dividends <- project_dividends(stage_b, stage_c, current_eps = stage_a$current_eps, years = years)  
  stage_f <- stage_f_total_return(stage_b, stage_c, dividends, years = years)
  stage_g <- stage_g_total_verdict(stage_f)
  
  list(
    ticker = ticker,
    metrics = metrics,
    checklist = checklist,
    stage_a = stage_a,
    stage_b = stage_b,
    stage_c = stage_c,
    stage_d = stage_d,
    stage_e = stage_e,
    dividends = dividends,
    stage_f = stage_f,
    stage_g = stage_g
  )
}