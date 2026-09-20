project_dividends <- function(stage_b, stage_c, current_eps, years = 10) {
  growth_roe <- stage_b$growth_rate / 100
  growth_eps <- stage_c$eps_growth_rate / 100
  payout <- stage_b$avg_payout / 100
  
  eps_path_roe <- current_eps * (1 + growth_roe)^(1:years)
  eps_path_eps <- current_eps * (1 + growth_eps)^(1:years)
  
  div_path_roe <- eps_path_roe * payout
  div_path_eps <- eps_path_eps * payout
  
  table <- data.frame(
    Year = paste0("Year ", 1:years),
    EPS_ROE_Method = round(eps_path_roe, 2),
    Dividend_ROE_Method = round(div_path_roe, 2),
    EPS_EPS_Method = round(eps_path_eps, 2),
    Dividend_EPS_Method = round(div_path_eps, 2)
  )
  
  list(
    table = table,
    total_dividend_roe_method = round(sum(div_path_roe), 2),
    total_dividend_eps_method = round(sum(div_path_eps), 2)
  )
}