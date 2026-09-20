get_buffett_metrics <- function(ticker) {
  url <- paste0("https://www.screener.in/company/", ticker, "/consolidated/")
  page <- read_html(url)
  
  pl_table <- page %>% html_element("#profit-loss table") %>% html_table() %>% rename(metric = 1)
  bs_table <- page %>% html_element("#balance-sheet table") %>% html_table() %>% rename(metric = 1)
  ratios_table <- page %>% html_element("#ratios table") %>% html_table() %>% rename(metric = 1)
  cf_table <- page %>% html_element("#cash-flow table") %>% html_table() %>% rename(metric = 1)
  
  # Net Profit, Equity, Reserves -> ROE
  net_profit <- pl_table %>% filter(grepl("Net Profit", metric)) %>% select(-metric, -TTM) %>% mutate(across(everything(), clean_num))
  equity <- bs_table %>% filter(grepl("Equity Capital", metric)) %>% select(-metric) %>% mutate(across(everything(), clean_num))
  reserves <- bs_table %>% filter(grepl("Reserves", metric)) %>% select(-metric) %>% mutate(across(everything(), clean_num))
  roe <- net_profit / (equity + reserves) * 100
  
  # EPS trend
  eps <- pl_table %>% filter(grepl("EPS", metric)) %>% select(-metric, -TTM) %>% mutate(across(everything(), clean_num))
  
  # ROCE (proxy for ROIC)
  roce <- ratios_table %>% filter(grepl("ROCE", metric)) %>% select(-metric) %>% mutate(across(everything(), ~ clean_num(gsub("%", "", .))))
  
  # Debt vs Net Income
  borrowings <- bs_table %>% filter(grepl("Borrowings", metric)) %>% select(-metric) %>% mutate(across(everything(), clean_num))
  debt_to_ni <- borrowings / net_profit
  
  # Free Cash Flow
  fcf <- cf_table %>% filter(grepl("Free Cash Flow", metric)) %>% select(-metric) %>% mutate(across(everything(), clean_num))
  
  list(
    ticker = ticker,
    roe = roe,
    eps = eps,
    roce = roce,
    debt_to_net_income = debt_to_ni,
    free_cash_flow = fcf
  )
}
