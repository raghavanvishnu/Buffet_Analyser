stage_b_roe_method <- function(metrics, page, years = 10) {
  # Average ROE over last 10 years
  roe_vals <- as.numeric(metrics$roe)
  avg_roe <- mean(tail(roe_vals, years), na.rm = TRUE)
  
  # Average payout ratio - need to pull from pl_table again
  pl_table <- page %>% html_element("#profit-loss table") %>% html_table() %>% rename(metric = 1)
  payout <- pl_table %>% filter(grepl("Dividend Payout", metric)) %>% 
    select(-metric, -TTM) %>% mutate(across(everything(), ~ clean_num(gsub("%", "", .))))
  avg_payout <- mean(tail(as.numeric(payout), years), na.rm = TRUE)
  
  # Sustainable growth rate
  growth_rate <- avg_roe * (1 - avg_payout / 100)
  
  # Current book value and P/E from top-ratios
  ratios_text <- page %>% html_elements("#top-ratios li") %>% html_text2()
  book_value <- ratios_text[grepl("Book Value", ratios_text)] %>% gsub("[^0-9.]", "", .) %>% as.numeric()
  pe_ratio <- ratios_text[grepl("Stock P/E", ratios_text)] %>% gsub("[^0-9.]", "", .) %>% as.numeric()
  
  # Future book value (10 years)
  future_book_value <- book_value * (1 + growth_rate / 100)^years
  
  # Future EPS = Future Book Value * Avg ROE
  future_eps <- future_book_value * (avg_roe / 100)
  
  # Future stock price = Future EPS * P/E
  future_price <- future_eps * pe_ratio
  
  # Current price
  current_price <- ratios_text[grepl("Current Price", ratios_text)] %>% gsub("[^0-9.]", "", .) %>% as.numeric()
  
  # Expected rate of return: solve for i in Future = Current(1+i)^n
  expected_return <- ((future_price / current_price)^(1/years) - 1) * 100
  
  list(
    avg_roe = avg_roe,
    avg_payout = avg_payout,
    growth_rate = growth_rate,
    book_value = book_value,
    future_book_value = future_book_value,
    future_eps = future_eps,
    pe_ratio = pe_ratio,
    future_price = future_price,
    current_price = current_price,
    expected_return_roe_method = expected_return
  )
}