stage_b_roe_method <- function(metrics, page, pe_to_use, years = 10) {
  roe_vals <- as.numeric(metrics$roe)
  avg_roe <- mean(tail(roe_vals, years), na.rm = TRUE)
  
  pl_table <- page %>% html_element("#profit-loss table") %>% html_table() %>% rename(metric = 1)
  payout <- pl_table %>% filter(grepl("Dividend Payout", metric)) %>%
    select(-metric, -TTM) %>% mutate(across(everything(), ~ clean_num(gsub("%", "", .))))
  avg_payout <- mean(tail(as.numeric(payout), years), na.rm = TRUE)
  
  growth_rate <- avg_roe * (1 - avg_payout / 100)
  
  ratios_text <- page %>% html_elements("#top-ratios li") %>% html_text2()
  book_value <- ratios_text[grepl("Book Value", ratios_text)] %>% gsub("[^0-9.]", "", .) %>% as.numeric()
  
  future_book_value <- book_value * (1 + growth_rate / 100)^years
  future_eps <- future_book_value * (avg_roe / 100)
  future_price <- future_eps * pe_to_use
  
  current_price <- ratios_text[grepl("Current Price", ratios_text)] %>% gsub("[^0-9.]", "", .) %>% as.numeric()
  
  expected_return <- ((future_price / current_price)^(1/years) - 1) * 100
  
  list(
    avg_roe = avg_roe,
    avg_payout = avg_payout,
    growth_rate = growth_rate,
    book_value = book_value,
    future_book_value = future_book_value,
    future_eps = future_eps,
    pe_ratio = pe_to_use,
    future_price = future_price,
    current_price = current_price,
    expected_return_roe_method = expected_return
  )
}