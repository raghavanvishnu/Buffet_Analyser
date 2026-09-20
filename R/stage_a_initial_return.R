stage_a_initial_return <- function(metrics, page,
                                   bond_yields = c(
                                     "India (INR, G-Sec)" = 6.70,
                                     "United States (USD)" = 4.24,
                                     "United Kingdom (GBP, Gilt)" = 4.53,
                                     "Germany (EUR, Bund)" = 2.85,
                                     "Japan (JPY)" = 2.25,
                                     "China (CNY)" = 1.80
                                   )) {
  eps_vals <- as.numeric(metrics$eps)
  current_eps <- eps_vals[length(eps_vals)]
  
  price_text <- page %>% html_elements("#top-ratios li") %>% html_text2()
  current_price <- price_text[grepl("Current Price", price_text)] %>%
    gsub("[^0-9.]", "", .) %>% as.numeric()
  
  initial_rate_of_return <- (current_eps / current_price) * 100
  
  bond_comparison <- data.frame(
    Benchmark = names(bond_yields),
    Yield = paste0(bond_yields, "%"),
    Better_Than_Stock = ifelse(initial_rate_of_return > bond_yields, "Stock Wins", "Bond Wins")
  )
  
  list(
    current_eps = current_eps,
    current_price = current_price,
    initial_rate_of_return = initial_rate_of_return,
    bond_comparison = bond_comparison
  )
}