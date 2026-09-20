stage_a_initial_return <- function(metrics, page) {
  # Current EPS = most recent year's EPS
  eps_vals <- as.numeric(metrics$eps)
  current_eps <- eps_vals[length(eps_vals)]
  
  # Current price from top-ratios
  price_text <- page %>% html_elements("#top-ratios li") %>% html_text2()
  current_price <- price_text[grepl("Current Price", price_text)] %>% 
    gsub("[^0-9.]", "", .) %>% as.numeric()
  
  initial_rate_of_return <- (current_eps / current_price) * 100
  
  list(
    current_eps = current_eps,
    current_price = current_price,
    initial_rate_of_return = initial_rate_of_return
  )
}