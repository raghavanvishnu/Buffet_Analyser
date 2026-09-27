#' Scrape a company profile for a US ticker from roic.ai's main quote page
#'
#' Pulls CEO, sector, industry, and the business description from the
#' <dt>/<dd> label-value pairs on roic.ai's quote page. Unlike Screener.in,
#' roic.ai has no machine-generated Pros/Cons list, so those stay empty.
#'
#' @param ticker US ticker symbol as used by roic.ai (e.g. "AAPL")
#' @return A list with company_name, about, sector, industry, pros (empty),
#'   cons (empty) - matching the shape get_company_profile() (India) returns
get_company_profile_us <- function(ticker) {
  result <- tryCatch({
    resp <- httr::GET(
      paste0("https://www.roic.ai/quote/", ticker),
      httr::add_headers("User-Agent" = "Mozilla/5.0 (Windows NT 10.0; Win64; x64)")
    )
    if (httr::status_code(resp) != 200) stop("bad status")
    
    page <- read_html(httr::content(resp, as = "text", encoding = "UTF-8"))
    dt_labels <- page %>% html_elements("dt") %>% html_text2()
    dd_values <- page %>% html_elements("dd") %>% html_text2()
    
    get_field <- function(label) {
      idx <- which(dt_labels == label)
      if (length(idx) == 0) return(NA_character_)
      dd_values[idx[1]]
    }
    
    list(
      company_name = ticker,
      sector = get_field("Sector"),
      industry = get_field("Industry"),
      about = get_field("Business"),
      pros = character(0),
      cons = character(0)
    )
  }, error = function(e) {
    list(
      company_name = ticker,
      sector = NA_character_,
      industry = NA_character_,
      about = "",
      pros = character(0),
      cons = character(0)
    )
  })
  
  result
}