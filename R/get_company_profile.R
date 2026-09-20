get_company_profile <- function(ticker) {
  url <- paste0("https://www.screener.in/company/", ticker, "/consolidated/")
  page <- read_html(url)
  
  company_name <- tryCatch(
    page %>% html_element("h1") %>% html_text2(),
    error = function(e) NA_character_
  )
  
  about <- tryCatch({
    txt <- page %>%
      html_elements(xpath = "//div[contains(@class,'company-profile')]//p") %>%
      html_text2()
    if (length(txt) == 0) NA_character_ else txt[1]
  }, error = function(e) NA_character_)
  
  sector <- tryCatch({
    s <- page %>% html_elements("a[title='Sector']") %>% html_text2()
    if (length(s) == 0) NA_character_ else s[1]
  }, error = function(e) NA_character_)
  
  industry <- tryCatch({
    i <- page %>% html_elements("a[title='Industry']") %>% html_text2()
    if (length(i) == 0) NA_character_ else i[1]
  }, error = function(e) NA_character_)
  
  pros <- tryCatch(
    page %>% html_elements(".pros li") %>% html_text2(),
    error = function(e) character(0)
  )
  
  cons <- tryCatch(
    page %>% html_elements(".cons li") %>% html_text2(),
    error = function(e) character(0)
  )
  
  list(
    company_name = company_name,
    about = about,
    sector = sector,
    industry = industry,
    pros = pros,
    cons = cons
  )
}