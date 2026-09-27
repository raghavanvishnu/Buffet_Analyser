#' Clean a roic.ai numeric string: strips commas, treats "- -" / "-" as NA
#'
#' @param x Character vector of numbers as shown on roic.ai (e.g. "1,234", "- -")
#' @return Numeric vector
clean_num_us <- function(x) {
  x <- trimws(x)
  x[x %in% c("-", "- -", "")] <- NA
  x <- gsub(",", "", x)
  x <- gsub("[()]", "-", x)  # (123) style negatives, if present
  as.numeric(x)
}

#' Fetch a roic.ai page and return its tables, paired as (header, data)
#'
#' roic.ai pages alternate a 1-row "as of date" header table with a data
#' table for each statement section. This pairs them up and combines each
#' pair into one properly-labeled tibble.
#'
#' @param url The roic.ai page URL to fetch
#' @return A list of combined tibbles, one per statement section found
fetch_roic_tables <- function(url) {
  resp <- httr::GET(url, httr::add_headers("User-Agent" = "Mozilla/5.0 (Windows NT 10.0; Win64; x64)"))
  if (httr::status_code(resp) != 200) {
    stop("Could not load roic.ai page: ", url, " (HTTP ", httr::status_code(resp), ")")
  }
  page <- read_html(httr::content(resp, as = "text", encoding = "UTF-8"))
  all_tables <- page %>% html_elements("table")
  if (length(all_tables) < 2) {
    stop("roic.ai page structure not recognized at: ", url, " (found ", length(all_tables), " tables)")
  }
  
  combined <- list()
  i <- 1
  while (i < length(all_tables)) {
    header_tbl <- html_table(all_tables[[i]], fill = TRUE)
    data_tbl <- html_table(all_tables[[i + 1]], fill = TRUE)
    if (nrow(header_tbl) == 1 && nrow(data_tbl) > 1) {
      names(data_tbl) <- c("Line Item", as.character(header_tbl[1, -1]))
      combined[[length(combined) + 1]] <- data_tbl
      i <- i + 2
    } else {
      i <- i + 1
    }
  }
  combined
}

#' Find a row by label pattern across a list of combined tables
#'
#' @param tables List of tibbles from fetch_roic_tables()
#' @param pattern Regex pattern to match against the "Line Item" column
#' @param label Human-readable label for error messages
#' @return A one-row data.frame of just the year columns (no Line Item column)
find_roic_row <- function(tables, pattern, label) {
  for (tbl in tables) {
    if (!"Line Item" %in% names(tbl)) next
    match_row <- tbl[grepl(pattern, tbl[["Line Item"]], ignore.case = TRUE, perl = TRUE), ]
    if (nrow(match_row) > 0) {
      return(match_row[1, setdiff(names(match_row), "Line Item"), drop = FALSE])
    }
  }
  stop("Could not find a '", label, "' row in roic.ai's tables. The site's layout or wording may have changed.")
}

#' Scrape core financial metrics for a US ticker from roic.ai
#'
#' US equivalent of get_buffett_metrics.R. Returns the same list shape
#' (ticker, roe, eps, roce, debt_to_net_income, free_cash_flow) so every
#' downstream Stage A-I function works unchanged regardless of market.
#'
#' @param ticker US ticker symbol as used by roic.ai (e.g. "AAPL")
#' @return A list with ticker, roe, eps, roce, debt_to_net_income, free_cash_flow
get_buffett_metrics_us <- function(ticker) {
  fin_tables <- fetch_roic_tables(paste0("https://www.roic.ai/quote/", ticker, "/financials"))
  ratio_tables <- fetch_roic_tables(paste0("https://www.roic.ai/quote/", ticker, "/ratios"))
  
  net_income <- find_roic_row(fin_tables, "^\\+?\\s*Net Income$", "Net Income") %>%
    mutate(across(everything(), clean_num_us))
  
  eps <- find_roic_row(fin_tables, "Diluted EPS|EPS.*Diluted|Earnings Per Share", "EPS") %>%
    mutate(across(everything(), clean_num_us))
  
  fcf <- find_roic_row(fin_tables, "Free Cash Flow", "Free Cash Flow") %>%
    mutate(across(everything(), clean_num_us))
  
  dps <- tryCatch(
    find_roic_row(fin_tables, "Dividend per Share", "Dividends Per Share") %>%
      mutate(across(everything(), clean_num_us)),
    error = function(e) NULL
  )
  
  
  roe <- find_roic_row(ratio_tables, "Return on Common Equity", "Return on Equity") %>%
    mutate(across(everything(), clean_num_us))
  
  roce <- find_roic_row(ratio_tables, "Return on Capital(?! Employed)", "Return on Capital") %>%
    mutate(across(everything(), clean_num_us))
  
  total_debt <- find_roic_row(ratio_tables, "^Total Debt$", "Total Debt") %>%
    mutate(across(everything(), clean_num_us))
  
  # Align columns: ratio tables and financial tables may have slightly
  # different year sets (e.g. ratios page includes a TTM column). Use
  # only the years present in both.
  common_years <- intersect(names(net_income), names(total_debt))
  net_income <- net_income[, common_years, drop = FALSE]
  total_debt_aligned <- total_debt[, common_years, drop = FALSE]
  debt_to_net_income <- total_debt_aligned / net_income
  
  list(
    ticker = ticker,
    roe = roe,
    eps = eps,
    roce = roce,
    debt_to_net_income = debt_to_net_income,
    free_cash_flow = fcf,
    dividends_per_share = dps
  )
}