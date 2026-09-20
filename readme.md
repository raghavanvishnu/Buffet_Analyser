# Buffett-Style Stock Analyzer

An R Shiny application that runs a simplified version of Warren Buffett's value-investing checklist against any NSE-listed stock, and generates a full, formatted analysis report on demand.

Enter a ticker (e.g. `TCS`, `RELIANCE`, `INFY`), answer a short qualitative checklist, and get back a report covering business quality, historical trends, multi-method valuation, price sensitivity, and dividend projections — as HTML or PDF.

## What it does

Given a ticker, the app:

1. **Scrapes company fundamentals** from [Screener.in](https://www.screener.in) — Profit & Loss, Balance Sheet, Cash Flow, and Ratios (10+ years), plus the company's business description and sector.
2. **Pulls historical price data** from Yahoo Finance via `tidyquant`, and derives historical P/E ratios by combining price history with reported EPS.
3. **Runs a quantitative quality checklist** — EPS trend, ROE, ROCE/ROIC, debt-to-net-income, and free cash flow — based on Buffett's published criteria.
4. **Values the stock two independent ways**:
   - An ROE / book-value driven projection (sustainable growth model)
   - A historical EPS growth-rate projection
5. **Stress-tests the valuation** against the stock's own historical minimum, mean, and maximum P/E — not just today's price.
6. **Computes total return**, combining projected price appreciation with projected dividends, and issues a final verdict (Fantastic / Good / Unacceptable) using Buffett's own thresholds.
7. **Compares the stock's earnings yield** against sovereign bond yields in six major currencies (INR, USD, GBP, EUR, JPY, CNY).
8. **Generates a complete report** — company profile, computation sheets (with the underlying formulas shown), historical trend charts, and a one-line "key takeaway" under every section — downloadable as HTML or PDF.

## Why

Most free stock-screening tools give you a single score with no visibility into how it was calculated. This project shows every step — the actual formula, the actual numbers, and where each number came from — so the report can be checked, not just trusted.

## Tech stack

- **R / Shiny** — front-end for ticker input and report generation
- **rvest** — web scraping (Screener.in)
- **tidyquant / quantmod** — historical price data (Yahoo Finance)
- **dplyr** — data wrangling
- **ggplot2 / plotly** — interactive historical trend charts
- **R Markdown (knitr)** — report generation (HTML/PDF)
- **renv** — locked package versions for reproducibility

## Project structure

```
buffett-analyzer/
├── R/                          # Core logic, one function per file
│   ├── get_buffett_metrics.R   # Scrapes ROE, EPS, ROCE, debt, FCF from Screener
│   ├── get_company_profile.R   # Scrapes company name, sector, description, pros/cons
│   ├── buffett_checklist.R     # Part 2 quantitative Yes/No checklist
│   ├── stage_a_initial_return.R    # EPS/Price vs sovereign bond yields
│   ├── stage_b_roe_method.R        # ROE-based valuation
│   ├── stage_c_eps_method.R        # EPS-growth-based valuation
│   ├── stage_d_verdict.R           # Price-only verdict
│   ├── get_historical_pe.R         # Derives historical PE from price + EPS history
│   ├── stage_e_pe_scenarios.R      # Valuation across min/mean/max historical PE
│   ├── project_dividends.R         # Dividend projection under both growth methods
│   ├── stage_f_total_return.R      # Total return (price + dividends)
│   ├── stage_g_total_verdict.R     # Final verdict including dividends
│   ├── analyze_stock.R             # Master function — runs the full pipeline
│   └── load_all.R                  # Sources every file above
├── report_template.Rmd         # The report itself
├── app.R                       # Shiny front-end
├── renv.lock                   # Locked package versions
└── README.md
```

## Running locally

```r
# Restore the exact package versions this project was built with
renv::restore()

# Launch the app
shiny::runApp()
```

Or generate a report directly, without the UI:

```r
source("R/load_all.R")
rmarkdown::render("report_template.Rmd", params = list(ticker = "TCS"))
```

## Limitations

- Scrapes Screener.in's public pages; if their HTML structure changes, the scrapers may need updating.
- Sovereign bond yields (used in Stage A) are manually set constants, not fetched live — update periodically.
- Consolidated financials only; standalone-vs-consolidated differences are not shown separately.
- No peer or sector comparison — the report evaluates a company only against its own history.
- This is a screening and learning tool, not investment advice.

## Background

This project applies the checklist from Buffett's published value-investing framework (Parts 1–3, including the ROE and EPS expected-return methods and the Fantastic/Good/Unacceptable thresholds) programmatically, extending it with historical PE sensitivity analysis, total-return-with-dividends verdicts, and multi-currency bond comparisons not present in the original framework.
