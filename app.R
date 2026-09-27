library(shiny)
library(bslib)
library(rmarkdown)
library(httr)
library(jsonlite)

source("R/search_company.R")
source("R/search_company_us.R")

`%||%` <- function(a, b) if (is.null(a)) b else a

my_theme <- bs_theme(
  version = 5,
  bg = "#ffffff",
  fg = "#2c3e50",
  primary = "#2c3e50",
  secondary = "#eef3f8",
  base_font = font_google("Inter"),
  heading_font = font_google("Inter"),
  "border-radius" = "0.5rem"
)

part1_questions_list <- c(
  "Strong durable competitive advantage?",
  "Do stores need to carry the product to stay in business?",
  "Does the product have a consumer monopoly?",
  "Can it keep raising prices with inflation?",
  "Needs continual plant/factory upgrades?",
  "Product/service unlikely to go obsolete?",
  "Still sold during a recession?",
  "Has little competition?"
)

part1_ui <- function(prefix) {
  tagList(
    h4("Part 1: Qualitative Checklist"),
    p(em("Answer based on your own knowledge of the business — these can't be pulled from financial data."),
      style = "font-size: 0.85rem; color: #6b7280;"),
    radioButtons(paste0(prefix, "_q1"), "Strong durable competitive advantage?", choices = c("Yes", "No"), selected = character(0)),
    radioButtons(paste0(prefix, "_q2"), "Do stores need to carry the product to stay in business?", choices = c("Yes", "No"), selected = character(0)),
    radioButtons(paste0(prefix, "_q3"), "Does the product have a consumer monopoly?", choices = c("Yes", "No"), selected = character(0)),
    radioButtons(paste0(prefix, "_q4"), "Can it keep raising prices with inflation?", choices = c("Yes", "No"), selected = character(0)),
    radioButtons(paste0(prefix, "_q5"), "Needs continual plant/factory upgrades?", choices = c("Yes", "No"), selected = character(0)),
    radioButtons(paste0(prefix, "_q6"), "Product/service unlikely to go obsolete?", choices = c("Yes", "No"), selected = character(0)),
    radioButtons(paste0(prefix, "_q7"), "Still sold during a recession?", choices = c("Yes", "No"), selected = character(0)),
    radioButtons(paste0(prefix, "_q8"), "Has little competition?", choices = c("Yes", "No"), selected = character(0))
  )
}

collect_part1_answers <- function(input, prefix) {
  c(
    input[[paste0(prefix, "_q1")]] %||% "Not answered",
    input[[paste0(prefix, "_q2")]] %||% "Not answered",
    input[[paste0(prefix, "_q3")]] %||% "Not answered",
    input[[paste0(prefix, "_q4")]] %||% "Not answered",
    input[[paste0(prefix, "_q5")]] %||% "Not answered",
    input[[paste0(prefix, "_q6")]] %||% "Not answered",
    input[[paste0(prefix, "_q7")]] %||% "Not answered",
    input[[paste0(prefix, "_q8")]] %||% "Not answered"
  )
}

ui <- fluidPage(
  theme = my_theme,
  tags$head(
    tags$style(HTML("
      .app-header {
        background: linear-gradient(135deg, #2c3e50 0%, #34495e 100%);
        color: white;
        padding: 24px 28px;
        border-radius: 0.5rem;
        margin-bottom: 24px;
      }
      .app-header h1 { margin: 0; font-size: 1.6rem; font-weight: 700; }
      .app-header p { margin: 6px 0 0 0; opacity: 0.85; font-size: 0.9rem; }
      .section-card {
        background: #f9fafb;
        border: 1px solid #e5e7eb;
        border-radius: 0.5rem;
        padding: 16px 18px;
        margin-bottom: 18px;
      }
      .section-title {
        font-weight: 700;
        color: #2c3e50;
        margin-bottom: 8px;
        font-size: 0.95rem;
        text-transform: uppercase;
        letter-spacing: 0.03em;
      }
      .btn-primary {
        background-color: #2c3e50 !important;
        border-color: #2c3e50 !important;
      }
      .btn-primary:hover {
        background-color: #34495e !important;
      }
      .report-frame {
        width: 100%;
        height: 85vh;
        border: 1px solid #e5e7eb;
        border-radius: 0.5rem;
      }
      .placeholder-box {
        display: flex;
        align-items: center;
        justify-content: center;
        height: 60vh;
        color: #9ca3af;
        font-size: 1rem;
        text-align: center;
        border: 2px dashed #e5e7eb;
        border-radius: 0.5rem;
      }
    "))
  ),
  
  div(class = "app-header",
      h1("Buffett-Style Stock Analysis"),
      p("Search a company, answer a short qualitative checklist, and generate a full valuation report.")
  ),
  
  tabsetPanel(
    id = "market_tab",
    
    tabPanel("India (NSE)",
             br(),
             sidebarLayout(
               sidebarPanel(
                 width = 4,
                 div(class = "section-card",
                     div(class = "section-title", "1. Find the Company"),
                     textInput("in_company_query", "Company Name:", placeholder = "e.g. Tata Consultancy, Infosys"),
                     actionButton("in_search_btn", "Search", icon = icon("magnifying-glass"), class = "btn-primary"),
                     uiOutput("in_ticker_choice_ui")
                 ),
                 div(class = "section-card", part1_ui("in")),
                 div(class = "section-card",
                     div(class = "section-title", "3. Generate"),
                     actionButton("in_generate", "Generate Report", icon = icon("chart-line"), class = "btn-primary", width = "100%"),
                     br(), br(),
                     downloadButton("in_download", "Download Report", width = "100%")
                 )
               ),
               mainPanel(
                 width = 8,
                 uiOutput("in_status"),
                 uiOutput("in_report_display")
               )
             )
    ),
    
    tabPanel("US (NYSE/NASDAQ)",
             br(),
             sidebarLayout(
               sidebarPanel(
                 width = 4,
                 div(class = "section-card",
                     div(class = "section-title", "1. Find the Company"),
                     textInput("us_company_query", "Company Name or Ticker:", placeholder = "e.g. Apple, Coca-Cola, MSFT"),
                     actionButton("us_search_btn", "Search", icon = icon("magnifying-glass"), class = "btn-primary"),
                     uiOutput("us_ticker_choice_ui")
                 ),
                 div(class = "section-card", part1_ui("us")),
                 div(class = "section-card",
                     div(class = "section-title", "3. Generate"),
                     actionButton("us_generate", "Generate Report", icon = icon("chart-line"), class = "btn-primary", width = "100%"),
                     br(), br(),
                     downloadButton("us_download", "Download Report", width = "100%")
                 )
               ),
               mainPanel(
                 width = 8,
                 uiOutput("us_status"),
                 uiOutput("us_report_display")
               )
             )
    )
  )
)

server <- function(input, output, session) {
  
  ## ---------- INDIA TAB ----------
  in_report_path <- reactiveVal(NULL)
  in_report_html <- reactiveVal(NULL)
  in_search_results <- reactiveVal(NULL)
  
  output$in_report_display <- renderUI({
    if (is.null(in_report_html())) {
      div(class = "placeholder-box", "Your report will appear here once generated.")
    } else {
      tags$iframe(srcdoc = in_report_html(), class = "report-frame")
    }
  })
  
  observeEvent(input$in_search_btn, {
    req(input$in_company_query)
    output$in_status <- renderUI(tags$p("Searching...", style = "color:#6b7280;"))
    results <- search_company(input$in_company_query)
    in_search_results(results)
    if (nrow(results) == 0) {
      output$in_status <- renderUI(tags$p(style = "color:#dc2626;", "No matches found. Try a different name."))
    } else {
      output$in_status <- renderUI(tags$p(style = "color:#16a34a;", paste(nrow(results), "match(es) found.")))
    }
  })
  
  output$in_ticker_choice_ui <- renderUI({
    results <- in_search_results()
    if (is.null(results) || nrow(results) == 0) return(NULL)
    choices <- setNames(results$ticker, paste0(results$name, " (", results$ticker, ")"))
    selectInput("in_selected_ticker", "Select the company:", choices = choices)
  })
  
  observeEvent(input$in_generate, {
    ticker <- input$in_selected_ticker %||% input$in_company_query
    req(ticker)
    output$in_status <- renderUI(tags$p("Generating report... this may take a moment.", style = "color:#6b7280;"))
    in_report_html(NULL)
    
    out_file <- tempfile(fileext = ".html")
    answers <- collect_part1_answers(input, "in")
    
    result <- tryCatch({
      rmarkdown::render(
        "report_template.Rmd",
        output_format = "html_document",
        output_file = out_file,
        params = list(ticker = toupper(ticker), part1_questions = part1_questions_list, part1_answers = answers),
        envir = new.env()
      )
      TRUE
    }, error = function(e) {
      output$in_status <- renderUI(tags$p(style = "color:#dc2626;", paste("Could not generate report. Error:", e$message)))
      FALSE
    })
    
    if (isTRUE(result)) {
      in_report_path(out_file)
      in_report_html(paste(readLines(out_file, warn = FALSE, encoding = "UTF-8"), collapse = "\n"))
      output$in_status <- renderUI(tags$p(style = "color:#16a34a; font-weight:600;", "Report ready below."))
    }
  })
  
  output$in_download <- downloadHandler(
    filename = function() {
      ticker <- input$in_selected_ticker %||% input$in_company_query %||% "report"
      paste0(toupper(ticker), "_buffett_report.html")
    },
    content = function(file) { req(in_report_path()); file.copy(in_report_path(), file) }
  )
  
  ## ---------- US TAB ----------
  us_report_path <- reactiveVal(NULL)
  us_report_html <- reactiveVal(NULL)
  us_search_results <- reactiveVal(NULL)
  
  output$us_report_display <- renderUI({
    if (is.null(us_report_html())) {
      div(class = "placeholder-box", "Your report will appear here once generated.")
    } else {
      tags$iframe(srcdoc = us_report_html(), class = "report-frame")
    }
  })
  
  observeEvent(input$us_search_btn, {
    req(input$us_company_query)
    output$us_status <- renderUI(tags$p("Searching...", style = "color:#6b7280;"))
    results <- search_company_us(input$us_company_query)
    us_search_results(results)
    if (nrow(results) == 0) {
      output$us_status <- renderUI(tags$p(style = "color:#dc2626;", "No matches found. Try a different name or the exact ticker."))
    } else {
      output$us_status <- renderUI(tags$p(style = "color:#16a34a;", paste(nrow(results), "match(es) found.")))
    }
  })
  
  output$us_ticker_choice_ui <- renderUI({
    results <- us_search_results()
    if (is.null(results) || nrow(results) == 0) return(NULL)
    choices <- setNames(results$ticker, paste0(results$name, " (", results$ticker, ")"))
    selectInput("us_selected_ticker", "Select the company:", choices = choices)
  })
  
  observeEvent(input$us_generate, {
    ticker <- input$us_selected_ticker %||% input$us_company_query
    req(ticker)
    output$us_status <- renderUI(tags$p("Generating report... this may take a moment.", style = "color:#6b7280;"))
    us_report_html(NULL)
    
    out_file <- tempfile(fileext = ".html")
    answers <- collect_part1_answers(input, "us")
    
    result <- tryCatch({
      rmarkdown::render(
        "report_template_us.Rmd",
        output_format = "html_document",
        output_file = out_file,
        params = list(ticker = toupper(ticker), part1_questions = part1_questions_list, part1_answers = answers),
        envir = new.env()
      )
      TRUE
    }, error = function(e) {
      output$us_status <- renderUI(tags$p(style = "color:#dc2626;", paste("Could not generate report. Error:", e$message)))
      FALSE
    })
    
    if (isTRUE(result)) {
      us_report_path(out_file)
      us_report_html(paste(readLines(out_file, warn = FALSE, encoding = "UTF-8"), collapse = "\n"))
      output$us_status <- renderUI(tags$p(style = "color:#16a34a; font-weight:600;", "Report ready below."))
    }
  })
  
  output$us_download <- downloadHandler(
    filename = function() {
      ticker <- input$us_selected_ticker %||% input$us_company_query %||% "report"
      paste0(toupper(ticker), "_buffett_report_us.html")
    },
    content = function(file) { req(us_report_path()); file.copy(us_report_path(), file) }
  )
}

shinyApp(ui, server)