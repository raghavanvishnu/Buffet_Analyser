library(shiny)
library(bslib)
library(rmarkdown)
library(httr)
library(jsonlite)

source("R/search_company.R")

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
  
  sidebarLayout(
    sidebarPanel(
      width = 4,
      div(class = "section-card",
          div(class = "section-title", "1. Find the Company"),
          textInput("company_query", NULL, placeholder = "e.g. Tata Consultancy, Infosys"),
          actionButton("search_btn", "Search", icon = icon("magnifying-glass"), class = "btn-primary"),
          uiOutput("ticker_choice_ui")
      ),
      
      div(class = "section-card",
          div(class = "section-title", "2. Qualitative Checklist"),
          p(em("Answer based on your own knowledge of the business — these can't be pulled from financial data."),
            style = "font-size: 0.85rem; color: #6b7280;"),
          radioButtons("q1", "Strong durable competitive advantage?", choices = c("Yes", "No"), selected = character(0)),
          radioButtons("q2", "Do stores need to carry the product to stay in business?", choices = c("Yes", "No"), selected = character(0)),
          radioButtons("q3", "Does the product have a consumer monopoly?", choices = c("Yes", "No"), selected = character(0)),
          radioButtons("q4", "Can it keep raising prices with inflation?", choices = c("Yes", "No"), selected = character(0)),
          radioButtons("q5", "Needs continual plant/factory upgrades?", choices = c("Yes", "No"), selected = character(0)),
          radioButtons("q6", "Product/service unlikely to go obsolete?", choices = c("Yes", "No"), selected = character(0)),
          radioButtons("q7", "Still sold during a recession?", choices = c("Yes", "No"), selected = character(0)),
          radioButtons("q8", "Has little competition?", choices = c("Yes", "No"), selected = character(0))
      ),
      
      div(class = "section-card",
          div(class = "section-title", "3. Generate"),
          actionButton("generate", "Generate Report", icon = icon("chart-line"), class = "btn-primary", width = "100%"),
          br(), br(),
          downloadButton("download", "Download Report", width = "100%")
      )
    ),
    
    mainPanel(
      width = 8,
      uiOutput("status"),
      uiOutput("report_display")
    )
  )
)

server <- function(input, output, session) {
  report_path <- reactiveVal(NULL)
  report_ext <- reactiveVal(NULL)
  search_results <- reactiveVal(NULL)
  report_html <- reactiveVal(NULL)
  
  output$report_display <- renderUI({
    if (is.null(report_html())) {
      div(class = "placeholder-box", "Your report will appear here once generated.")
    } else {
      tags$iframe(srcdoc = report_html(), class = "report-frame")
    }
  })
  
  observeEvent(input$search_btn, {
    req(input$company_query)
    output$status <- renderUI(tags$p("Searching...", style = "color:#6b7280;"))
    results <- search_company(input$company_query)
    search_results(results)
    
    if (nrow(results) == 0) {
      output$status <- renderUI(
        tags$p(style = "color:#dc2626;", "No matches found. Try a different name, or type the exact NSE ticker below.")
      )
    } else {
      output$status <- renderUI(tags$p(style = "color:#16a34a;", paste(nrow(results), "match(es) found.")))
    }
  })
  
  output$ticker_choice_ui <- renderUI({
    results <- search_results()
    if (is.null(results) || nrow(results) == 0) return(NULL)
    
    choices <- setNames(results$ticker, paste0(results$name, " (", results$ticker, ")"))
    selectInput("selected_ticker", "Select the company:", choices = choices)
  })
  
  observeEvent(input$generate, {
    ticker <- input$selected_ticker %||% input$company_query
    req(ticker)
    
    output$status <- renderUI(tags$p("Generating report... this may take a moment.", style = "color:#6b7280;"))
    report_html(NULL)
    
    ext <- ".html"
    out_file <- tempfile(fileext = ext)
    
    part1_questions <- c(
      "Strong durable competitive advantage?",
      "Do stores need to carry the product to stay in business?",
      "Does the product have a consumer monopoly?",
      "Can it keep raising prices with inflation?",
      "Needs continual plant/factory upgrades?",
      "Product/service unlikely to go obsolete?",
      "Still sold during a recession?",
      "Has little competition?"
    )
    part1_answers <- c(
      input$q1 %||% "Not answered",
      input$q2 %||% "Not answered",
      input$q3 %||% "Not answered",
      input$q4 %||% "Not answered",
      input$q5 %||% "Not answered",
      input$q6 %||% "Not answered",
      input$q7 %||% "Not answered",
      input$q8 %||% "Not answered"
    )
    
    result <- tryCatch({
      rmarkdown::render(
        "report_template.Rmd",
        output_format = "html_document",
        output_file = out_file,
        params = list(
          ticker = toupper(ticker),
          part1_questions = part1_questions,
          part1_answers = part1_answers
        ),
        envir = new.env()
      )
      TRUE
    }, error = function(e) {
      output$status <- renderUI(
        tags$p(style = "color:#dc2626;",
               paste("Could not generate report. Check the ticker/company and try again. Error:", e$message))
      )
      FALSE
    })
    
    if (isTRUE(result)) {
      report_path(out_file)
      report_ext(ext)
      report_html(paste(readLines(out_file, warn = FALSE, encoding = "UTF-8"), collapse = "\n"))
      output$status <- renderUI(
        tags$p(style = "color:#16a34a; font-weight:600;", "Report ready below.")
      )
    }
  })
  
  output$download <- downloadHandler(
    filename = function() {
      ticker <- input$selected_ticker %||% input$company_query %||% "report"
      req(report_ext())
      paste0(toupper(ticker), "_buffett_report", report_ext())
    },
    content = function(file) {
      req(report_path())
      file.copy(report_path(), file)
    }
  )
}

shinyApp(ui, server)