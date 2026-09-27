library(shiny)
library(rmarkdown)
library(httr)
library(jsonlite)

source("R/search_company.R")

`%||%` <- function(a, b) if (is.null(a)) b else a

ui <- fluidPage(
  titlePanel("Buffett-Style Stock Analysis"),
  sidebarLayout(
    sidebarPanel(
      textInput("company_query", "Company Name:", placeholder = "e.g. Tata Consultancy, Infosys"),
      actionButton("search_btn", "Search"),
      br(), br(),
      uiOutput("ticker_choice_ui"),
      
      radioButtons("format", "Report Format:",
                   choices = c("HTML" = "html_document")),
      hr(),
      h4("Part 1: Qualitative Checklist"),
      p(em("Answer based on your own knowledge of the business — these can't be pulled from financial data.")),
      radioButtons("q1", "Strong durable competitive advantage?", choices = c("Yes", "No"), selected = character(0)),
      radioButtons("q2", "Do stores need to carry the product to stay in business?", choices = c("Yes", "No"), selected = character(0)),
      radioButtons("q3", "Does the product have a consumer monopoly?", choices = c("Yes", "No"), selected = character(0)),
      radioButtons("q4", "Can it keep raising prices with inflation?", choices = c("Yes", "No"), selected = character(0)),
      radioButtons("q5", "Needs continual plant/factory upgrades?", choices = c("Yes", "No"), selected = character(0)),
      radioButtons("q6", "Product/service unlikely to go obsolete?", choices = c("Yes", "No"), selected = character(0)),
      radioButtons("q7", "Still sold during a recession?", choices = c("Yes", "No"), selected = character(0)),
      radioButtons("q8", "Has little competition?", choices = c("Yes", "No"), selected = character(0)),
      hr(),
      actionButton("generate", "Generate Report", class = "btn-primary"),
      br(), br(),
      downloadButton("download", "Download Report")
    ),
    mainPanel(
      uiOutput("status")
    )
  )
)

server <- function(input, output, session) {
  report_path <- reactiveVal(NULL)
  report_ext <- reactiveVal(NULL)
  search_results <- reactiveVal(NULL)
  
  observeEvent(input$search_btn, {
    req(input$company_query)
    output$status <- renderUI(tags$p("Searching..."))
    results <- search_company(input$company_query)
    search_results(results)
    
    if (nrow(results) == 0) {
      output$status <- renderUI(
        tags$p(style = "color:red;", "No matches found. Try a different name, or type the exact NSE ticker below.")
      )
    } else {
      output$status <- renderUI(tags$p(style = "color:green;", paste(nrow(results), "match(es) found.")))
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
    
    output$status <- renderUI(tags$p("Generating report... this may take a moment."))
    
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
        tags$p(style = "color:red;",
               paste("Could not generate report. Check the ticker/company and try again. Error:", e$message))
      )
      FALSE
    })
    
    if (isTRUE(result)) {
      report_path(out_file)
      report_ext(ext)
      output$status <- renderUI(
        tags$p(style = "color:green;", "Report ready. Click Download below.")
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