library(shiny)
library(rmarkdown)

`%||%` <- function(a, b) if (is.null(a)) b else a

ui <- fluidPage(
  titlePanel("Buffett-Style Stock Analysis"),
  sidebarLayout(
    sidebarPanel(
      textInput("ticker", "NSE Ticker Symbol:", placeholder = "e.g. TCS, RELIANCE, ITC"),
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
  
  observeEvent(input$generate, {
    req(input$ticker)
    
    output$status <- renderUI(tags$p("Generating report... this may take a moment."))
    
    ext <- ifelse(input$format == "pdf_document", ".pdf", ".html")
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
        output_format = input$format,
        output_file = out_file,
        params = list(
          ticker = toupper(input$ticker),
          part1_questions = part1_questions,
          part1_answers = part1_answers
        ),
        envir = new.env()
      )
      TRUE
    }, error = function(e) {
      output$status <- renderUI(
        tags$p(style = "color:red;",
               paste("Could not generate report. Check the ticker symbol and try again. Error:", e$message))
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
      req(report_ext())
      paste0(toupper(input$ticker), "_buffett_report", report_ext())
    },
    content = function(file) {
      req(report_path())
      file.copy(report_path(), file)
    }
  )
}

shinyApp(ui, server)