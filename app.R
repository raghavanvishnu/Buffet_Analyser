library(shiny)
library(rmarkdown)

ui <- fluidPage(
  titlePanel("Buffett-Style Stock Analysis"),
  sidebarLayout(
    sidebarPanel(
      textInput("ticker", "NSE Ticker Symbol:", placeholder = "e.g. TCS, RELIANCE, ITC"),
      radioButtons("format", "Report Format:",
                   choices = c("HTML" = "html_document", "PDF" = "pdf_document")),
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
    
    result <- tryCatch({
      rmarkdown::render(
        "report_template.Rmd",
        output_format = input$format,
        output_file = out_file,
        params = list(ticker = toupper(input$ticker)),
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