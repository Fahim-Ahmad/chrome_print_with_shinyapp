
library(shiny)
library(shinydashboard)
library(shinybusy)
library(tidyverse)

chrome_extra_args <- function(default_args = c("--disable-gpu")) {
  args <- default_args
  if (identical(Sys.getenv("R_CONFIG_ACTIVE"), "shinyapps")) {
    args <- c(args,
              "--no-sandbox",
              "--disable-dev-shm-usage")
  }
  args
}

ui <- dashboardPage(
  dashboardHeader(disable = T),
  dashboardSidebar(disable = T),
  dashboardBody(
    splitLayout(cellWidths = c("50%", "50%"),
                sliderInput("slider", "Number of observations", min = 100, max = 1000, value = 700, step = 10),
                radioButtons("report_type", "Select report type", choices = c("html", "pdf"))
                ),
    br(),
    conditionalPanel(
      condition = "input.report_type == 'pdf'",
      splitLayout(cellWidths = c("25%", "25%", "50%"),
                  actionButton("buildPDF", "Build pdf report", width = "100%"),
                  uiOutput("downloadBtn"),
                  NULL
                  )
    ),
    conditionalPanel(
      condition = "input.report_type == 'html'",
      downloadButton("html_report", "Download html report")
    ),
    hr(),
    uiOutput("markdown")
  )
)

server <- function(input, output) {
  
  observeEvent(input$buildPDF, {
    output$downloadBtn <- renderUI({
      show_modal_spinner()
      pagedown::chrome_print(
        input = rmarkdown::render("report.Rmd",
                                  params = list(N = input$slider),
                                  envir = new.env(parent = globalenv())
        ), 
        output = tempfile(fileext = ".pdf"),
        extra_args = chrome_extra_args(),
        verbose = 1,
        async = TRUE
      )$then(
        onFulfilled = function(value) {
          showNotification(
            paste("PDF file succesfully generated"),
            type = "message"
          )
          output$downloadPDF <- downloadHandler(
            filename = "report.pdf",
            content = function(file) {
              file.copy(value, file)
            },
            contentType = "application/pdf"
          )
          downloadButton("downloadPDF", "Download pdf report")
        },
        onRejected = function(error) {
          showNotification(
            error$message,
            duration = NULL,
            type = "error"
          )
          HTML("")
        }
      )$finally(remove_modal_spinner)
    })
  })
  
  output$downloadBtn <- renderUI(HTML(""))
  
  output$markdown <- renderUI({
    params <- list(N = input$slider)
    HTML(markdown::markdownToHTML(knitr::knit('report.Rmd', quiet = TRUE)))
  })
  
  output$html_report <- downloadHandler(
    filename = "report.html",
    content = function(file) {
      rmarkdown::render("report.Rmd", output_file = file,
                        params = list(N = input$slider),
                        envir = new.env(parent = globalenv())
      )
    }
  )
  
}

shinyApp(ui, server)

