#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#

library(shiny)
library(openxlsx)
library(broom)
library(dplyr)
library(knitr)    # For creating HTML tables
library(kableExtra) # For additional table styling (optional)

# Force rsconnect to detect openxlsx by using a dummy function
forceDependencyOpenxlsx <- function() {
  openxlsx::getNamedRegions  # Reference to a function in openxlsx
}

dummy_openxlsx <- openxlsx::getNamedRegions

if (!requireNamespace("openxlsx", quietly = TRUE)) {
  stop("Package 'openxlsx' is required but not installed.")
}


# Source functions
source("functions.R")


# Define UI
ui <- fluidPage(
  titlePanel("Lab 5 / Lab 6 Regeression Analysis"),
  sidebarLayout(
    sidebarPanel(
      downloadButton('downloadTemplate_5', "Download Lab 5 Excel File"),
      downloadButton('downloadTemplate_6', "Download Lab 6 Excel File"),
      br(),
      fileInput('file1', 'Choose Excel File',
                accept = c(".xlsx")),
      checkboxInput('include_constant', 'Include Intercept (uncheck if intercept not significant)', TRUE),
      actionButton("run_analysis", "Run Analysis")
    ),
    mainPanel(
      plotOutput("scatterPlot"),
      uiOutput("modelSummary")  # Changed from tableOutput to uiOutput
    )
  )
)

# Define Server
server <- function(input, output) {
  dataInput <- eventReactive(input$run_analysis, {
    req(input$file1)

    # Read the data without the "New names" message
    df_raw <- openxlsx::read.xlsx(input$file1$datapath, sheet = 1, rows = 3:11, cols = 13:16, colNames = FALSE)

    # Assign column names
    colnames(df_raw) <- c("x", "y", "sigma_x", "sigma_y")

    # Remove rows with any NA values
    df <- na.omit(df_raw)

    # Convert columns to numeric in case they are read as characters
    df <- df %>%
      mutate(
        x = as.numeric(x),
        y = as.numeric(y),
        sigma_x = as.numeric(sigma_x),
        sigma_y = as.numeric(sigma_y)
      )

    # Read the theoretical slope from cell G6 using openxlsx
    theoretical_slope <- openxlsx::read.xlsx(
      input$file1$datapath,
      sheet = 1,
      rows = 6,       # Row 6
      cols = 9,       # Column 7 (G)
      colNames = FALSE
    )
    theoretical_slope <- as.numeric(theoretical_slope)

    # Check if theoretical_slope is numeric and not NA
    if(is.na(theoretical_slope)) {
      showNotification("Theoretical slope in cell G6 is missing or not numeric.", type = "error")
      theoretical_slope <- NULL
    }

    # Return both df and theoretical_slope
    list(df = df, theoretical_slope = theoretical_slope)
  })

  # Update modelResult()
  modelResult <- reactive({
    data <- dataInput()
    df <- data$df
    include_constant <- input$include_constant

    # Perform Iterated WLS Regression
    result <- iterated_wls(df, include_constant)
    return(result)
  })

  # Update output$modelSummary (unchanged)
  output$modelSummary <- renderUI({
    result <- modelResult()
    model <- result$model
    include_constant <- input$include_constant

    # Use broom's tidy function to extract coefficients
    coef_table <- tidy(model, conf.int = TRUE, conf.level = 0.95)

    # Rename columns for clarity
    coef_table <- coef_table %>%
      rename(
        `Estimated Value` = estimate,
        `Standard Error` = std.error,
        `t value` = statistic,
        `P-value` = p.value,
        `Lower 95% CI` = conf.low,
        `Upper 95% CI` = conf.high
      )

    # Select relevant columns
    coef_table <- coef_table %>%
      select(term, `Estimated Value`, `Standard Error`, `Lower 95% CI`, `Upper 95% CI`, `P-value`)

    # If the intercept is not included, remove it from the table
    if(!include_constant) {
      coef_table <- coef_table %>% filter(term != "(Intercept)")
    }

    # Round numeric values for better presentation, excluding 'P-value'
    coef_table <- coef_table %>%
      mutate_at(vars(`Estimated Value`, `Standard Error`, `Lower 95% CI`, `Upper 95% CI`), round, digits = 4) %>%
      mutate(`P-value` = ifelse(`P-value` < 0.001,
                                formatC(`P-value`, format = "e", digits = 2),
                                round(`P-value`, 4)))


    # Rename the term column for better display
    coef_table <- coef_table %>%
      mutate(term = ifelse(term == "(Intercept)", "Intercept", "Slope"))

    # Generate HTML table using knitr::kable
    table_html <- kable(coef_table, format = "html", table.attr = "class='table table-striped'") %>%
      kable_styling(full_width = FALSE)

    # Return the HTML code
    HTML(table_html)
  })
  # Update output$scatterPlot
  output$scatterPlot <- renderPlot({
    data <- dataInput()
    df <- data$df
    theoretical_slope <- data$theoretical_slope
    result <- modelResult()
    include_constant <- input$include_constant

    # Create Plot
    p <- create_plot(df, result$model, theoretical_slope, include_constant)
    print(p)
  })

  output$downloadTemplate_5 <- downloadHandler(
    filename = function() {
      "Lab5_Template.xlsx"
    },
    content = function(file) {
      # Copy the template file from the 'www' directory to the temp file
      file.copy("www/Lab5_Template.xlsx", file)
    },
    contentType = "application/vnd.openxmlformats-officedocument.spreadsheetml.sheet"
  )

  output$downloadTemplate_6 <- downloadHandler(
    filename=function() {
      "Lab6_Template.xlsx"
    },
    content=function(file){
      file.copy("www/Lab6_Template.xlsx", file)
    },
    contentType = "application/vnd.openxmlformats-officedocument.spreadsheetml.sheet"
  )
}

# Run the app
shinyApp(ui = ui, server = server)
