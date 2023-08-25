library(shiny)
library(tidyverse)
library(data.table)
library(foreach)
library(usmap)
library(ggplot2)
library(plotly)
library(rjson)

data <- fromJSON(file="https://raw.githubusercontent.com/plotly/datasets/master/geojson-counties-fips.json")
data$features[[1]]


# Define UI for application
ui <- fluidPage(

    # Application title
    titlePanel("Adaptive Metrics for COVID-19 Risk Designation"),

    # Sidebar with for tabs for user input
    sidebarLayout(
        sidebarPanel(
          
          # numeric input for weight chosen
          numericInput("weightInput",
                        h4("Weight (Cost of False Negatives to False Positives):"),
                        value = 0),
          
          # checkbox for indicators selected
          checkboxGroupInput("indicators", 
                             h4("Indicators Selected"), 
                             choices = list("New COVID-19 Cases" = 1, 
                                            "New COVID-19 Hospitalizations" = 2, 
                                            "Percentage of Inpatient Bed Occupancy by COVID-19 Patients" = 3)),
          
          # dropdown tab to select desired outcome to be predicted
          selectInput("outcomeSelected", h4("Select Outcome"), 
                      choices = list("Deaths (/100k/week)" = 1, "ICU Patients (/100k/week)" = 2,
                                     "Hospital Occupancy (%)" = 3)),
          
          # change to slider later based on choice selected
          numericInput("outcomeThreshold", h4("Outcome Cutoff"), value = 0),
          
          br(),
              
          # input for date range of training data
          dateRangeInput("trainingDates", h4("Training Date Range")),
              
          # 
          fileInput("dataFile", h4("Import Data"))
        ),

        # Laying out output
        mainPanel(
           h3("Training Points"),
           plotOutput("trainingPlot"),
           tableOutput("trainingStatistics"),
           h3("Output"),
           dataTableOutput("modelOutput"),
           plotOutput("heatmap", click = "plot_click")
        )
    )
  )


server <- function(input, output) {

  # feed in input file
  dataSubset <- reactive({
    req(input$dataFile)
    data <- read.csv(input$dataFile$datapath)
    
    data$mult <- input$weightInput
    
    # access outcome selected to compare outcome threshold
    outcome_col <- switch(input$outcomeSelected,
                          "1" = "deaths_avg_per_100k",
                          "2" = "admits_weekly",
                          "3" = "perc_covid_100")
    
    # create outcome_label column
    data$outcome_label <- switch(input$outcomeSelected,
                                       "1" = "deaths_over",
                                       "2" = "hospitalizations_over",
                                       "3" = "hospital_occupancy_over"
    )
    
    # compare outcome and add threshold column
    outcome_selected <- data[[outcome_col]]
    data$outcome_over <- outcome_selected > input$outcomeThreshold
    
    # Rename the outcome_over column based on the selected indicator
    indicator_label <- data$outcome_label
    colnames(data)[colnames(data) == "outcome_over"] <- indicator_label
    
    return(data)
  })
  
  
  # running model on input file
  observeEvent(c(input$indicators, input$trainingDates, input$dataFile, input$weightInput, input$outcomeSelected, input$outcomeThreshold), {
    currentDataSubset <- dataSubset()
    
    # create formula for logistic regression based on indicators chosen and outcomeSelected
    indicator_column_map <- c("1" = "cases_weekly",
                              "2" = "admits_weekly",
                              "3" = "perc_covid_100")
    
    # Get selected indicators
    selected_indicators <- as.character(input$indicators)

    # Create formula string
    formula_string <- paste(currentDataSubset$outcome_label[1], "~", paste(indicator_column_map[selected_indicators], collapse = " + "))

    # Convert formula string to formula object
    model_formula <- as.formula(formula_string)

    # prepare dates for entry
    trainingStart <- as.Date(input$trainingDates[1])
    trainingEnd <- as.Date(input$trainingDates[2])
    
    # convert dataSubset to data table
    dataFile <- as.data.table(currentDataSubset)
    
    # run model
    result <- best_measure_DT_reg(d_test_ind = dataFile, train_dates = trainingStart, 
                                  end_dates = trainingEnd, form = model_formula)

    # display the output df
    output$modelOutput <- renderDataTable(result)
    
    # plot reactive heatmap
    heatmap <- plot_usmap(include = dataSubset()$location) +
      labs(title = "Heat Map of Locations")
    output$heatmap <-renderPlot(heatmap)
    
    # Create a reactive hover event to display information
    observeEvent(input$heatmap_hover, {
      hovered_location <- input$heatmap_hover
      if (!is.null(hovered_location)) {
        hovered_data <- subset(dataSubset(), location == hovered_location)
        showModal(modalDialog(
          title = hovered_location,
          paste("Value:", hovered_data$value)
        ))
      }
    })
    
  }, ignoreInit = TRUE, ignoreNULL = TRUE)
  
  
  # display training points in scatterplot
  observeEvent(c(input$indicators, input$trainingDates, input$dataFile), {
    req(input$indicators, input$trainingDates, input$dataFile)
    
    currentDataSubset <- dataSubset()
    
    # Subsetting data based on training dates
    trainingData <- currentDataSubset[currentDataSubset$ymd >= input$trainingDates[1] & currentDataSubset$ymd <= input$trainingDates[2], ]
    metrics_selected <- as.numeric(input$indicators)

    # Map between indicators selected and columns in data frame
    indicator_column_map <- c("1" = "cases_weekly",
                              "2" = "admits_weekly",
                              "3" = "perc_covid_100")
    
    # Access outcome label
    comp_name <- dataSubset()$outcome_label[1]

    # Create plots for all indicators chosen
    plots <- lapply(metrics_selected, function(metric) {
      column_name <- indicator_column_map[as.character(metric)]
      ggplot(trainingData, aes(x = ymd, y = .data[[column_name]], color = .data[[comp_name]])) +
        geom_point() +
        labs(title = paste("Scatter Plot of", column_name),
             x = "Date", y = column_name) +
        scale_color_manual(values = c("TRUE" = "red", "FALSE" = "blue"))
    })
    
    # Return the list of plots as a grid
    output$trainingPlot <- renderPlot({
      gridExtra::grid.arrange(grobs = plots, ncol = 2)
    })
  }, ignoreInit = TRUE, ignoreNULL = TRUE)

}

# Run the application 
shinyApp(ui = ui, server = server)
