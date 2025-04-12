# Allow large file uploads (up to 100MB)
options(shiny.maxRequestSize = 100 * 1024^2)

library(shiny)
library(dplyr)
library(ggplot2)
library(lubridate)
library(gridExtra)

ui <- fluidPage(
  titlePanel("IMPACT OF WEATHER ON TRAFFIC ACCIDENTS"),
  sidebarLayout(
    sidebarPanel(
      fileInput("accident_file", "Upload Accident Data CSV", accept = ".csv"),
      fileInput("weather_file", "Upload Weather Data CSV", accept = ".csv"),  # Still allows weather file upload
      actionButton("run_btn", "Run Analysis", icon = icon("play")),
      selectInput("season", "Select Season:", choices = NULL),
      selectInput("time_of_day", "Select Time of Day:", choices = NULL),
      width = 3
    ),
    mainPanel(
      tabsetPanel(
        tabPanel("Summary", verbatimTextOutput("summary_out")),
        tabPanel("Time Visualizations", plotOutput("plot1"), plotOutput("plot2")),
        tabPanel("Seasonal Accidents", plotOutput("accident_season_plot")),
        tabPanel("Time of Day Accidents", plotOutput("accident_tod_plot"))
      )
    )
  )
)

server <- function(input, output, session) {
  observeEvent(input$run_btn, {
    req(input$accident_file)
    
    # Load accident data
    accidents_data <- read.csv(input$accident_file$datapath, stringsAsFactors = FALSE)
    
    # Load weather data (but won't use it in the analysis)
    if (!is.null(input$weather_file)) {
      weather_data <- read.csv(input$weather_file$datapath, stringsAsFactors = FALSE)
    }
    
    # Date/time processing function
    process_datetime <- function(data, start_col, end_col) {
      data$start_time <- as.POSIXct(data[[start_col]], format="%m/%d/%Y %H:%M", tz="UTC")
      data$end_time <- as.POSIXct(data[[end_col]], format="%m/%d/%Y %H:%M", tz="UTC")
      data$date <- as.Date(data$start_time)
      data$hour <- hour(data$start_time)
      data$day_of_week <- wday(data$start_time, label=TRUE)
      data$month_num <- month(data$start_time)
      data$month <- month(data$start_time, label=TRUE)
      data$year <- year(data$start_time)
      data$season <- case_when(
        data$month_num %in% c(12, 1, 2) ~ "Winter",
        data$month_num %in% c(3, 4, 5) ~ "Spring",
        data$month_num %in% c(6, 7, 8) ~ "Summer",
        data$month_num %in% c(9, 10, 11) ~ "Fall",
        TRUE ~ NA_character_
      )
      data$time_of_day <- case_when(
        data$hour >= 5 & data$hour < 12 ~ "Morning",
        data$hour >= 12 & data$hour < 17 ~ "Afternoon",
        data$hour >= 17 & data$hour < 21 ~ "Evening",
        TRUE ~ "Night"
      )
      return(data)
    }
    
    # Process the data
    accidents_data <- process_datetime(accidents_data, "StartTime.UTC.", "EndTime.UTC.")
    
    # Update input choices dynamically based on the dataset
    updateSelectInput(session, "season", choices = unique(accidents_data$season), selected = "Winter")
    updateSelectInput(session, "time_of_day", choices = unique(accidents_data$time_of_day), selected = "Morning")
    
    # Summary output
    output$summary_out <- renderPrint({
      paste0("✅ Accident Records: ", nrow(accidents_data),
             "\n📅 Date Range: ", min(accidents_data$start_time, na.rm = TRUE),
             " to ", max(accidents_data$start_time, na.rm = TRUE))
    })
    
    # Visualization: Hour of Day & Day of Week
    output$plot1 <- renderPlot({
      p1 <- ggplot(accidents_data, aes(x = hour)) +
        geom_histogram(binwidth = 1, fill = "steelblue", color = "black") +
        labs(title = "Accidents by Hour of Day", x = "Hour", y = "Count") +
        theme_minimal()
      p2 <- ggplot(accidents_data, aes(x = day_of_week)) +
        geom_bar(fill = "darkred", color = "black") +
        labs(title = "Accidents by Day of Week", x = "Day", y = "Count") +
        theme_minimal()
      grid.arrange(p1, p2, ncol = 2)
    })
    
    # Time-of-day visual
    output$plot2 <- renderPlot({
      tod <- accidents_data %>% 
        group_by(time_of_day) %>% 
        summarize(count = n(), .groups = "drop")
      ggplot(tod, aes(x = time_of_day, y = count, fill = time_of_day)) +
        geom_bar(stat = "identity", color = "black") +
        labs(title = "Accidents by Time of Day", x = "Time of Day", y = "Count") +
        theme_minimal() + theme(legend.position = "none")
    })
    
    # Seasonal Accidents Plot
    output$accident_season_plot <- renderPlot({
      seasonal_data <- accidents_data %>%
        filter(season == input$season) %>%
        count(day_of_week)
      ggplot(seasonal_data, aes(x = day_of_week, y = n)) +
        geom_bar(stat = "identity", fill = "steelblue") +
        labs(title = paste("Accidents by Day of Week in", input$season), x = "Day", y = "Count")
    })
    
    # Time of Day Accidents Plot
    output$accident_tod_plot <- renderPlot({
      tod_data <- accidents_data %>%
        filter(time_of_day == input$time_of_day) %>%
        count(hour)
      ggplot(tod_data, aes(x = hour, y = n)) +
        geom_bar(stat = "identity", fill = "darkred") +
        labs(title = paste("Accidents by Hour during", input$time_of_day), x = "Hour", y = "Count")
    })
  })
}

shinyApp(ui = ui, server = server)
