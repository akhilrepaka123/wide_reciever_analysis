library(shiny)
library(ggplot2)
library(dplyr)
library(rsconnect)
library(shinyWidgets)





football_stats <- read.csv("Football Stats (SDS Project 2) - Sheet1.csv")

#UI
ui <- fluidPage(
  titlePanel("Football Wide Reciever Analysis"),
  
  sidebarLayout(
    sidebarPanel(
      h3("Variables:"),
      p("Use the widgets below to explore the dataset. Choose variables, filters, and graph options!"),
      h4("Variable Explanations:"),
      p("TOUCHDOWNS: The amount of touchdowns a reciever caught during the season."),
      p("RECEPTIONS: The amount of receptions (completed catches) a reciever caught during the season."),
      p("YAC.R: (Yards After Carry) The average amount of yards ran after catching the ball."),
      p("TOTAL_YARDS: The total amount of yards a reciever has."),
      
      # Select variables
      selectInput("variables", "Select variables to Analyze (Please select only up to two!):",
                  choices = c("TOUCHDOWNS", "RECEPTIONS", "YAC.R", "TOTAL_YARDS"),
                  multiple = TRUE),
      
      # Add filtering options
      sliderInput("yards_filter", "Filter by Total Yards:", #can filter the dataset by the amount of total yards a reciever had in the season
                  min = min(football_stats$TOTAL_YARDS, na.rm = TRUE),
                  max = max(football_stats$TOTAL_YARDS, na.rm = TRUE),
                  value = c(min(football_stats$TOTAL_YARDS, na.rm = TRUE),
                            max(football_stats$TOTAL_YARDS, na.rm = TRUE))),
      
      
      dropdown( #created a dropdown so it doesn't take up to much space
        checkboxGroupInput("team_filter", "Select Teams:", #can filter the dataset by what teams the user wants to analyze
                           choices = sort(unique(football_stats$TEAM)),
                           selected = sort(unique(football_stats$TEAM))),
        label = "Filter by Team",
        icon = icon("sliders"), 
        status = "primary",     
        circle = TRUE           
      ),
      
      # Choice Element: Color Choice
      selectInput("color_choice", "Select Graph Color:",
                  choices = c("Blue", "Green", "Gold"),
                  selected = "Blue"),
      
      
      # Choice Element: A checkbox that updates some feature of the output if selected.
      checkboxInput("show_median", "Show Median Line for Univariate Graphs", value = FALSE)
    ),
    
    mainPanel(
      h3("Descriptive Statistics"),
      p("Shows Median, Mean, Minimum, Maximum, First Quartile, and Third Quartile"),#shows all descriptive statistics
      verbatimTextOutput("summary_output"),
      
      h3("Graph"), #shows the corresponding graphs
      plotOutput("plot_output"),
      
      h5("Correlation Coefficient"),
      verbatimTextOutput("cc")
      
    )
  )
)

# Server Logic
server <- function(input, output) {
  
  observeEvent(input$variables, { #shows error warning if more than two variables are chosen
    if (length(input$variables) > 2) {
      showNotification("You can select a maximum of 2 variables.", type = "error") #doesn't kick the user out, but just shows them a notification
    }
  })
  
  #Filtering the data with the input from the filter yards and team buttons
  filtered_data <- reactive({
    football_stats %>%
      filter(
        TOTAL_YARDS >= input$yards_filter[1],
        TOTAL_YARDS <= input$yards_filter[2], #only analyzes in the recievers within this yardage range
        TEAM %in% input$team_filter #ony analyzes teams that are chosen by the filter
      )
  })
  
  # Output summary statistics
  output$summary_output <- renderPrint({
    req(input$variables) #requres at least one variable
    data <- filtered_data()[, input$variables, drop = FALSE]
    (summary(data))
    #gives all the descriptive statistics for the new filtered data
  })
  
  # Outputting plot
  output$plot_output <- renderPlot({
    req(input$variables)  
    data <- filtered_data()
    
    # Choosing plot color
    color <- switch(input$color_choice, Blue = "blue", Green = "green", Gold = "gold")
    
    # Generate plot based on selected variables
    if (length(input$variables) == 1) { #for univariate plots
      ggplot(data, aes_string(x = input$variables[1])) +
        geom_histogram(fill = color, col="black", bins = 50) +
        labs(title = paste("Univariate Analysis of", input$variables[1]),
             x = input$variables[1], y = "Count") +
        theme_classic() +
        if (input$show_median) geom_vline(aes_string(xintercept = median(data[[input$variables[1]]], na.rm = TRUE)),
                                          color = "black", linetype = "dashed")
    } 
    
    
    
    else if (length(input$variables) >= 2) { #for multivariate plots
      output$cc <- renderPrint({
        req(input$variables)
        validate(
          need(length(input$variables) == 2, "Please select a total of two variables.")
        )
        cor_coeff <- cor(data[[input$variables[1]]], data[[input$variables[2]]], use = "complete.obs")
        cor_coeff
        
      })

    
        ggplot(data, aes_string(x = input$variables[1], y = input$variables[2])) +
        geom_point(color = color) +
        labs(title = paste("Multivariate Analysis of", input$variables[1], "and", input$variables[2]),
             x = input$variables[1], y = input$variables[2]) +
        theme_classic()
    }
  })
}

shinyApp(ui = ui, server = server) #running the app

