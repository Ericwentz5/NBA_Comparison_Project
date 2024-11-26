library(shiny)
library(fmsb)
library(shinyWidgets)
library(readr)
library(ggplot2)

# Read in the dataset
all_stats_df <- read_csv('../../data/Player_Stats.csv')

ui <- fluidPage(
  tags$head(
    tags$style(HTML("
      .nav-pills > li.active > a, 
      .nav-pills > li.active > a:focus, 
      .nav-pills > li.active > a:hover {
        background-color: blue;
        color: white;
      }
    "))
  ),
  titlePanel("Basketball Skills Analysis"),
  tabsetPanel(
    type = "pills",
    tabPanel("Visual Comparison",
             sidebarLayout(
               sidebarPanel(
                 h3("Your Attributes"),
                 sliderInput("shooting", "Shooting", min = 0, max = 100, value = 50),
                 sliderInput("passing", "Passing", min = 0, max = 100, value = 50),
                 sliderInput("defense", "Defense", min = 0, max = 100, value = 50),
                 sliderInput("rebounding", "Rebounding", min = 0, max = 100, value = 50),
                 sliderInput("scoring", "Scoring", min = 0, max = 100, value = 50),
                 h3("NBA Comparison Stats"),
                 sliderInput("shooting2", "Shooting 2", min = 0, max = 100, value = 50),
                 sliderInput("passing2", "Passing 2", min = 0, max = 100, value = 50),
                 sliderInput("defense2", "Defense 2", min = 0, max = 100, value = 50),
                 sliderInput("rebounding2", "Rebounding 2", min = 0, max = 100, value = 50),
                 sliderInput("scoring2", "Scoring 2", min = 0, max = 100, value = 50)
               ),
               mainPanel(
                 plotOutput("radarPlot", width = "800px", height = "800px"),
                 h4("Player Similarity Percentage:"),
                 textOutput("overlapArea")
               )
             )
    ),
    tabPanel("The Model",
             fluidPage(
               h3("Your Attribute Graph"),
               plotOutput("userGraph", width = "600px", height = "400px"),
               h3("Player Stats: Points Per Game"),
               plotOutput("dataGraph", width = "600px", height = "400px")
             )
    ),
    tabPanel("Fun GIF",
             fluidPage(
               h3("That moment when our group is the best!"),
               tags$iframe(
                 src = "https://giphy.com/embed/t76hozkYg6dQuCtVjN",
                 width = "480",
                 height = "271",
                 style = "border: none;"
               )
             )
    )
  )
)

server <- function(input, output) {
  
  # Collect user inputs from sliders
  user_inputs <- reactive({
    c(
      Shooting = input$shooting,
      Passing = input$passing,
      Defense = input$defense,
      Rebounding = input$rebounding,
      Scoring = input$scoring
    )
  })
  
  # Radar plot for Visual Comparison tab
  output$radarPlot <- renderPlot({
    data <- data.frame(
      Shooting = c(100, 0, input$shooting, input$shooting2),
      Passing = c(100, 0, input$passing, input$passing2),
      Defense = c(100, 0, input$defense, input$defense2),
      Rebounding = c(100, 0, input$rebounding, input$rebounding2),
      Scoring = c(100, 0, input$scoring, input$scoring2)
    )
    
    radarchart(data, 
               axistype = 1, 
               pcol = c("blue", "red"), 
               pfcol = c(rgb(0.2, 0.5, 0.7, 0.5), rgb(0.8, 0.3, 0.3, 0.5)), 
               plwd = 2, 
               plty = c(1, 1), 
               lty = 1, 
               cglcol = "grey", 
               cglty = 1, 
               axislabcol = "black", 
               caxislabels = seq(0, 100, 25), 
               cglwd = 0.8)
  })
  
  # Overlap area calculation for Visual Comparison tab
  output$overlapArea <- renderText({
    user_values <- c(input$shooting, input$passing, input$defense, input$rebounding, input$scoring)
    nba_values <- c(input$shooting2, input$passing2, input$defense2, input$rebounding2, input$scoring2)
    user_area <- polygon_area(user_values)
    nba_area <- polygon_area(nba_values)
    overlap_values <- pmin(user_values, nba_values)
    overlap_area <- polygon_area(overlap_values)
    average_area <- (user_area + nba_area) / 2
    overlap_percentage <- (overlap_area / average_area) * 100
    overlap_percentage <- min(overlap_percentage, 100)
    paste("", round(overlap_percentage, 2), "%")
  })
  
  # Simple bar graph for The Model tab
  output$userGraph <- renderPlot({
    bar_values <- user_inputs()
    bar_names <- names(bar_values)
    barplot(
      bar_values,
      names.arg = bar_names,
      col = "blue",
      main = "Your Attributes",
      xlab = "Attributes",
      ylab = "Value",
      ylim = c(0, 100)
    )
  })
  
  # Bar graph for player stats (Points Per Game)
  output$dataGraph <- renderPlot({
    ggplot(all_stats_df, aes(x = Player_Name, y = PPG)) +
      geom_bar(stat = "identity", fill = "darkgreen") +
      theme_minimal() +
      labs(
        title = "Player Points Per Game (PPG)",
        x = "Player",
        y = "Points Per Game"
      ) +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
  })
}

shinyApp(ui = ui, server = server)