library(shiny)
library(fmsb)
library(shinyWidgets)
library(readr)
library(ggplot2)
library(dplyr)
library(DT)
library(httr) # For ChatGPT API calls

# Read in the datasets
all_stats_df <- read_csv('../../data/Player_Stats.csv')
df <- read_csv('../../data/player_percentiles.csv')

# Corrected function to find the top 3 closest players based on input stats
find_closest_players <- function(input_stats, df) {
  top_players <- df %>%
    mutate(
      distance = sqrt((scoring - input_stats$scoring)^2 +
                        (shooting - input_stats$shooting)^2 +
                        (passing - input_stats$passing)^2 +
                        (defense - input_stats$defense)^2 +
                        (rebounding - input_stats$rebounding)^2)
    ) %>%
    arrange(distance) %>%
    slice(1:3) %>%
    mutate(distance = round(distance, 2)) %>%
    select(Player_Name, scoring, shooting, passing, defense, rebounding, distance)
  
  return(top_players)
}

# Function to calculate polygon area
polygon_area <- function(values) {
  n <- length(values)
  area <- 0
  for (i in 1:n) {
    j <- ifelse(i == n, 1, i + 1)
    area <- area + values[i] * values[j]
  }
  return(abs(area) / 2)
}


ui <- fluidPage(
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
                 h4("Top 3 Closest Players:"),
                 DTOutput("closestPlayersTable"),
                 h4("Overlap Between User and Top Player:"),
                 textOutput("overlapPercentage")
               ),
               mainPanel(
                 plotOutput("radarPlot", width = "800px", height = "800px")
               )
             )
    ),
    tabPanel("Player Description",
             fluidPage(
               h3("Generated Description of the Most Similar Player"),
               verbatimTextOutput("playerBlurb")
             )
    )
  )
)

server <- function(input, output) {
  
  # Collect user inputs from sliders
  user_inputs <- reactive({
    list(
      scoring = input$scoring,
      shooting = input$shooting,
      passing = input$passing,
      defense = input$defense,
      rebounding = input$rebounding
    )
  })
  
  # Find top 3 closest players
  top_closest_players <- reactive({
    input_stats <- user_inputs()
    find_closest_players(input_stats, df)
  })
  
  # Radar plot for Visual Comparison tab
  output$radarPlot <- renderPlot({
    input_stats <- user_inputs()
    closest_players <- top_closest_players()
    top_player <- closest_players[1, ]
    
    data <- data.frame(
      Shooting = c(100, 0, input_stats$shooting, top_player$shooting),
      Passing = c(100, 0, input_stats$passing, top_player$passing),
      Defense = c(100, 0, input_stats$defense, top_player$defense),
      Rebounding = c(100, 0, input_stats$rebounding, top_player$rebounding),
      Scoring = c(100, 0, input_stats$scoring, top_player$scoring)
    )
    
    radarchart(
      data, 
      axistype = 1, 
      pcol = c("blue", "red"), 
      pfcol = c(rgb(0.2, 0.5, 0.7, 0.5), rgb(0.8, 0.3, 0.3, 0.5)), 
      plwd = 2, 
      plty = c(1, 1), 
      cglcol = "grey", 
      cglty = 1, 
      axislabcol = "black", 
      caxislabels = seq(0, 100, 25), 
      cglwd = 0.8
    )
  })
  
  # Calculate and display overlap percentage
  output$overlapPercentage <- renderText({
    input_stats <- user_inputs()
    closest_players <- top_closest_players()
    top_player <- closest_players[1, ]
    
    user_values <- c(input_stats$shooting, input_stats$passing, input_stats$defense, input_stats$rebounding, input_stats$scoring)
    player_values <- c(top_player$shooting, top_player$passing, top_player$defense, top_player$rebounding, top_player$scoring)
    
    user_area <- polygon_area(user_values)
    player_area <- polygon_area(player_values)
    overlap_values <- pmin(user_values, player_values)
    overlap_area <- polygon_area(overlap_values)
    overlap_percentage <- (overlap_area / ((user_area + player_area) / 2)) * 100
    overlap_percentage <- min(overlap_percentage, 100)
    
    paste("Overlap: ", round(overlap_percentage, 2), "%")
  })
  
  # Display closest players in a table
  output$closestPlayersTable <- renderDT({
    closest_players <- top_closest_players()
    datatable(
      closest_players, 
      options = list(pageLength = 3, dom = 't', autoWidth = TRUE),
      rownames = FALSE,
      colnames = c("Player Name", "Points Per Game", "Shooting", 
                   "Passing", "Defense", "Rebounding", "Distance")
    )
  })
  
  # Generate blurb for the top player using ChatGPT
  output$playerBlurb <- renderText({
    closest_players <- top_closest_players()
    top_player <- closest_players[1, ]
    
    stats <- list(
      shooting = top_player$shooting,
      passing = top_player$passing,
      defense = top_player$defense,
      rebounding = top_player$rebounding,
      scoring = top_player$scoring
    )
    
    generate_player_blurb(top_player$Player_Name, stats)
  })
}

shinyApp(ui = ui, server = server)