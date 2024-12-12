library(shiny)
library(fmsb)
library(shinyWidgets)
library(readr)
library(ggplot2)
library(dplyr)
library(DT)
library(httr) # For ChatGPT API calls
library(plotly)

# Read in the datasets


all_stats_df <- read_csv('../../data/Player_Stats.csv')
df <- read_csv('../../data/player_percentiles.csv')

print(head(df))
str(df)

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
    ),
    tabPanel("Hypothetical Player", # New Tab
             sidebarLayout(
               sidebarPanel(
                 h3("Enter Percentiles"),
                 sliderInput("percentile_shooting", "Shooting Percentile", min = 0, max = 100, value = 50),
                 sliderInput("percentile_passing", "Passing Percentile", min = 0, max = 100, value = 50),
                 sliderInput("percentile_defense", "Defense Percentile", min = 0, max = 100, value = 50),
                 sliderInput("percentile_rebounding", "Rebounding Percentile", min = 0, max = 100, value = 50),
                 sliderInput("percentile_scoring", "Scoring Percentile", min = 0, max = 100, value = 50)
               ),
               mainPanel(
                 h4("Hypothetical Player Stats"),
                 tableOutput("hypotheticalStats"),
                 h4("Dissimilarity Matrix Placement"),
                 plotOutput("hypotheticalPlacementPlot")
               )
             )
    )
  )
)




server <- function(input, output) {
    
    ### HELPER FUNCTIONS ###
    
    # Function to reverse percentile to raw stats
    reverse_percentile <- function(percentile, min_stat, max_stat) {
      stat <- (percentile / 100) * (max_stat - min_stat) + min_stat
      return(stat)
    }
    
    # Function to reverse percentiles into stats
    reverse_stats <- function(percentiles, ranges) {
      data.frame(
        PPG = reverse_percentile(percentiles$scoring, ranges$PPG_min, ranges$PPG_max),
        Paint_PPG = reverse_percentile(percentiles$scoring, ranges$Paint_PPG_min, ranges$Paint_PPG_max),
        made_3PT_pg = reverse_percentile(percentiles$shooting, ranges$made_3PT_pg_min, ranges$made_3PT_pg_max),
        APG = reverse_percentile(percentiles$passing, ranges$APG_min, ranges$APG_max),
        ATO = reverse_percentile(percentiles$passing, ranges$ATO_min, ranges$ATO_max),
        SPG = reverse_percentile(percentiles$defense, ranges$SPG_min, ranges$SPG_max),
        BPG = reverse_percentile(percentiles$defense, ranges$BPG_min, ranges$BPG_max),
        DefRBD = reverse_percentile(percentiles$rebounding, ranges$DefRBD_min, ranges$DefRBD_max),
        OffRBD = reverse_percentile(percentiles$rebounding, ranges$OffRBD_min, ranges$OffRBD_max)
      )
    }
    
    # Helper function to compute polygon area for radar chart
    polygon_area <- function(values) {
      n <- length(values)
      area <- 0
      for (i in 1:n) {
        j <- ifelse(i == n, 1, i + 1)
        area <- area + values[i] * values[j]
      }
      return(abs(area) / 2)
    }
    
    ### DATA PREPARATION ###
    
    # Precompute the min/max values for each metric
    stat_ranges <- list(
      PPG_min = min(all_stats_df$PPG, na.rm = TRUE),
      PPG_max = max(all_stats_df$PPG, na.rm = TRUE),
      Paint_PPG_min = min(all_stats_df$Paint_PPG, na.rm = TRUE),
      Paint_PPG_max = max(all_stats_df$Paint_PPG, na.rm = TRUE),
      made_3PT_pg_min = min(all_stats_df$made_3PT_pg, na.rm = TRUE),
      made_3PT_pg_max = max(all_stats_df$made_3PT_pg, na.rm = TRUE),
      APG_min = min(all_stats_df$APG, na.rm = TRUE),
      APG_max = max(all_stats_df$APG, na.rm = TRUE),
      ATO_min = min(all_stats_df$ATO, na.rm = TRUE),
      ATO_max = max(all_stats_df$ATO, na.rm = TRUE),
      SPG_min = min(all_stats_df$SPG, na.rm = TRUE),
      SPG_max = max(all_stats_df$SPG, na.rm = TRUE),
      BPG_min = min(all_stats_df$BPG, na.rm = TRUE),
      BPG_max = max(all_stats_df$BPG, na.rm = TRUE),
      DefRBD_min = min(all_stats_df$DefRBD, na.rm = TRUE),
      DefRBD_max = max(all_stats_df$DefRBD, na.rm = TRUE),
      OffRBD_min = min(all_stats_df$OffRBD, na.rm = TRUE),
      OffRBD_max = max(all_stats_df$OffRBD, na.rm = TRUE)
    )
    
    ### TAB 1: VISUAL COMPARISON ###
    
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
    
    # Radar plot for "Visual Comparison"
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
    
    # Display closest players table
    output$closestPlayersTable <- renderDT({
      closest_players <- top_closest_players()
      datatable(
        closest_players, 
        options = list(pageLength = 3, dom = 't', autoWidth = TRUE),
        rownames = FALSE,
        colnames = c("Player Name", "Scoring", "Shooting", 
                     "Passing", "Defense", "Rebounding", "Distance")
      )
    })
    
    # Display overlap percentage
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
    
    ### TAB 2: PLAYER DESCRIPTION ###
    
    output$playerBlurb <- renderText({
      closest_players <- top_closest_players()
      top_player <- closest_players[1, ]
      
      stats <- list(
        scoring = top_player$scoring,
        shooting = top_player$shooting,
        passing = top_player$passing,
        defense = top_player$defense,
        rebounding = top_player$rebounding
      )
      
      paste0(
        "Player: ", top_player$Player_Name, "\n",
        "Scoring: ", stats$scoring, "\n",
        "Shooting: ", stats$shooting, "\n",
        "Passing: ", stats$passing, "\n",
        "Defense: ", stats$defense, "\n",
        "Rebounding: ", stats$rebounding
      )
    })
    
    ### TAB 3: HYPOTHETICAL PLAYER ###
    
    # Calculate hypothetical player stats from percentiles
    hypothetical_player <- reactive({
      percentiles <- list(
        scoring = input$percentile_scoring,
        shooting = input$percentile_shooting,
        passing = input$percentile_passing,
        defense = input$percentile_defense,
        rebounding = input$percentile_rebounding
      )
      reverse_stats(percentiles, stat_ranges)
    })
    
    

    # Display hypothetical player stats
    output$hypotheticalStats <- renderTable({
      hypothetical_player()
    })
    
    # Reactive function to compute new MDS data with the hypothetical player
    new_player_mds_data <- eventReactive(input$update_plot, {
      new_stats <- hypothetical_player()
      
      # Add position encoding
      new_position <- factor(input$new_player_position, levels = levels(Player_data$Position))
      position_encoded <- dummyVars(~ Position, data = Player_data)
      position_matrix <- predict(position_encoded, newdata = data.frame(Position = new_position))
      position_matrix <- as.data.frame(position_matrix)
      
      # Combine stats and position encoding
      new_player_point <- cbind(as.data.frame(t(new_stats)), position_matrix)
      
      # Add missing columns for consistency
      missing_columns <- setdiff(names(Player_data_without_names), names(new_player_point))
      for (col in missing_columns) {
        new_player_point[[col]] <- 0
      }
      new_player_point <- new_player_point[, names(Player_data_without_names)]
      
      # Compute Gower dissimilarity and project the new player
      new_point_dissimilarity <- daisy(rbind(Player_data_without_names, new_player_point), metric = "gower")
      new_mds_result <- cmdscale(as.dist(new_point_dissimilarity), k = 2)
      new_player_mds <- as.data.frame(new_mds_result[nrow(new_mds_result), , drop = FALSE])
      colnames(new_player_mds) <- c("MDS1", "MDS2")
      new_player_mds$Player_Name <- "New Player"
      new_player_mds$cluster_name <- "New Player"
      
      # Combine with existing MDS data
      rbind(mds_data, new_player_mds)
    })
    
    # Render the interactive MDS plot
    output$interactiveMDSPlot <- renderPlotly({
      combined_mds_data <- new_player_mds_data()
      
      
      print(head(combined_mds_data))
      print(str(combined_mds_data))
      
      
      plot_ly(
        data = combined_mds_data,
        x = ~MDS1,
        y = ~MDS2,
        color = ~cluster_name,
        colors = c("blue", "purple", "black", "orange", "red", "green", "pink"),
        text = ~paste("Player Name:", Player_Name, "<br>Cluster:", cluster_name),
        type = "scatter",
        mode = "markers",
        marker = list(size = 8, opacity = 0.8)
      ) %>%
        layout(
          title = "Interactive MDS Plot with New Player",
          xaxis = list(title = "MDS Dimension 1"),
          yaxis = list(title = "MDS Dimension 2"),
          legend = list(title = list(text = "Cluster Names"))
        )
    })
  }


shinyApp(ui = ui, server = server)