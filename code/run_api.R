library(httr)
library(jsonlite)
library(dplyr)


api_key <- "HSSl560bJq1tcEhYaXzoK4eH3yvmvLhY5kM9WIh7" 
season_year <- 2023
season_type <- "REG"


nba_teams <- data.frame(
  team_id = c(
    "583eca2f-fb46-11e1-82cb-f4ce4684ea4c", # Timberwolves
    "583ec773-fb46-11e1-82cb-f4ce4684ea4c", # Cavaliers
    "583ecae2-fb46-11e1-82cb-f4ce4684ea4c", # Lakers
    "583ec97e-fb46-11e1-82cb-f4ce4684ea4c", # Hornets
    "583ecdfb-fb46-11e1-82cb-f4ce4684ea4c", # Clippers
    "583eccfa-fb46-11e1-82cb-f4ce4684ea4c", # Celtics
    "583ec8d4-fb46-11e1-82cb-f4ce4684ea4c", # Wizards
    "583ecf50-fb46-11e1-82cb-f4ce4684ea4c", # Mavericks
    "583ed102-fb46-11e1-82cb-f4ce4684ea4c", # Nuggets
    "583ecb8f-fb46-11e1-82cb-f4ce4684ea4c", # Hawks
    "583ecefd-fb46-11e1-82cb-f4ce4684ea4c", # Bucks
    "583ec5fd-fb46-11e1-82cb-f4ce4684ea4c", # Bulls
    "583eca88-fb46-11e1-82cb-f4ce4684ea4c", # Grizzlies
    "583ecea6-fb46-11e1-82cb-f4ce4684ea4c", # Heat
    "583ecc9a-fb46-11e1-82cb-f4ce4684ea4c", # Pelicans
    "583ece50-fb46-11e1-82cb-f4ce4684ea4c", # Jazz
    "583ec7cd-fb46-11e1-82cb-f4ce4684ea4c", # Pacers
    "583ecda6-fb46-11e1-82cb-f4ce4684ea4c", # Raptors
    "583ed157-fb46-11e1-82cb-f4ce4684ea4c", # Magic
    "583ecd4f-fb46-11e1-82cb-f4ce4684ea4c", # Spurs
    "583ed0ac-fb46-11e1-82cb-f4ce4684ea4c", # Kings
    "583ecfa8-fb46-11e1-82cb-f4ce4684ea4c", # Suns
    "583ecfff-fb46-11e1-82cb-f4ce4684ea4c", # Thunder
    "583ed056-fb46-11e1-82cb-f4ce4684ea4c", # Trail Blazers
    "583ec825-fb46-11e1-82cb-f4ce4684ea4c" # Warriors
  ),
  alias = c(
    "MIN", # Timberwolves
    "CLE", # Cavaliers
    "LAL", # Lakers
    "CHA", # Hornets
    "LAC", # Clippers
    "BOS", # Celtics
    "WAS", # Wizards
    "DAL", # Mavericks
    "DEN", # Nuggets
    "ATL", # Hawks
    "MIL", # Bucks
    "CHI", # Bulls
    "MEM", # Grizzlies
    "MIA", # Heat
    "NOP", # Pelicans
    "UTA", # Jazz
    "IND", # Pacers
    "TOR", # Raptors
    "ORL", # Magic
    "SAS", # Spurs
    "SAC", # Kings
    "PHX", # Suns
    "OKC", # Thunder
    "POR", # Trail Blazers
    "GSW" # Warriors
  )
)


all_teams_stats <- list()

for (team in 1:nrow(nba_teams)) {
  team_id <- nba_teams$team_id[team]
  team_alias <- nba_teams$alias[team]
  
  
  endpoint <- paste0(
    "https://api.sportradar.com/nba/trial/v8/en/seasons/",
    season_year, "/", season_type, "/teams/", team_id, "/statistics.json?api_key=", api_key
  )
  
  response <- GET(endpoint, add_headers("accept" = "application/json"))
  
  if (response$status_code == 200) {
    # Parse the JSON data
    json_data <- content(response, as = "text", encoding = "UTF-8")
    nba_stats <- fromJSON(json_data, flatten = TRUE)
    
    players_stats <- nba_stats$players
    
    
    team_player_stats <- list()
    
    for (i in 1:length(players_stats$full_name)) {
      # Extract individual player stats
      player_name <- players_stats$full_name[i]
      minutes <- players_stats$total.minutes[i]
      ppg <- players_stats$average.points[i]
      apg <- players_stats$average.assists[i]
      DefRBD <- players_stats$average.def_rebounds[i]
      OffRBD <- players_stats$average.off_rebounds[i]
      spg <- players_stats$average.steals[i]
      bpg <- players_stats$average.blocks[i]
      paint_ppg <- players_stats$average.points_in_paint[i]
      att_3pt_pg <- players_stats$average.three_points_att[i]
      made_3pt_pg <- players_stats$average.three_points_made[i]
      ATO <- players_stats$total.assists_turnover_ratio[i]
      Position <- players_stats$position[i]
      
      # Create a list for the player's stats
      player_stats <- list(
        Team_Alias = team_alias,
        Player_Name = player_name,
        Minutes = minutes,
        PPG = ppg,
        APG = apg,
        DefRBD = DefRBD,
        OffRBD = OffRBD,
        SPG = spg,
        BPG = bpg,
        Paint_PPG = paint_ppg,
        att_3PT_pg = att_3pt_pg,
        made_3PT_pg = made_3pt_pg,
        ATO = ATO,
        Position = Position
      )
      
      
      team_player_stats[[i]] <- player_stats
    }
    
    
    team_player_df <- do.call(rbind.data.frame, team_player_stats)
    
    
    all_teams_stats[[team_alias]] <- team_player_df
    
    
    
    
  } else {
    # Handle the error
    cat("Error fetching data for team", team_alias, ": HTTP status", response$status_code, "\n")
  }
  
  
  Sys.sleep(1.5)  # Delay for 1.5 seconds between requests
}


all_stats_df <- do.call(rbind, all_teams_stats)

write_csv(all_stats_df,'../data/Player_Stats.csv')

