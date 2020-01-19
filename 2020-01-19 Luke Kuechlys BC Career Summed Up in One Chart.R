# Imports
library(httr)
library(jsonlite)
library(tidyverse)
library(plotly)

# Luke Tackles Data
luke_tackles <- read.csv('D:/Rich/Documents/Fun/Independent Projects/CFB/eagleytics/lukedata.csv')

# Hit CFB Data API
cfb_api <- function(path, query="") {
  url <- modify_url("https://api.collegefootballdata.com", path=path, query=query)
  print(url)
  
  resp <- GET(url)
  if (http_type(resp) != "application/json") {
    stop("API did not return json", call. = FALSE)
  }
  
  parsed <- fromJSON(content(resp, "text"))
  
  return(parsed)
}

# Define Function to Get Tackle Stats for A Game
parse_game_tackles <- function(game) {
  
  tryCatch({
    
    gameid <- game$id
    game <- game$teams[[1]]
    
    team1name <- game$school[1]
    team1 <- game$categories[[1]]
    team1stats <- team1$types[team1$name=="defensive"][[1]]
    team1tackles <- team1stats$athletes[team1stats$name=="TOT"][[1]]
    team1tackles_df <- cbind(gameid, teamname=team1name, team1tackles)
    
    team2name <- game$school[2]
    team2 <- game$categories[[2]]
    team2stats <- team2$types[team2$name=="defensive"][[1]]
    team2tackles <- team2stats$athletes[team2stats$name=="TOT"][[1]]
    team2tackles_df <- cbind(gameid, teamname=team2name, team2tackles)
    
    game_tackles_df <- rbind(team1tackles_df, team2tackles_df)
    colnames(game_tackles_df) <- c("gameid", "teamname", "playerid", "playername", "tackles")
    
    return(game_tackles_df)
    
  }, error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
}

# Initialize Empty Data Frame
tackles_df <- data.frame()
output_df <- data.frame()

# Iterate Through Years
for (year in 2009:2019) {
  cat("Year", year, "\n")
  game_df <- cfb_api("/games", paste("year=", year, sep=""))
  postseason_game_df <- cfb_api("/games", paste("year=", year, "&seasonType=postseason", sep=""))
  game_df <- rbind(game_df, postseason_game_df)

  # Iterate Through All Weeks in the Regular Season
  for (week in 1:15) {
    cat("Week", week, "\n")
    game_stats_df <- cfb_api("/games/players", paste("year=", year, "&week=", week, sep=""))
    
    # Iterate Through All Games in the Week and Parse Tackles
    for (game in 1:nrow(game_stats_df)) {
      cat("Game", game, "\n")
      tackles_df <- rbind(parse_game_tackles(game_stats_df[game,]), tackles_df)
    }
  }
  
  # Postseason
  cat("Postseason", "\n")
  game_stats_df <- cfb_api("/games/players", "year=2019&week=1&seasonType=postseason")
  
  # Iterate Through All Games in the Week and Parse Tackles
  for (game in 1:nrow(game_stats_df)) {
    cat("Game", game, "\n")
    tackles_df <- rbind(parse_game_tackles(game_stats_df[game,]), tackles_df)
  }

  # Create Output Data
  output <- tackles_df %>%
    inner_join(game_df, by=c("gameid"="id"))
  
  output_df <- rbind(output_df, output)
}

# Add Career Game Num
output_df <- output_df %>% 
  group_by(playerid) %>% 
  arrange(playerid, start_date) %>%
  mutate(gamenum=row_number(start_date), cum_tackles=cumsum(tackles))
  
# Get Top 10 Tacklers
top_tacklers <- output_df %>%
  group_by(playerid, playername) %>%
  summarize(career_tackles=sum(as.integer(tackles))) %>%
  arrange(desc(career_tackles)) %>%
  ungroup() %>%
  top_n(1000)

top_tacklers_cum <- output_df %>%
  inner_join(top_tacklers)

# Plot
ggplot() + 
  geom_line(data=top_tacklers_cum, aes(x=gamenum, y=cum_tackles, group=playername), color="gray") +
  geom_line(data=luke_tackles, aes(x=GameNum, y=cum_tackles), color="maroon", size=2) + 
  geom_hline(aes(yintercept=545)) +
  geom_text(aes(x=20, y=545, label="545 - FBS Record (Since 2000) Tim McGarigle, Northwestern, 2002-05 (48 games)"), nudge_y=10) +
  scale_y_continuous(breaks=seq(0, 600, 100)) +
  labs(x="Career Game #", y="Cumulative Career Tackles", title="Luke Kuechly (2009-11) vs. Everyone Else (2016-19)", subtitle="@eagleytics", caption="Data: @CFB_Data")



