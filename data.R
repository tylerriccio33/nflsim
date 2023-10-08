
colors <- brewer.pal(9, 'RdYlGn')


results <- read_rds("unpacked.rds") %>%
  select(replication_results = value) %>%
  filter(!map_lgl(replication_results, is.null))


all_players <- nflreadr::load_players()

player_name_lookup <- all_players %>%
  select(id_gsis = gsis_id, name = short_name) %>%
  drop_na(id_gsis, name) 

player_headshot_lookup <- all_players %>%
  select(id_gsis = gsis_id, head = headshot)

teams <- nflreadr::load_teams()

logo_lookup <- select(teams, id_posteam = team_abbr,
                      logo = team_wordmark)


game_results_data <- results %>%
  extract_variable(id_home_team, id_away_team, id_game) %>%
  extract_variable(id_home_team, id_away_team, id_game) %>%
  mutate(home_score = map2_dbl(replication_results, id_home_team, ~ {
    last_diff <- summarize(.x, diff = last(score_differential))
    last_diff <- last_diff$diff
    last_team <- summarize(.x, team = last(id_posteam))
    last_team <- last_team$team
    corrected <- if_else(last_team == .y, last_diff, last_diff * -1)
    return(corrected)
  })) %>%
  mutate(game = glue("{id_away_team} - {id_home_team}"))