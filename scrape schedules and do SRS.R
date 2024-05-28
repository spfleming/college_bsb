#TBH, haven't run this in a while. Got the SRS code originally off some stack overflow post.

# ALL TEAMS
library(jsonlite)
library(tidyverse)
library(rvest)
library(xml2)


teams <- fromJSON("https://d1baseball.com/wp-content/themes/d1-wp/data/2023-players.json") |> select(team_slug) |> unique() |> 
  arrange(team_slug)

map <- fromJSON("https://d1baseball.com/wp-content/themes/d1-wp/data/2023-players.json") |> group_by(team_name, team_slug) |> count() |> ungroup() |> select(-n)

#305 teams

scrape_func <- function(slug){
  print(slug)
  url <- paste0("https://d1baseball.com/team/", slug, "/")
  possibleError <- tryCatch(sched <- read_html(url) |> 
      html_nodes("#team-schedule > table:nth-child(1)"),   
      error = function(e) e)
  if(!inherits(possibleError, "error")){
      
      links <- sched |> html_elements('.result')
      results <- bind_rows(lapply(xml_attrs(links), 
                                  function(x) data.frame(as.list(x), stringsAsFactors=FALSE)))
  sched <- sched |> html_table() |> as.data.frame()
  colnames(sched) <- sched[1, ] 
  sched <- sched |> as.data.frame() |> 
    filter(Date != "Date") |> 
    mutate(Results = str_trim(Results)) |> 
    select(Date, Loc, Opponent, Results, Notes) |> 
    mutate(tm = slug)  |> as.data.frame() 
  
  sched <- cbind(sched, results)
  }
}


tictoc::tic()
schedules <- purrr::map_dfr(teams$team_slug, scrape_func)
tictoc::toc()

ex.dat <- schedules |> 
  group_by(tm) |> 
  mutate(game.no = row_number(), games = n(), weight = game.no/games) |>
  # ungroup()|>
  # ungroup() |> 
  # group_by(Opponent) |>
  # mutate(opp.games = n()) |>
  # filter(opp.games > 7) |>
  # ungroup() |>
  filter(!grepl("Tue", Date) & !grepl("Wed", Date)) |>
  mutate(Results = as.character(Results)) |> 
  separate(Results, into = c("score", "opp_score"), sep = "-")  |> 
  filter(!is.na(opp_score)) |> 
  mutate(score = str_trim(score),
         opp_score = str_trim(opp_score)) |> 
  mutate(score = as.numeric(score),
         opp_score = as.numeric(opp_score)) |> 
  filter(class == "result win") |> 
  left_join(map, by = c("tm" = "team_slug")) |> 
  mutate(w.loc = ifelse(grepl(" - ", Notes), "N", 
                        ifelse(Loc == "vs", "H", "A"))) |> 
  ungroup() |> 
  select(winner = team_name, w.loc, loser = Opponent, 
         win.score = score, lose.score = opp_score) |> 
  # Correct some weird ones
  mutate(loser = case_when(
    loser == "South Florida" ~ "USF",
    loser == "Saint Mary's" ~ "Saint Mary's (CA)",
    loser == "Sam Houston" ~ "Sam Houston State",
    loser == "UT Rio Grande Valley" ~ "UTRGV",
    loser == "Florida Gulf Coast" ~ "FGCU",
    loser == "Louisiana" ~ "UL Lafayette",
    TRUE ~ loser
  )) 


all.teams <- unique(c(ex.dat$winner, ex.dat$loser))

transform_wl <- function(game_data, team_id){
  
  col_w <- if_else(game_data$winner == team_id, 1, 0) %>%
    na_if(0)
  
  col_l <- if_else(game_data$loser == team_id, -1, 0) %>%
    na_if(0)
  
  col_all <- coalesce(col_w, col_l) %>%
    tbl_df()
  
  return(col_all)
  
}

srs_ex <- map(all.teams, ~ transform_wl(ex.dat, team_id = .x)) %>%
  bind_cols() %>%
  setNames(all.teams) %>%
  replace(is.na(.), 0)%>%
  mutate(loc = fct_recode(ex.dat$w.loc, "1" = "H", "-1" = "A", "0" = "N")) %>%
  mutate(loc = as.numeric(as.character(loc))) |>
  select(loc, everything()) %>%
  as.matrix()


scorediff_ex <- ex.dat%>%
  mutate(scorediff = as.numeric(win.score) - as.numeric(lose.score)) %>%
  select(scorediff) %>% 
  as.matrix()

  
results_ex <- lsei(srs_ex, scorediff_ex)


srs <- tibble(team= colnames(srs_ex),
              SRS = results_ex[[1]]) %>%
  arrange(desc(SRS)) %>% select(team, SRS) 
