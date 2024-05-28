library(dplyr)
library(jsonlite)



# Create sequence for scraping
start_date <- as.Date("02/15", format = "%m/%d")
end_date <- as.Date("05/26", format = "%m/%d")
days <- stringr::str_remove_all(seq(start_date, end_date, by = "days"), "2024-")
days <- stringr::str_replace_all(days, "-", "\\/")

ncaa_games <- function(date) {
  print(date)
  possibleError <- tryCatch(gms <- jsonlite::fromJSON(
    paste0("https://data.ncaa.com/casablanca/scoreboard/baseball/d1/2024/", 
           date, "/scoreboard.json")) |>
      as.data.frame() |> mutate(season = szn),
    error = function(e) e)
  if(!inherits(possibleError, "error")) {
    gms <- jsonlite::fromJSON(
    paste0("https://data.ncaa.com/casablanca/scoreboard/baseball/d1/2024/", 
           date, "/scoreboard.json")) |>
      as.data.frame() |> mutate(season = szn) |> 
      filter(game$finalMessage == "FINAL") |> 
      filter(game$gameState == "final")
  }
}



tictoc::tic()
gms <- purrr::map_dfr(days, ncaa_games) |>
  as.data.frame()
tictoc::toc()





games24 <- gms |> 
   filter(game$gameState == "final") |> 
     tidyr::unnest(cols = c(game)) |> 
        as.data.frame() |> 
        select(url, home, away, bracketRegion, startDate, gameID) |> 
        tidyr::unnest(cols = c(home)) |> 
        tidyr::unnest(cols = c(names)) |> 
        select(url, home.score = score, home = short, home.winner = winner, home.conf = conferences, away, startDate, gameID) |> 
        tidyr::unnest(cols = c(away)) |> 
        tidyr::unnest(cols = c(names))  |> 
        select(url, gameID, startDate, home.score, home, home.winner, home.conf, away = short, away.score = score, away.conf = conferences) |> 
  as.data.frame() |> 
  tidyr::unnest(cols = c(home.conf)) |> 
  mutate(home.conf = conferenceName) |> select(-conferenceName, -conferenceSeo) |> 
  tidyr::unnest(cols = c(away.conf)) |> 
   mutate(away.conf = conferenceName) |> select(-conferenceName, -conferenceSeo)  |> 
   mutate(home.score = as.numeric(home.score), away.score = as.numeric(away.score)) |> 
    #mutate(home.score = ifelse(home.score > 12, 12 + 2*atan(home.score-12), home.score)) |> 
    #mutate(away.score = ifelse(away.score > 12, 12 + 2*atan(away.score-12), away.score)) |> 
   mutate(home.response = home.score, away.response = away.score, binary.response = ifelse(home.score > away.score, 1, 0))  |> 
   na.omit() |> 
   mutate(neutral.site = 0)

