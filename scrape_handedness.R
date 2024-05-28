#D1 Names and Ids
url <- "https://d1baseball.com/wp-content/themes/d1-wp/data/2023-players.json"

player.ids <- fromJSON(url) |> as.data.frame()

hands <- as.data.frame(matrix(nrow = 1, ncol = 3))
colnames(hands) <- c("team", "player", "hand")

get_hands <- function(i){
  print(i)
  
  url <- player.ids$player_url[i]
  name <- player.ids$player_name[i] |> as.data.frame()
  team <- player.ids$team_name[i] |> as.data.frame()
  
  a <- url |> read_html() |> 
    html_nodes("div.info-item:nth-child(2)") |> 
    html_text()  |> as.data.frame()
  names(a) <- "hand"
  
  possibleError <- tryCatch(tmp <- cbind(name, team, a),
                            error = function(e) e)
  if(!inherits(possibleError, "error")){
    tmp <- cbind(name, team, a)
  }
}

library(furrr)
library(future)
library(jsonlite)
library(rvest)
plan(multisession, workers = availableCores())

tictoc::tic()
handedness1 <- future_map_dfr(seq(1, 2000), get_hands)
handedness2 <- future_map_dfr(seq(2001, 4000), get_hands)
handedness3 <- future_map_dfr(seq(4001, 6000), get_hands)
handedness4 <- future_map_dfr(seq(6001, 8000), get_hands)
handedness5 <- future_map_dfr(seq(8001, 10000), get_hands)
handedness6 <- future_map_dfr(seq(10000, 11688), get_hands)
tictoc::toc()


hands.all <- rbind(handedness1, handedness2, handedness3, 
                   handedness4, handedness5, handedness6)

colnames(hands.all) <- c("name", "team", "hand")

hands.all <- hands.all |> group_by(name, team) |> 
  filter(row_number() == 1) |> 
  separate(hand, into = c("junk", "bat", "bat_throw"), sep = "\n") |> 
  select(-junk, -bat) |> 
  separate(`bat_throw`, into = c("bat", "throw"), sep = "/") |> 
  mutate(throw = str_trim(throw))

write.csv(hands.all, "data/pitcher_hands.csv")

hands.all2 <- hands.all |> separate(name, 
                                    into = c("first", "last"), 
                                    sep = " ") |> 
  mutate(last = toupper(last))

get_starters <- function(id){
  url <- paste0("https://data.ncaa.com/casablanca", id, "/boxscore.json")
  
  possibleError <- tryCatch(dat <- jsonlite::fromJSON(url), 
                            error = function(e) e)
  if(!inherits(possibleError, "error")){
  pitch <- dat$teams |> as.data.frame()
  
  away.starter <- pitch$pitcherStats[[1]][1:2] |> as.data.frame() |> filter(row_number() == 1) |> mutate(away_pitcher = lastName) |> 
    select(away_pitcher)
  
  home.starter <- pitch$pitcherStats[[2]][1:2] |> as.data.frame() |> filter(row_number() == 1) |> mutate(home_pitcher = lastName) |> 
    select(home_pitcher)
  
  tmp <- cbind(id, away.starter, home.starter)
  }
}

home.away <- games |> select(home, away, id = url) |> 
  group_by(id) |> filter(row_number() == 1)
games.clean <- games |> filter(!url %in% c("/game/6122870"))
starters <- future_map_dfr(games.clean$url, get_starters)



unique(hands.all$team)
unique(c(starters2$home, starters2$away)) |> view()
a <- unique(hands.all$team)
write.csv(a, "~/Desktop/team_walk.csv")

teams <- read.csv("~/Downloads/D1_to_NCAA - team_walk.csv")
hands.all3 <- hands.all2 |> 
  left_join(teams, by = c("team"="d1_name"))

starters2 <- starters |> 
  mutate(away_pitch = toupper(home_pitcher),
         home_pitch = toupper(away_pitcher)) |> 
  select(-home_pitcher, -away_pitcher) |> 
  left_join(home.away, by = c("id")) |> 
  left_join(hands.all3, 
            by = c("home_pitch" = "last", "home" = "team")) |> 
  left_join(hands.all3,
            by = c("away_pitch" = "last", "away" = "team")) |> 
  select(url =id, home_p = throw.x, away_p = throw.y) |> 
  mutate(home_p = ifelse(is.na(home_p), "R", home_p),
         away_p = ifelse(is.na(away_p), "R", away_p))
