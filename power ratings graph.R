

srs.rank <- van %>%
  mutate(SRS = net) %>%
  left_join(team.info, by = c("team" = "school")) %>% filter(!is.na(conference.x)) %>%
  arrange(desc(SRS)) %>%
  mutate(color = ifelse(color == " ", "#FFFFFF", color),
         color = ifelse(is.na(color), "#FFFFFF", color)
  ) %>% select(logo, team, SRS) %>% ungroup() %>% arrange(desc(SRS)) %>%
  mutate(blank = " ", rank = row_number()) %>% relocate(rank) %>%
  mutate(logo = ifelse(team == "Grand Canyon","https://cdn.d1baseball.com/logos/teams/256/gcanyon.png",logo )) %>%
  mutate(team = 
           case_when(team == "UC Santa Barbara" ~ "UCSB",
                     team == "Miami (FL)" ~ "Miami",
                     TRUE ~ team
           )) %>%
  arrange(desc(SRS)) %>%
  mutate(logo = ifelse(team == "NC State", "http://a.espncdn.com/i/teamlogos/ncaa/500/152.png", logo)) %>% filter(team %in% c("Ole Miss", "Notre Dame", "Auburn", "Arkansas", "Texas A&M", "Texas", "Oklahoma", "Stanford"))


dat1 <- srs.rank %>% filter(rank %in% c(1,5)) %>% select(rank, logo, SRS)
dat2 <- srs.rank %>% filter(rank %in% c(2,6)) %>% select(rank, logo, SRS)
dat3 <- srs.rank %>% filter(rank %in% c(3, 7)) %>% select(rank, logo, SRS)
dat4 <- srs.rank %>% filter(rank %in% c(4, 8)) %>% select(rank, logo, SRS)

colnames(dat2) <- paste0(colnames(dat2), "2")
colnames(dat3) <- paste0(colnames(dat3), "3")
colnames(dat4) <- paste0(colnames(dat4), "4")

ranks <- cbind(dat1, dat2, dat3, dat4)




tbl <- ranks %>% select(logo, logo2, logo3, logo4) %>%
  reactable(
    borderless = TRUE,
    bordered = FALSE,
    outlined = FALSE,
    defaultColDef = colDef(style = list(borderLeft = "2px solid", borderRight = "2px solid", borderBottom = "2px solid", borderColor =  "#0755A5"), headerStyle = list(border = "none")),
    columns = list(
    logo = colDef(name = "",
                  style = list(borderBottom = "4px solid", borderColor =  "#0755A5"), 
                  minWidth = 280,
                  cell = function(value, index){
                    rank <- ranks$rank[index]
                    srs <- ranks$SRS[index]
                    div(
                      div(style = "align: left;padding-top:0;font-size:30px; font-weight: 700; color: #0755A5", paste0("#", rank)),
                      div(style = "align: center; padding-right:15%", img(src = value, height = "120%", width = "120%")),
                      div(style = "float: right; padding-bottom:0; font-size: 44px; font-weight: 700", round(srs, 2))
                    )
                    
                    
                  }),
  logo2 = colDef(name = "",
                 style = list(borderLeft = "4px solid", borderBottom = "4px solid", borderColor =  "#0755A5"), 
               minWidth = 280,
               cell = function(value, index){
                 rank <- ranks$rank2[index]
                 srs <- ranks$SRS2[index]
                 div(
                   div(style = "align: left;padding-top:0;font-size:30px; font-weight: 700; color: #0755A5", paste0("#", rank)),
                   div(style = "align: center; padding-right:15%", img(src = value, height = "120%", width = "120%")),
                   div(style = "float: right; padding-bottom:0; font-size: 44px; font-weight: 700", round(srs, 2))
                 )
                 
                 
                 }),
  logo3 = colDef(name = "",
               minWidth = 280,
               style = list(borderLeft = "4px solid", borderBottom = "4px solid", borderColor =  "#0755A5"), 
               cell = function(value, index){
                 rank <- ranks$rank3[index]
                 srs <- ranks$SRS3[index]
                 div(
                   div(style = "align: left;padding-top:0;font-size:30px; font-weight: 700; color: #0755A5", paste0("#", rank)),
                   div(style = "align: center; padding-right:15%",img(src = value, height = "120%", width = "120%")),
                   div(style = "float: right; padding-bottom:0; font-size: 44px; font-weight: 700", round(srs, 2))
                 )
                 
                 
               }),
  logo4 = colDef(name = "",
               minWidth = 280,
               style = list(borderLeft = "4px solid", borderBottom = "4px solid", borderColor =  "#0755A5"), 
               cell = function(value, index){
                 rank <- ranks$rank4[index]
                 srs <- ranks$SRS4[index]
                 div(
                   div(style = "align: left;padding-top:0;font-size:30px; font-weight: 700; color: #0755A5", paste0("#", rank)),
                   div(style = "align: center; padding-right:15%", img(src = value, height = "120%", width = "120%")),
                   div(style = "float: right; padding-bottom:0; font-size: 44px; font-weight: 700", round(srs, 2))
                 )
                 
                 
               })

  )
) %>%
  reactablefmtr::add_title(title = "@STATSOWAR", font_size = 44, font_weight = "bold", align = "center", background_color = "#0755A5", font_color = "white") %>%
  reactablefmtr::add_subtitle(subtitle = "COLLEGE WORLD SERIES POWER RANKINGS", align = "center", font_size = 64, font_weight = "bold", background_color = "#0755A5", font_color = "white") %>%
  reactablefmtr::google_font("Barlow Condensed", font_weight = 700) #%>%
  #reactablefmtr::add_source(source = list, align = "center", font_size = 34)

reactablefmtr::save_reactable(tbl, "powersb.png", vwidth = 1215)
