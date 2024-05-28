

#INDIVIDUAL
wRAA <- bats %>% filter(!grepl("Totals", Player)) %>%
  filter(AB > 0) %>%
  mutate(
    wOBA = (.75*BB + .76*HBP + .94*(H-`2B`-`3B`-HR) + 1.32*`2B` + 1.63*`3B` + 2.07*HR)/AB)

avg.wOBA <- 0.360
wOBA.scale <- 1.19
runsPA <- 0.15

library(magick)
library(showtext)
showtext_auto()

easton <- image_read("https://dbukjj6eu5tsf.cloudfront.net/sidearm.sites/texastech.com/images/2021/4/14/20210414_183210449_iOS.jpg") %>% image_crop("750x1050+500+75") %>%
  image_scale(400) 
  

easton
jung <- image_read("https://external-content.duckduckgo.com/iu/?u=https%3A%2F%2Fdbukjj6eu5tsf.cloudfront.net%2Fgofrogs.com%2Fimages%2F2020%2F2%2F22%2FInside_Minn_71.jpg&f=1&nofb=1") %>%
  image_crop("750x1050+800+55") %>%
  image_scale(400) 
jung
ivan <- image_read("https://external-content.duckduckgo.com/iu/?u=https%3A%2F%2Fs.hdnux.com%2Fphotos%2F01%2F24%2F42%2F74%2F22163234%2F3%2F1200x0.jpg&f=1&nofb=1") %>% 
  image_crop("750x1050+300+75") %>%
  image_scale(400) 

ivan

big12 <- image_read("~/Desktop/banner.png") %>%
  image_rotate(-90) 
blogo <- image_read("https://external-content.duckduckgo.com/iu/?u=https%3A%2F%2Flarrybrownsports.com%2Fwp-content%2Fuploads%2F2020%2F08%2Fbig-12.jpg&f=1&nofb=1") %>%
  image_scale(280)

background <- image_read("~/Desktop/background.jpg") %>%
  image_scale(1600)

table1 <- image_read("~/Desktop/table1.png") %>% image_scale(415)
table2 <- image_read("~/Desktop/table2.png") %>% image_scale(415)
table3 <- image_read("~/Desktop/table3.png") %>% image_scale(415)


trace(grDevices::png, exit = quote({
  showtext::showtext_begin()
}), print = FALSE)

chart <- image_composite(background, big12, offset = "+0+0") %>%
  image_annotate("BIG 12 BASEBALL", gravity = "north",
                 location = "+0+0", color = "#494949", size = 100) %>%
  image_annotate("2022 END OF SEASON BATTING STAT LEADERS", 
                 style = "Italic", size = 45, color = "#494949", gravity = "north", location = "+00+100", font = "Helvetica") %>%
  image_composite(easton, offset = "+200+150") %>%
  image_composite(ivan, offset = "+650+150") %>%
  image_composite(jung, offset = "+1100+150") %>%
  image_composite(blogo, offset = "+0+840") %>%
  image_annotate(OBP$Player[2], location = "+200+595", size = 30,color = "white", boxcolor = OBP$color[2]) %>%
  image_annotate(paste(" wOBA:", round(OBP$wOBA[2], 3)),  font = "Helvetica", location = "+200+630", size = 35,color = "white", boxcolor = OBP$color[2]) %>%
  image_annotate(wRC$Player[1], location = "+650+590",  font = "Helvetica", size = 35,color = "white", boxcolor = wRC$color[1]) %>%
  image_annotate(paste(" wRC:", round(wRC$wRC[1], 1)),  font = "Helvetica", location = "+650+630", size = 35,color = "white", boxcolor = wRC$color[1]) %>%
  image_annotate(paste(wRA$Player[7], ""), location = "+1100+590",  font = "Helvetica", size = 35,color = "white", boxcolor = wRA$color[7]) %>%
  image_annotate(paste("wRAA: ", round(wRA$wRAA[7], 1)),  font = "Helvetica", location = "+1100+630", size = 35,color = "white", boxcolor = wRA$color[7]) %>%
  image_composite(table1, offset = "+280+670") %>%
  image_composite(table2, offset = "+730+670") %>%
  image_composite(table3, offset = "+1180+670")
  



chart

 OBP <- wRAA %>% 
  mutate(wRAA = AB*(wOBA - avg.wOBA)/wOBA.scale,
         wRC = ((wOBA - avg.wOBA)/wOBA.scale + runsPA)*AB) %>%
  left_join(team.info, by = c("school")) %>%
  arrange(desc(wOBA)) %>% filter(AB > 99) %>% filter(row_number() < 10) %>%
  select(logo, Player, color, wOBA) %>%
  separate(Player, into = c("Last", "First"), sep = ",") %>%
  mutate(Player = toupper(paste(First, Last))) %>% select(-First, -Last) %>%
  relocate(logo, Player, wOBA)

library(reactable)

tbl1 <- OBP %>% select(-color) %>%
  reactable(
    columns = list(
      logo = colDef(name = "",
                    maxWidth = 60,
                    cell = embed_img(height = 55, width = 55)
                    ),
      Player = colDef(name = "",
                      maxWidth = 350,
                      cell = function(value, index){
                        col <- OBP$color[index]
                        tagList(
                        div(value, style = paste0("padding-top: 10px; font-size: 30px; color: ", col)))
                      }
                      ),
      wOBA = colDef(
        maxWidth = 150,
        format = colFormat(digits = 3),
        style = list(fontSize = 45)
      )
    )
  ) 

save_reactable(tbl1, "tbl1.png", vheight = 500, vwidth = 500)


tbwRC <- wRAA %>% 
  mutate(wRAA = AB*(wOBA - avg.wOBA)/wOBA.scale,
         wRC = ((wOBA - avg.wOBA)/wOBA.scale + runsPA)*AB) %>%
  left_join(team.info, by = c("school")) %>%
  arrange(desc(wRC)) %>%  filter(AB > 99) %>% filter(row_number() < 10) %>%
  select(Player, logo, color, wRC) %>%
  separate(Player, into = c("Last", "First"), sep = ",") %>%
  mutate(Player = toupper(paste(First, Last))) %>% select(-First, -Last) %>%
  relocate(logo, Player, wRC)

wRC %>% select(-color) %>%
  reactable(
    columns = list(
      logo = colDef(name = "",
                    maxWidth = 60,
                    cell = embed_img(height = 55, width = 55)
      ),
      Player = colDef(name = "",
                      maxWidth = 350,
                      cell = function(value, index){
                        col <- wRC$color[index]
                        tagList(
                          div(value, style = paste0("padding-top: 10px; font-size: 30px; color: ", col)))
                      }
      ),
      wRC = colDef(
        format = colFormat(digits = 1),
        style = list(fontSize = 45)
      )
    )
  ) 
wRA <- wRAA %>% 
  mutate(wRAA = AB*(wOBA - avg.wOBA)/wOBA.scale,
         wRC = ((wOBA - avg.wOBA)/wOBA.scale + runsPA)*AB) %>%
  left_join(team.info, by = c("school")) %>%
  arrange(desc(wRAA)) %>% filter(AB > 99) %>% filter(row_number() < 10) %>%
  select(Player, logo, color, wRAA)%>%
  separate(Player, into = c("Last", "First"), sep = ",") %>%
  mutate(Player = toupper(paste(First, Last))) %>% select(-First, -Last) %>%
  relocate(logo, Player, wRAA)
  
wRA %>% select(-color) %>%
  reactable(
    columns = list(
      logo = colDef(name = "",
                    maxWidth = 60,
                    cell = embed_img(height = 55, width = 55)
      ),
      Player = colDef(name = "",
                      maxWidth = 350,
                      cell = function(value, index){
                        col <- wRA$color[index]
                        tagList(
                          div(value, style = paste0("padding-top: 10px; font-size: 30px; color: ", col)))
                      }
      ),
      wRAA = colDef(
        format = colFormat(digits = 1),
        style = list(fontSize = 45)
      )
    )
  ) 
