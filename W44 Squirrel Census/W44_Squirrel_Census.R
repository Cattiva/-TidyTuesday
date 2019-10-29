library(tidyverse)
library(extrafont)
library(magick)
library(ggimage)
library(jpeg)
library(grid)

img <- readJPEG(str_c(getwd(), "/centralparkmap2.jpg"))
img_squirrel <- readJPEG(str_c(getwd(), "/squirrel.jpg"))

nyc_squirrels <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-10-29/nyc_squirrels.csv")

# Preprocessing
squirrel_action <- nyc_squirrels %>% 
  select(unique_squirrel_id, hectare, shift, age, running, chasing, climbing, eating, foraging) %>% 
  mutate(hectare_ns = str_sub(hectare, 1,2), # Dividing Hectare IDs for x and y axis
         hectare_ew = str_sub(hectare, 3,3)) %>% 
  pivot_longer(cols = c(running:foraging),
               names_to = "action",
               values_to = "lgl") %>% 
  filter(lgl == TRUE) %>% 
  select(-lgl) %>% 
  count(hectare_ns, hectare_ew, action, sort = TRUE) %>% 
  mutate(hectare_ns = str_replace_all(hectare_ns, "0", ""),
         hectare_ns = factor(hectare_ns, levels = c(1:42)),
         hectare_ew = factor(hectare_ew, levels = c("A", "B", "C", "D", "E", "F", "G", "H", "I")))

#Background colour
background <- "#FFEEDBFF"  


#Plot
ggplot(squirrel_action,
       aes(hectare_ew, as_factor(hectare_ns), size = n)) +
  annotation_custom(rasterGrob(img, 
                               width = unit(1,"npc"),
                               height = unit(1,"npc")), 
                    -Inf, Inf, -Inf, Inf)+
  geom_jitter(shape = "🐿", #Unicode for the squirrel
             color = "#68392EFF",
             stroke = 2) +
  facet_grid(~ action)+
  theme_minimal()+
  theme(plot.title = element_text(color = "#68392EFF",
                                  face = "bold"),
        plot.background = element_rect(fill = alpha(background,0.4)),
        text = element_text(color = "#68392EFF"),
        strip.text = element_text(color = "#68392EFF"),
        panel.grid = element_blank(),
        axis.text = element_blank(),
        axis.title = element_blank()) +
  labs(title = "Squirrel Census - Where are Squirrels doing things?",
       size = "Count of Squirrels",
       caption = "Viz by @alexnickel_skol | data by NYC Squirrel Census")

ggsave("squirrelcensus.png")
