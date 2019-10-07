##### Week 38 contribution to #TidyTuesday
##### National Park Visits in the US

#Load libraries
library(tidyverse)
library(lubridate)
library(ggrepel)
library(extrafont)
library(sf)
library(usmap)
library(transformr)
library(maps)
library(cluster)

#Read from locale or Github
park_visits <- readr::read_csv(str_c(getwd(), "/national_parks.csv"))

natural_colour <- c("#68392EFF", "#E3AA61FF", "#4F834FFF", "#28663FFF","#935341FF", "#68392E90", "#93534180", "#E3AA6180", "#4F834F80", "#28663F80")
  
background <- "#FFEEDBFF"  

######Total Visits per State######

#Make year to date
park_visits$year <- make_date(park_visits$year)

#Summarise by total visitors and select top 8 for not so crowded plot
top_states <- park_visits %>% 
  group_by(state) %>% 
  summarise("total_visitors" = sum(visitors)) %>% 
  arrange(desc(total_visitors)) %>% 
  top_n(8)

#Summarise by Visits per State per Year
visits_per_state <- park_visits %>% 
  group_by(year, state) %>% 
  summarise("visits_per_year" = sum(visitors)) %>% 
  filter(!is.na(visits_per_year),
         state %in% top_states$state, #That's why the top_states were identified
         !is.na(year)) %>% 
  ungroup()

#Plot this
ggplot(visits_per_state)+
  geom_line(aes(year, visits_per_year, group = state, colour = state), 
            na.rm = TRUE, 
            size = 1)+
  scale_colour_manual(values = natural_colour)+ #From the custom colour palette
  scale_y_continuous(labels = scales::comma, 
                     expand = c(0,0))+ #Big number separator and expand() used for avoiding blank space over axis
  theme(panel.background = element_blank(),
        panel.grid = element_blank(),
        plot.background = element_rect(fill = alpha(background,0.2)),
        axis.line = element_line(colour = "black"),
        axis.text = element_text(colour = "black"),
        legend.background = element_blank()
        )+
  labs(title = "National Park visitors per year for eight most visited States",
       x = "Year",
       y = "Visitors per Year",
       colour = "State",
       caption = "Data by dataisplural/data.world \n Visuals by @alexnickel_skol"
       )

ggsave("visit_per_state.png")

######CA and DC National Parks##########

#Identify Top 5 Parks in both states
comp <- park_visits %>% 
  filter(state == c("CA", "DC"),
         year == "2016-01-01",
         !is.na(parkname)) %>% 
  group_by(year, state, parkname) %>% 
  summarise(total_visitors = sum(visitors)) %>% 
  arrange(desc(total_visitors)) %>% 
  ungroup() %>% 
  group_by(state) %>% 
  top_n(5)

#Calculate sum of visitors per year 
total_parks <- park_visits %>% 
  filter(parkname %in% comp$parkname,
         !is.na(year)) %>% 
  group_by(year, state, parkname) %>% 
  summarise(total = sum(visitors)) %>% 
  ungroup() %>% 
  group_by(parkname) %>% 
  mutate(top = max(total))

#Plot
ggplot(total_parks, aes(year, total, colour = parkname))+
  geom_line(aes(linetype = state), size = 1)+
  geom_text_repel(
    data = total_parks %>% filter(total %in% top), #subsetting is needed, otherwise it comes to accidental aRt
    aes(label = parkname, colour = parkname),
    nudge_x = -50000, #set the labels to left side of plot
    direction = "y", #align vertically
    point.padding = unit(0.5, "lines"),
    box.padding = unit(0.5, "lines"),
    segment.colour = "#CCCCCC", 
    family = "Calibri")+
  scale_colour_manual(values = natural_colour,
                      name = NULL,
                      breaks = NULL,
                      labels = NULL)+
  scale_y_continuous(labels = scales::comma,
                     expand = c(0,0))+
  scale_linetype_discrete(name = "State",
                          breaks = c("CA", "DC"),
                          labels = c("CA", "DC"))+
  scale_x_date(expand = c(0,0))+
  theme(panel.background = element_blank(),
        panel.grid = element_blank(),
        plot.background = element_rect(fill = alpha(background,0.2)),
        axis.line = element_line(colour = "black"),
        axis.text = element_text(colour = "black"),
        legend.background = element_blank(),
        text = element_text(family = "Calibri")
        )+
  labs(title = "National Park visitors per year for most frequented parks in CA and DC",
       x = "Year",
       y = "Visitors per Year",
       colour = "Park Name",
       linetype = "State",
       caption = "Data by dataisplural/data.world \n Visuals by @alexnickel_skol")

ggsave("ca_dc.png")

#Type of Parks

#Collapse factors to categories and calculate total visitors
type <- park_visits %>% 
  mutate(unit_type = as_factor(unit_type),
         unit_type = fct_collapse(unit_type,
                                  Nature = c("Ecological and Historic Preserve", "National Lakeshore", "National Park", "National Preserve", "National Recreation Area", "National Recreation River", "National Reserve", "National River", "National River and Recreation Area", "National Scenic River", "National Scenic Riverway", "National Seashore", "Park", "Scenic and Recreational River", "Wild and Scenic River"),
                                  History = c("International Historic Site", "National Historic Site", "National Historical Park", "National Historical Park and Preserve", "National Memorial", "National Monument"),
                                  War = c("National Battlefield", "National Battlefield Park", "National Battlefield Site", "National Military Park", "War in the Pacific"),
                                  Other = c("National Parkway", "Other Designation", "Parkway"))) %>%
  group_by(state, unit_type) %>% 
  filter(state %in% c("CA", "DC"),
         year == "2016-01-01") %>% 
  summarise(total_visitors = sum(visitors))

#Plot
ggplot(type)+
  geom_col(aes(unit_type, total_visitors, fill = state), 
           position = "dodge")+
  scale_fill_manual(values = c("#68392EFF", "#4F834FFF"))+
  scale_y_continuous(labels = scales::comma,
                     expand = c(0,0))+
  theme(panel.background = element_blank(),
        panel.grid = element_blank(),
        plot.background = element_rect(fill = alpha(background,0.2)),
        axis.line = element_line(colour = "black"),
        axis.text = element_text(colour = "black"),
        legend.background = element_blank(),
        text = element_text(family = "Calibri")
  ) +
  labs(title = "Visitors by park category in CA and DC (as of 2016)",
       x = "Category",
       y = "Total Visitors",
       fill = "State",
       caption = "Data by dataisplural/data.world \n Visuals by @alexnickel_skol")

ggsave("type_of_parks_dc_ca.png")

#US Map

#Same preprocessing as above, collapse factors
map <- park_visits %>% 
  mutate(unit_type = as_factor(unit_type),
         unit_type = fct_collapse(unit_type,
                                       Nature = c("Ecological and Historic Preserve", "National Lakeshore", "National Park", "National Preserve", "National Recreation Area", "National Recreation River", "National Reserve", "National River", "National River and Recreation Area", "National Scenic River", "National Scenic Riverway", "National Seashore", "Park", "Scenic and Recreational River", "Wild and Scenic River"),
                                       History = c("International Historic Site", "National Historic Site", "National Historical Park", "National Historical Park and Preserve", "National Memorial", "National Monument"),
                                       War = c("National Battlefield", "National Battlefield Park", "National Battlefield Site", "National Military Park", "War in the Pacific"),
                                       Other = c("National Parkway", "Other Designation", "Parkway"))) %>% 
  group_by(state) %>%
  filter(!is.na(visitors),
         !is.na(unit_type)) %>% 
  mutate(pct = visitors / sum(visitors)) %>% #needed for fill colour
  ungroup() %>% 
  group_by(state, unit_type) %>% 
  summarise(total_pct = sum(pct)) %>% 
  filter(total_pct == max(total_pct)) #Selects the unit type with the highest percentage per state

colnames(map)[1]<-'abbr'

#US Map Data
state_map <- us_map(regions = "states")

#join with preprocessed data 
mapdata <- left_join(state_map, map)

#Plot

ggplot(mapdata,
       aes(x, 
           y, 
           fill = unit_type,
           group = group), 
       na.rm = TRUE)+
  geom_polygon(na.rm = TRUE, 
               colour = alpha(background, 0.3), #For nice subtile state borders
               size = 0.1)+
  coord_equal()+
  theme_void()+
  scale_fill_manual(values = c("#28663FFF", "#E3AA61FF", "#68392E90", "#68392EFF"))+
  theme(panel.background = element_blank(),
        plot.background = element_rect(fill = alpha(background,0.2)),
        legend.background = element_blank(),
        text = element_text(family = "Calibri")
  )+
  labs(title = "Most common National Park type per state",
       subtitle = "West is nature, East is history",
       fill = "National Park Type",
       caption = "Data by dataisplural/data.world \n Visuals by @alexnickel_skol"
  )

ggsave("map.png")
