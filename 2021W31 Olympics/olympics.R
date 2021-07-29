# Back to business W31 2021
library(magrittr)
library(ggflags) #Needs to be loaded
library(patch)
olympic_data <- tidytuesdayR::tt_load("2021-07-27") # Read data

# scrape country mappings for ggflag
html <- xml2::read_html("https://www.nationsonline.org/oneworld/country_code_list.htm")

country_mapping <- rvest::html_table(html) %>% 
  do.call(rbind,.) %>% 
  dplyr::filter(stringr::str_detect(X5, "\\d{3}")) %>% 
  dplyr::mutate(dplyr::across(c(X3), stringr::str_to_lower)) %>% 
  dplyr::select("country" = X2,
                "alpha2" = X3,
                "alpha3" = X4) %>% 
  dplyr::mutate(country = ifelse(country == "Russian Federation", "Russia", country))

#Colour palette
medal_colour_palette <- rev(c("#daa520",
                          "#c0c0c0",
                          "#cd7f32"))


#Clean the data
regions <- olympic_data$regions
athletes <- olympic_data$olympics %>% 
  dplyr::mutate(games = as.integer(stringr::str_remove(games, " (Summer|Winter)"))) %>% 
  dplyr::rename("player_id" = id) %>% 
  dplyr::left_join(., regions, by = c("noc" = "NOC"))


medal_count <- athletes %>% 
  dplyr::group_by(season) %>% 
  dplyr::count(medal) %>% 
  dplyr::filter(!is.na(medal)) %>% 
  dplyr::ungroup() %>% 
  dplyr::rename("total_medals" = n)

noc_medal_count <- athletes %>% 
  dplyr::group_by(season, region) %>% 
  dplyr::count(medal) %>% 
  dplyr::filter(!is.na(medal)) %>% 
  dplyr::rename("total_medals_noc" = n) %>% 
  dplyr::left_join(medal_count, by = c("season", "medal")) %>% 
  dplyr::mutate(medal_share = total_medals_noc / total_medals,
                medal = factor(medal, levels = rev(c("Gold", "Silver", "Bronze")))) %>% 
  dplyr::ungroup() 
  

noc_medal_share_total <- 
  noc_medal_count %>% 
  dplyr::group_by(season, region) %>% 
  dplyr::summarise(medals_total = sum(total_medals_noc)) %>% 
  dplyr::mutate(medals_total_relative = medals_total / sum(medals_total)) %>% 
  dplyr::slice_max(medals_total_relative, n = 10) 

country_levels <- c(noc_medal_share_total[noc_medal_share_total$season == "Summer","region"])[[1]]

top_performers <- noc_medal_count %>% 
  dplyr::filter(region %in% unique(noc_medal_share_total$region)) %>%
  dplyr::mutate(region = factor(region, levels = rev(country_levels))) %>%
  dplyr::filter(!is.na(region)) %>% 
  dplyr::group_by(medal) 

medals_plot <- ggplot2::ggplot(top_performers)+
  ggplot2::geom_col(ggplot2::aes(region, medal_share, fill= medal), position = "dodge")+
  ggplot2::facet_wrap(~season)+
  ggplot2::scale_fill_manual(values = medal_colour_palette)+
  ggplot2::scale_y_continuous(labels = scales::percent_format())+
  ggplot2::labs(title = "Share of won medals per country",
                subtitle = "Showing the top 10 nations in combined medals",
                x = NULL,
                y = NULL)+
  ggplot2::coord_flip() +
  ggplot2::theme_minimal() +
  ggplot2::theme(plot.title.position = "panel",
                 plot.title = ggplot2::element_text(size = 10),
                 plot.subtitle = ggplot2::element_text(size = 8),
                 panel.grid.major = ggplot2::element_blank(),
                 legend.position = "none")


total_medals <- medal_count %>% 
  dplyr::group_by(season) %>% 
  dplyr::summarise(total = sum(total_medals))

summer_winter_comparison <- athletes %>% 
  dplyr::group_by(season, noc, region) %>%
  dplyr::count(medal) %>%
  dplyr::mutate(score = dplyr::case_when(
    medal == "Gold" ~ n * 8, #Gold shall count slightly more than double Silver
    medal == "Silver" ~ n * 3, #Silver shall count slightly more than double Bronze 
    medal == "Bronze" ~ n * 1,
    TRUE ~ n * 0
  )) %>% 
  dplyr::ungroup() %>% 
  dplyr::group_by(season) %>% 
  dplyr::mutate(score = score / mean(score)) %>% 
  dplyr::group_by(season, region) %>% 
  dplyr::summarise(total_score = sum(score, na.rm = T)) %>% 
  #dplyr::filter(!is.na(medal)) %>% 
  #dplyr::summarise(total_medals_won = sum(n)) %>% 
  #dplyr::left_join(., total_medals, by = "season") %>% 
  #dplyr::mutate(relative_medals_won = total_medals_won / total) %>%
  #dplyr::select(season,region, noc, relative_medals_won) %>% 
  tidyr::pivot_wider(names_from = season,
                     values_from = total_score) %>% 
  dplyr::left_join(., regions[is.na(regions$notes),]) %>% 
  dplyr::rename("noc" = NOC) %>% 
  dplyr::left_join(., country_mapping, by = c("noc" = "alpha3")) %>% 
  dplyr::left_join(., country_mapping, by = c("region" = "country")) %>% 
  dplyr::mutate(country_code = dplyr::case_when(
    !is.na(alpha2.x) ~ alpha2.x,
    TRUE ~ alpha2.y
  )) %>%
  dplyr::group_by(region, country_code) %>% 
  dplyr::select(region, Summer, Winter, country_code) %>% 
  dplyr::ungroup()
#Generate a linear model to get geom_abline as reference
model <- lm(Winter ~ Summer ,summer_winter_comparison)
intercept <- model$coefficients[1]
slope <- model$coefficients[2]

summer_winter_plot <- summer_winter_comparison %>% 
  dplyr::filter(Summer > 0.5, Winter > 0.5) %>% 
ggplot2::ggplot()+
  ggflags::geom_flag(ggplot2::aes(Summer, Winter, country = country_code))+
  ggplot2::annotate(geom = "text", x = 0.0, y = 45, 
    label = "Performing better in winter", hjust = 0, vjust = 0, size = 4
  ) +
  ggplot2::annotate(geom = "text", x = 85, y = 0,
                    label = "Performing better in summer", hjust = 0, vjust = 0, size = 4
  ) +
  ggplot2::geom_abline(ggplot2::aes(slope = slope, intercept = intercept))+
  ggplot2::labs(title = "Seasonal performance", 
                x = "Summer medal score",
                y = "Winter medal score")+
  ggplot2::theme_minimal() +
  ggplot2::theme(plot.title.position = "panel",
                 plot.title = ggplot2::element_text(size = 10),
                 panel.grid = ggplot2::element_blank(),
                 panel.border = ggplot2::element_rect(fill = NA))

(medals_plot + theme(plot.margin = unit(c(20,0,50,0), "pt"))) / summer_winter_plot +
  patchwork::plot_layout(heights = c(1,1.5))+
  patchwork::plot_annotation(title = "Which countries are performing better in which type of Olympics?",
                             subtitle = "Unsurprisingly, northern countries perform relatively better in Winter",
                             caption = "Medal score is each country's sum of medals 
                                        giving 8 points for gold, 3 for silver and 1 for bronze, 
                                        divided by the mean score of all countries")


ggplot2::ggsave(filename = "olympics.svg",
                width = 12,
                height = 9,
                dpi = 600)
