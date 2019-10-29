library(tidyverse)


nyc_squirrels <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-10-29/nyc_squirrels.csv")

squirrel_sound <- nyc_squirrels %>% 
  select(unique_squirrel_id, shift, age, kuks, quaas, moans, tail_flags, tail_twitches)

squirrel_sound %>% 
  pivot_longer(cols = c(kuks, quaas, moans),
               names_to = "type",
               values_to = "sound") %>% 
  pivot_longer(cols = c(tail_flags, tail_twitches),
               names_to = "movement",
               values_to = "exag") %>% 
  filter(sound == TRUE) %>% 
  count(movement, type, sort = TRUE)


squirrel_action <- nyc_squirrels %>% 
  select(unique_squirrel_id, hectare, shift, age, running, chasing, climbing, eating, foraging, kuks, quaas, moans, tail_flags, tail_twitches, approaches, indifferent, runs_from) %>% 
  mutate(hectare_ns = str_sub(hectare, 1,2),
         hectare_ew = str_sub(hectare, 3,3))

squirrel_action$hectare %>% 
  str_sub(1,2)

  
actions <- squirrel_action %>% 
  pivot_longer(cols = c(running:runs_from),
               names_to = "action",
               values_to = "lgl") %>% 
  filter(lgl == TRUE,
         action == "foraging") %>% 
  select(-lgl) %>% 
  count(hectare_ns, hectare_ew, action, sort = TRUE)
 
 test <- actions %>% 
   mutate(hectare_ns = str_replace_all(hectare_ns, "0", ""),
          hectare_ns = factor(hectare_ns, levels = c(1:42)),
          hectare_ew = factor(hectare_ew, levels = c("A", "B", "C", "D", "E", "F", "G", "H", "I")))
  
test$hectare_ns
  
  factor(actions$hectare_n, levels = c(1:42))
  
ggplot(test,
       aes(hectare_ew, as_factor(hectare_ns), fill = n)) +
  geom_tile()
