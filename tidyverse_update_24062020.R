library(tidyverse)
library(here)
#
got_char <- read_csv(here("01_data", "got_char.csv"))
got_houses <- read_csv(here("01_data", "got_houses.csv"))

got_complete <- got_char %>% 
  left_join(got_houses, by = c("actor" = "name"))
  
top_10 <- got_complete %>%   
  mutate(total = season_1 + season_2, season_3) %>% 
  arrange(desc(total)) %>% 
  select(-starts_with("season"), -gender) %>% 
  rename(
    Character = actor,
    'Total Minutes' = total,
    House = house_a
  ) %>% 
  slice(1:10)
View(top_10)

# Exercise 2 --------------------------------------------------------------
got_houses %>%
  count(house_a, gender, sort = TRUE)

got_houses_plot <- got_houses %>% 
  drop_na(house_a) %>% 
  group_by(house_a) %>% 
  mutate(n = n()) %>% 
  ungroup() %>% 
  filter(n >= 10) %>% 
  mutate(gender_cat = ifelse(gender == 0, "Female", "Male"))
  #' mutate(gender = ifelse(gender == 0, "Female", "Male"))
  #'  This would rewrite the "gender" variable instead of crearing a new one.

got_houses_plot %>% 
  ggplot(aes(reorder(house_a, n),
             fill = gender_cat)) +
  geom_bar()

got_complete %>% 
  pivot_longer(
    cols = season_1:season_7,
    names_to = "season",
    values_to = "time"
  )

got_long %>% 
  group