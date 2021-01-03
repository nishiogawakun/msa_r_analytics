library(tidyverse)
library(dplyr)
library(ggplot)
library(ggthemes)
pbp_2018 <- read_csv("reg_pbp_2018.csv")


clutch_data <- pbp_2018 %>% filter((score_differential < 0 & score_differential >= -7) &
  qtr == 4 &
  down > 2 &
  play_type == 'pass') %>% select(epa, passer_player_name) %>%
  filter(!is.na(epa))

clutch_qb <- clutch_data %>% group_by(passer_player_name) %>%
  summarize(average_epa = mean(epa)) %>% arrange(desc(average_epa))

head(clutch_qb, 10)

clutch_qb$passer_player_name <- 
  factor(clutch_qb$passer_player_name, levels = clutch_qb$passer_player_name[order(clutch_qb$average_epa)])

ggplot(clutch_qb, aes(x = passer_player_name, y = average_epa)) + geom_col() + 
  theme_clean() + theme(axis.text.x=element_text(angle = -90, hjust = 0)) + 
  labs(title="Who Were the Most Clutch QB's in 2018?",x="QB", y = "Clutch Metric")
