library(here)
library(tidyverse)
library(ggridges)
library(viridis)
library(gridExtra)

anime_data <- read_csv(here::here('20190423','data','tidy_anime.csv'))

anime_data %>% glimpse()
summary(anime_data)

library(autoEDA)

# Quick data review
autoEDA(anime_data)

# plot to create score by type
anime_data %>%
  filter(type!='Unknown') %>%
  mutate_at(c('type', 'source', 'genre'), list(~as.factor)) %>%
  ggplot(aes(x = score, y=type, fill = ..x..)) +
  geom_density_ridges_gradient(scale = 1.5, rel_min_height = 0.01, gradient_lwd = 1.) +
  scale_fill_viridis(name = "Score", option = "C") +
  labs(
    title = 'Score by Anime Type',
    subtitle = 'Tidy Tuesday Anime - 04/23/2019'
  ) +
  theme_ridges(font_size = 13, grid = TRUE) + theme(axis.title.y = element_blank())->plt_type

# plot to create score by source
anime_data %>%
  filter(type!='Unknown') %>%
  mutate_at(c('type', 'source', 'genre'), funs(as.factor)) %>%
  ggplot(aes(x = score, y=source, fill = ..x..)) +
  geom_density_ridges_gradient(scale = 2, rel_min_height = 0.01, gradient_lwd = 1.) +
  scale_fill_viridis(name = "Score", option = "C") +
  labs(
    title = 'Score by Anime Source',
    subtitle = 'Tidy Tuesday Anime - 04/23/2019'
  ) +
  theme_ridges(font_size = 13, grid = TRUE) + theme(axis.title.y = element_blank())->plt_source

# combine plots
final_plt<-grid.arrange(plt_type, plt_source, nrow=2, heights=c(0.75,1))

ggsave(filename = here::here("20190423/plots/score_reviews.png"), plot = final_plt)
