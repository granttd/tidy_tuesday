library(here)
library(ggrepel)
library(tidyverse)
library(patchwork)

tv_data <- read_csv(here::here('20190108','data','IMDb_Economist_tv_ratings.csv'))

tv_data %>% glimpse()
summary(tv_data)

tv_data %>%
  filter(title == '12 Monkeys') %>%
  head(5)

# specific tv shows to note
tv_shows <- c("The Sopranos",
              "Twin Peaks",
              "Sex and the City",
              "The Wire",
              "The X-Files",
              "Breaking Bad",
              "Game of Thrones")

# specific TV show data
tv_shows_data<- tv_data %>%
  filter(title %in% tv_shows) %>%
  mutate(title = case_when(
    title == "Twin Peaks" & date> as.Date("2010-01-01") ~ "Twin Peaks\n(reboot)",
    title == "The X-Files" & date> as.Date("2010-01-01") ~ "The X-Files\n(reboot)",
    TRUE ~ title
  ))

# Make tv_data long splitting specific genres
genre_names <- tv_data %>%
  mutate(genres = str_split(genres, pattern = ",")) %>%
  unnest()

# Labels for first point of each series vs every data point
tv_labs <- tv_shows_data %>%
  group_by(title) %>%
  filter(row_number(title)==1)

# First plot
# We are trying to recreate original plot
tv_data %>%
  mutate(year = lubridate::year(date)) %>%
  ggplot(aes(x = date, y = av_rating, size = share)) +
    geom_point(color="#d7ebf2", alpha=0.8) +
    geom_smooth(method = "lm", se = F, linetype = "dashed", color = "skyblue4") +
    annotate(geom = "text", x = as.Date("2015-01-01"), y = 8.2,
             label = "TV drama trend", color = "skyblue4", fontface = "bold", size = 4) +
    geom_point(data=tv_shows_data, aes(x = date, y = av_rating, size = share), color = "#42bbd0") +
    geom_line(data=tv_shows_data, aes(x=date, y = av_rating, group = title), color = "#42bbd0",
              inherit.aes = FALSE) +
    geom_text_repel(data = tv_labs, aes(x = date, y = av_rating, label=title),
                    color = "#42bbd0", inherit.aes = FALSE, nudge_y = -0.15,
                    fontface = "bold", size = rel(5)) +
    coord_cartesian(ylim = c(5.5, 9.5)) +
    scale_y_continuous(breaks = seq(5.5, 9.5, 0.5), position = "right") +
    scale_x_date(breaks = as.Date(c("1990-01-01", "1995-01-01", "2000-01-01",
                                    "2005-01-01", "2010-01-01", "2015-01-01",
                                    "2018-01-01")),
                 date_labels = "%Y") +
    scale_size_continuous(range = c(2,10)) +
    labs(title = "The end of channel surfing\nTV's golden age is real",
         subtitle = "But for every Breaking Bad, more shows are just bad",
         caption = "*Seasons with at least 100 ratings on average\n*Size=Share of IMDb ratings for shows that year") +
    theme_minimal() +
    theme(plot.title = element_text(hjust = 0.5, size = rel(2.5)),
          plot.subtitle = element_text(hjust = 0.5, face = "italic", size = rel(1.3)),
          plot.caption = element_text(face = "italic", color = "grey60", size = rel(1)),
          axis.title = element_blank(),
          axis.text = element_text(size = rel(1.2)),
          panel.grid.minor = element_blank(),
          panel.grid.major.y = element_blank(),
          legend.position = "none")->plt_recreate

ggsave(filename = here::here("20190108/plots/plot_recreate.png"))
  
