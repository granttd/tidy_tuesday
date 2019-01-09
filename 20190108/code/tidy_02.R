library(here)
library(ggrepel)
library(tidyverse)

tv_data <- read_csv(here::here('20190108','data','IMDb_Economist_tv_ratings.csv'))

# Function to identify list of shows that meet criteria of being in top n 
# for each year periods for at least a certain number of appearances 
top_show_list_filter <- function(top_n_over_time, x_show_appearances){
  top_show_list<- tv_data %>%
    mutate(year = lubridate::year(date)) %>% 
    group_by(year) %>%
    arrange(-av_rating) %>%
    filter(row_number(year)<top_n_over_time) %>%
    group_by(title) %>%
    filter(row_number(title)==x_show_appearances) %>%
    pull(title)
  
  return(top_show_list)
}

# Additional wrapper to collect specific data for the top shows
top_show_data_filter <- function(top_n_over_time, x_show_appearances){
  top_list<-top_show_list_filter(top_n_over_time=top_n_over_time, 
                                 x_show_appearances = x_show_appearances)
  
  top_tv_shows_data<- tv_data %>%
    mutate(year = lubridate::year(date)) %>% 
    group_by(year) %>%
    arrange(-av_rating) %>%
    filter(row_number(year)<top_n_over_time) %>%
    filter(title %in% top_list)
  
  return(top_tv_shows_data)
}

# Combination of some of the original Economist plot with top 10 called out.

plot_top_show_data_filter <- function(top_n, appearances, optional_title = NA){
  
  # get shows that have been in the top 10 for a given year at least 5 times
  list_top_n_x_years<-top_show_list_filter(top_n_over_time = top_n, x_show_appearances = appearances)

  # data for shows when they were in top 10 for the years 5 times inclding non-top years
  all_data_top_n_x_years<-tv_data %>%
    filter(title %in% list_top_n_x_years)
  
  # filter top data for mapping labels
  top_labs_x<- all_data_top_n_x_years %>%
    group_by(title) %>%
    arrange(-av_rating) %>%
    filter(row_number(title)==1)
  
  mod_title<-if_else(is.na(optional_title), "Survival of the Fittest", paste0("Survival of the Fittest: ", optional_title))
  
  # create plot with specified dimensions of top n and x show appearances
  tv_data %>%
    mutate(year = lubridate::year(date)) %>%
    ggplot(aes(x = date, y = av_rating, size = share)) +
    geom_point(color="#d7ebf2", alpha=0.8, show.legend = FALSE) +
    geom_smooth(method = "lm", se = F, linetype = "dashed", color = "skyblue4") +
    geom_point(data=all_data_top_n_x_years, aes(x = date, y = av_rating, size = share,
                                                 fill = factor(title)), alpha = .7, shape = 21) +
    geom_line(data=all_data_top_n_x_years, aes(x=date, y = av_rating, group = title,
                                                color = factor(title)), size = 1, linetype="longdash",
                                                inherit.aes = FALSE, show.legend=FALSE) +
    geom_text_repel(data = top_labs_x, aes(x = date, y = av_rating, label=title),
                    color = "#000000", inherit.aes = FALSE, nudge_y = 0.25,
                    fontface = "bold", size = rel(3)) +
    coord_cartesian(ylim = c(5.5, 9.5)) +
    scale_y_continuous(breaks = seq(5.5, 9.5, 0.5), position = "right") +
    scale_x_date(breaks = as.Date(c("1990-01-01", "1995-01-01", "2000-01-01",
                                    "2005-01-01", "2010-01-01", "2015-01-01",
                                    "2018-01-01")),
                 date_labels = "%Y") +
    scale_size_continuous(range = c(2,10)) +
    labs(title = mod_title,
         subtitle = paste0("Shows in top ", top_n, " for at least ", appearances, " unique years"),
         caption = "*Seasons with at least 100 ratings on average\n*Size=Share of IMDb ratings for shows that year") +
    theme_minimal() +
    theme(plot.title = element_text(hjust = 0.5, size = rel(2.5)),
          plot.subtitle = element_text(hjust = 0.5, face = "italic", size = rel(1.3)),
          plot.caption = element_text(face = "italic", color = "grey60", size = rel(1)),
          axis.title = element_blank(),
          axis.text = element_text(size = rel(1.2)),
          panel.grid.minor = element_blank(),
          panel.grid.major.y = element_blank(),
          legend.position="bottom") +
          guides(size=FALSE) +
          guides(fill=guide_legend(title = "TV Show Titles", override.aes = list(size=4)))->plt_output
  
  return(plt_output)

}

plt_test_of_time<-plot_top_show_data_filter(top_n = 20, appearances = 10, optional_title = 'Test of Time')
plt_quality_and_time<-plot_top_show_data_filter(top_n = 5, appearances = 5, optional_title = 'Quality and Time')
plt_trifecta<-plot_top_show_data_filter(top_n = 3, appearances = 3, optional_title = "Trifecta")

ggsave(plt_test_of_time, filename = here::here("20190108/plots/plot_test_of_time.png"))
ggsave(plt_quality_and_time, filename = here::here("20190108/plots/plot_quality_and_time.png"))
ggsave(plt_trifecta, filename = here::here("20190108/plots/plot_trifecta.png"))
