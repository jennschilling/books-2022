# Books I Read in 2022
# Author: Jenn Schilling
# February 2023

#### Libraries ####

library(here)
library(tidyverse)
library(extrafont)
library(ggtext)
library(showtext)
library(janitor)
library(scales)
library(patchwork)
library(forcats)
library(ggfittext)
library(magick)
library(grid)

#### Data ####

books <- read_csv("2022_book_list.csv", lazy = FALSE) %>% clean_names()

books_edit <- books %>%
  mutate(month_finished = factor(month_finished,
                                 levels = c("January", "February", "March",
                                            "April", "May", "June",
                                            "July", "August", "September",
                                            "October", "November", "December")),
                              
         genre = ifelse(genre == "Fiction", "General Fiction", genre)) %>%
  group_by(month_finished) %>%
  mutate(run_total_pages = cumsum(pages),
         start = ifelse(row_number() == 1, 0 , lag(run_total_pages)),
         index = factor(row_number())) %>%
  ungroup() 

#### Formatting ####

font_add_google(name = "Montserrat")
showtext_auto()


font <- "Montserrat"
title_font <- "Montserrat"
font_color <- "gray10"
bcolor <- "#DFDFDF"
    
theme_set(theme_minimal(base_size = 12, base_family = font))
  
theme_update(
    panel.grid.minor = element_blank(),
    panel.grid.major = element_blank(),
    
    panel.background = element_rect(fill = bcolor, color = NA),
    plot.background = element_rect(fill = bcolor, color = NA),
    
    axis.title = element_text(size = 16, color = font_color),
    axis.text = element_text(size = 14, color = font_color),
    axis.ticks = element_line(color = font_color),
    
    axis.line = element_line(color = font_color),
    
    strip.text = element_text(size = 20, color = font_color, hjust = 0),
    
    legend.text = element_text(size = 10, color = font_color),
    legend.title = element_text(size = 10, color = font_color),
    
    plot.title.position = "plot",
    plot.title = element_markdown(size = 35, color = font_color, family = title_font, lineheight = 0.4),
    
    plot.caption.position = "plot",
    plot.caption = element_markdown(size = 14, color = font_color, hjust = 1)
)
  
genre_pal <- c("#418472", # data
               "#3A4B5B", # fantasy
               "#89B3A7", # general fiction
               "#E48F7B", # historical fiction
               "#A93343", # horror
               "#2B409F", # memoir
               "#698DA7", # mystery
               "#828284", # nonfiction
               "#C7453E", # poetry
               "#D3ACB6", # romance
               "#DF913E", # science fiction
               "#486344") # short stories
                 
  
#### Favorites by Month ####

fav_books_month <- books_edit |>
    group_by(month_finished) |>
    summarise(num_books_mon = n(),
              num_fav_mon = sum(favorite == "Yes", na.rm = TRUE),
              .groups = "drop") |>
    mutate(perc_fav = num_fav_mon / num_books_mon,
           perc_non_fav = 1 - perc_fav) |>
    pivot_longer(perc_fav:perc_non_fav) 
  
month_label <- fav_books_month |>
  distinct(month_finished, num_books_mon) |>
  mutate(month_label = paste(month_finished, "|",
                             num_books_mon, "books")) |>
  arrange(fct_rev(month_finished)) |>
  pull(month_label)

total_fav <- sum(books_edit$favorite == "Yes", na.rm = TRUE)
total_books <- nrow(books_edit)
    
ggplot(data = fav_books_month,
       mapping = aes(y = fct_rev(month_finished),
                     x = value,
                     fill = fct_rev(name))) +
  geom_col(position = "fill") +
  geom_text(data = fav_books_month |> filter(month_finished == "January" & name == "perc_fav"),
            mapping = aes(label = paste(percent(value, accuracy = 1), "favorites")),
            color = "white",
            family = font,
            hjust = 1,
            nudge_x = -0.01,
            size = 6) +
  scale_fill_manual(values = c(genre_pal[3], genre_pal[2]), 
                    guide = "none") +
  scale_x_continuous(labels = percent,
                     expand = expansion(mult = 0)) +
  scale_y_discrete(labels = month_label) +
  labs(x = "", y = "", fill = "",
       title = paste('In 2022, I had', total_fav, '<b><span style="color:#3A4B5B">favorites</span></b> out of the', 
                     total_books, 'books I<br>read.', 
                     'In January and May more than half of the<br>books 
                     I read were <b><span style="color:#3A4B5B">favorites</span></b>.'),
       caption = "Schilling Data Studio | @schillingdata") +
  theme(axis.ticks.y = element_blank(),
        axis.line.y = element_blank(),
        axis.text.y = element_text(size = 22),
        axis.line.x = element_line(size = 0.1),
        axis.ticks.x = element_line(size = 0.1),
        plot.margin = margin(20, 15, 20, 12))

ggsave(filename = "fav_books_month.png",
       width = 4,
       height = 4)
                     
#### Favorites by Genre ####

fav_books_genre <- books_edit |>
  group_by(genre) |>
  summarise(num_books_genre = n(),
            num_fav_genre = sum(favorite == "Yes", na.rm = TRUE),
            .groups = "drop") |>
  mutate(perc_fav = num_fav_genre / num_books_genre,
         perc_non_fav = 1 - perc_fav) |>
  pivot_longer(perc_fav:perc_non_fav) 

genre_label <- fav_books_genre |>
  distinct(genre, num_books_genre) |>
  mutate(genre_label = paste(genre, "|",
                             num_books_genre, "books")) |>
  arrange(num_books_genre) |>
  pull(genre_label)

total_fav <- sum(books_edit$favorite == "Yes", na.rm = TRUE)
total_books <- nrow(books_edit)

ggplot(data = fav_books_genre,
       mapping = aes(y = reorder(genre, num_books_genre),
                     x = value,
                     fill = fct_rev(name))) +
  geom_col(position = "fill") +
  geom_text(data = fav_books_genre |> filter(genre == "General Fiction" & name == "perc_fav"),
            mapping = aes(label = paste(percent(value, accuracy = 1), "favorites")),
            color = "white",
            family = font,
            hjust = 1,
            nudge_x = -0.01,
            size = 6) +
  scale_fill_manual(values = c(genre_pal[8], genre_pal[6]), 
                    guide = "none") +
  scale_x_continuous(labels = percent,
                     expand = expansion(mult = 0)) +
  scale_y_discrete(labels = genre_label) +
  labs(x = "", y = "", fill = "",
       title = paste('In 2022, I had', total_fav, '<b><span style="color:#2B409F">favorites</span></b> out of the',
                     total_books, 'books I read.',
                     '<br>I read books from 12 different genres. When I read<br>science fiction, data, 
                     short stories, and romance,<br>
                     more than half the books were <b><span style="color:#2B409F">favorites</span></b>.'),
       caption = "Schilling Data Studio | @schillingdata") +
  theme(axis.ticks.y = element_blank(),
        axis.line.y = element_blank(),
        axis.text.y = element_text(size = 22),
        axis.line.x = element_line(size = 0.1),
        axis.ticks.x = element_line(size = 0.1),
        plot.title = element_markdown(size = 32),
        plot.margin = margin(20, 15, 20, 12))

ggsave(filename = "fav_books_genre.png",
       width = 4,
       height = 4)
