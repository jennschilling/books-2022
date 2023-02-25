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
library(ggpol)

#### Data ####

books_22 <- read_csv("2022_book_list.csv", lazy = FALSE) %>% clean_names()

books_edit_22 <- books_22 %>%
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

books_21 <- read_csv("2021_book_list.csv", lazy = FALSE) %>% clean_names()

books_edit_21 <- books_21 %>%
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
    
    axis.title = element_text(size = 26, color = font_color),
    axis.text = element_text(size = 22, color = font_color, lineheight = 0.4),
    
    axis.ticks = element_line(color = font_color, size = 0.1),
    axis.line = element_line(color = font_color, size = 0.1),
    
    strip.text = element_text(size = 20, color = font_color, hjust = 0),
    
    legend.text = element_text(size = 10, color = font_color),
    legend.title = element_text(size = 10, color = font_color),
    
    plot.title.position = "plot",
    plot.title = element_markdown(size = 30, color = font_color, family = title_font, lineheight = 0.4),
    
    plot.caption.position = "plot",
    plot.caption = element_markdown(size = 14, color = font_color, hjust = 1),
    
    plot.margin = margin(10, 10, 10, 10)
)
  
genre_pal <- c("#418472", 
               "#3A4B5B", 
               "#89B3A7", 
               "#E48F7B", 
               "#A93343", 
               "#2B409F", 
               "#698DA7", 
               "#828284", 
               "#C7453E", 
               "#D3ACB6", 
               "#DF913E", 
               "#486344",
               "#51344D",
               "#A78682") 
                 
  
#### Compare 2021 and 2022 by Medium ####

medium_comp <- books_22 |> 
  count(medium) |> 
  mutate(total = sum(n),
         perc = n / total) |>
  select(medium, perc, n) |>
  full_join(books_21 |> 
              count(medium) |>
              mutate(total = sum(n),
                     perc = n / total) |>
              select(medium, perc, n),
            by = "medium",
            suffix = c("_22", "_21")) |>
  pivot_longer(perc_22:n_21) |>
  separate(name, into = c("variable", "year"), sep = "_") |>
  mutate(year = paste0("20", year)) |>
  group_by(year, variable) |>
  mutate(year_label = paste0(year, "\n", sum(value, na.rm = TRUE), " books")) |>
  ungroup() |>
  mutate(year_label = ifelse(variable == "perc", lead(year_label), year_label))

ggplot(data = medium_comp |>
         filter(variable == "perc"),
       mapping = aes(x = year_label,
                     y = value,
                     group = medium,
                     color = medium)) +
  geom_point(size = 1) +
  geom_line(size = 0.5) +
  geom_text(data = medium_comp |>
              filter(variable == "perc" & year == 2022),
            mapping = aes(label = medium),
            family = font,
            hjust = 0,
            nudge_x = 0.04,
            size = 6) +
  scale_color_manual(values = c(genre_pal[2], genre_pal[6], genre_pal[7]),
                     guide = "none") +
  scale_y_continuous(labels = percent) +
  labs(x = "", y = "Percent of Books",
       title = paste('Almost 30% of the books I read in 2022 were audiobooks,<br>a large increase from 2021 when 12% were audiobooks.'),
       caption = "Schilling Data Studio | @schillingdata")

ggsave(filename = "compare_medium_1.png",
       width = 4,
       height = 4)

ggplot(data = medium_comp |>
         filter(variable == "perc"),
       mapping = aes(x = year_label,
                     y = value,
                     fill = medium,
                     label = medium)) +
  geom_col(color = bcolor) +
  geom_text(position = position_stack(vjust = 0.5),
            family = font,
            color = "white",
            size = 6) +
  scale_fill_manual(values = c(genre_pal[2], genre_pal[6], genre_pal[7]),
                    guide = "none") +
  scale_y_continuous(labels = percent,
                     expand = expansion(0)) +
  labs(x = "", y = "Percent of Books",
       title = paste('Almost 30% of the books I read in 2022 were audiobooks,<br>a large increase from 2021 when 12% were audiobooks.<br>'),
       caption = "Schilling Data Studio | @schillingdata") +
  theme(axis.ticks.x = element_blank(),
        axis.line.x = element_blank())

ggsave(filename = "compare_medium_2.png",
       width = 4,
       height = 4)


#### Compare 2021 and 2022 by Genre ####

genre_comp <- books_edit_22 |> 
  count(genre) |> 
  mutate(total = sum(n),
         perc = n / total) |>
  select(genre, perc, n) |>
  full_join(books_edit_21 |> 
              count(genre) |>
              mutate(total = sum(n),
                     perc = n / total) |>
              select(genre, perc, n),
            by = "genre",
            suffix = c("_22", "_21")) |>
  pivot_longer(perc_22:n_21) |>
  separate(name, into = c("variable", "year"), sep = "_") |>
  mutate(year = paste0("20", year)) |>
  group_by(year, variable) |>
  mutate(year_label = paste0(year, "\n", sum(value, na.rm = TRUE), " books")) |>
  ungroup() |>
  mutate(year_label = ifelse(variable == "perc", lead(year_label), year_label))

genre_comp_label <- genre_comp |>
  filter(variable == "perc" &
           ((genre %in% c("General Fiction", "Nonfiction", "Mystery", "Romance", "Historical Fiction",
                         "Data", "Short Stories", "Memoir", "Science Fiction", "Horror") & year == 2022) |
              (genre %in% c("Young Adult", "Fantasy", "Poetry", "Self Help") & year == 2021)))

ggplot(data = genre_comp |>
         filter(variable == "perc"),
       mapping = aes(x = year_label,
                     y = value,
                     group = genre,
                     color = genre)) +
  geom_point(size = 1) +
  geom_line(size = 0.5) +
  geom_text(data = genre_comp_label |> filter(year == 2022),
            mapping = aes(label = genre),
            family = font,
            hjust = 0,
            nudge_x = 0.04,
            size = 6) +
  geom_text(data = genre_comp_label |> filter(year == 2021),
            mapping = aes(label = genre),
            family = font,
            hjust = 1,
            nudge_x = -0.04,
            size = 6) +
  scale_color_manual(values = genre_pal,
                     guide = "none") +
  scale_y_continuous(labels = percent) +
  labs(x = "", y = "Percent of Books",
       title = paste('In 2022, I read more General Fiction, Mystery, and Romance
                  <br>and less Nonfiction, Fantasy, and Memoir than 2021.'),
       caption = "Schilling Data Studio | @schillingdata") +
  theme(title = element_markdown(size = 28))

ggsave(filename = "compare_genre_1.png",
       width = 4.2,
       height = 4.2)

ggplot(data = genre_comp |>
         filter(variable == "perc") |>
         mutate(value = ifelse(is.na(value), 0, value)),
       mapping = aes(x = year_label,
                     y = fct_rev(genre),
                     fill = value,
                     label = percent(value, accuracy = 1))) +
  geom_tile(color = bcolor) +
  geom_text(family = font,
            color = "white",
            size = 6) +
  coord_cartesian(expand = FALSE) +
  scale_x_discrete(position = "top") +
  scale_fill_gradient(low = genre_pal[10],
                      high = genre_pal[13],
                      na.value = bcolor) +
  guides(fill = "none") +
  labs(x = "", y = "", fill = "",
       title = paste('In 2022, I read more General Fiction, Mystery, and Romance
                  <br>and less Nonfiction, Fantasy, and Memoir than 2021.'),
       subtitle = "Label is the percent of total books in each genre per year.",
       caption = "Schilling Data Studio | @schillingdata") +
  theme(title = element_markdown(size = 28),
        axis.line = element_blank(),
        axis.ticks = element_blank(),
        plot.margin = margin(10, 30, 10, 10))

ggsave(filename = "compare_genre_2.png",
       width = 4.2,
       height = 4.2)

ggplot(data = genre_comp |>
         filter(variable == "perc") |>
         mutate(value = ifelse(year == 2021, -value, value),
                value = ifelse(is.na(value), 0, value)),
       mapping = aes(x = value,
                     y = fct_rev(genre),
                     label = percent(abs(value), accuracy = 1), 
                     fill = year)) +
  geom_col() +
  geom_text(data = genre_comp |>
              filter(variable == "perc" & year == 2021) |>
              mutate(value = ifelse(year == 2021, -value, value),
                     value = ifelse(is.na(value), 0, value)),
            family = font,
            color = font_color,
            size = 6,
            hjust = 1,
            nudge_x = -0.01) +
  geom_text(data = genre_comp |>
              filter(variable == "perc" & year == 2022) |>
              mutate(value = ifelse(year == 2021, -value, value),
                     value = ifelse(is.na(value), 0, value)),
            family = font,
            color = font_color,
            size = 6,
            hjust = 0,
            nudge_x = 0.01) +
  scale_fill_manual(values = c(genre_pal[2], genre_pal[13])) +
  facet_share(~ year_label,
              scales = "free") +
  coord_cartesian(clip = "off",
                  expand = FALSE) +
  guides(fill = "none") +
  labs(x = "", y = "", fill = "",
       title = paste('In 2022, I read more General Fiction, Mystery, and Romance
                  <br>and less Nonfiction, Fantasy, and Memoir than 2021.'),
       subtitle = "Bars show the percent of annual books in each genre per year.",
       caption = "Schilling Data Studio | @schillingdata") +
  theme(title = element_markdown(size = 28),
        axis.line.x = element_blank(),
        axis.ticks = element_line(color = bcolor),
        axis.text.x = element_blank(),
        strip.text = element_text(size = 25, hjust = 0.5, lineheight = 0.4),
        plot.margin = margin(10, 75, 10, 10),
        panel.spacing = unit(0, "lines"),
        plot.caption = element_markdown(hjust = 1.45))

ggsave(filename = "compare_genre_3.png",
       width = 4.2,
       height = 4.2)


#### Compare 2021 and 2022 by Book Club ####
