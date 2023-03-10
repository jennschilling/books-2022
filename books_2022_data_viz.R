# Books I Read in 2022
# Author: Jenn Schilling
# February 2023

#### Libraries ####

library(here)
library(tidyverse)
library(extrafont)
library(ggtext)
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
         
         favorite_label = ifelse(favorite == "Yes", "★",  ""),
         
         source_label = case_when(
           source == "Purchased" ~ "•",
           source == "Library" ~ "•\n•",
           source == "Borrowed" ~ "•\n•\n•",
           source == "Gift" ~ "•\n•\n•\n•"),
         
         medium_label = case_when(
           medium == "Book" ~ "—",
           medium == "Audiobook" ~ "—\n—",
           medium == "eBook" ~ "—\n—\n—"),
         
         book_club_label = case_when(
           book_club == "Brunch Babes Reads" ~ "|",
           book_club == "Literati" ~ "||",
           !is.na(book_club) ~ "|||",
           TRUE ~ ""),
         
         label_color = ifelse(genre %in% c("Fantasy", "Memoir", "Short Stories", 
                                           "Poetry", "Horror"),
                              "#FFFFFF", "#000000"),
         
         genre = ifelse(genre == "Fiction", "General Fiction", genre)) %>%
  group_by(month_finished) %>%
  mutate(run_total_pages = cumsum(pages),
         start = ifelse(row_number() == 1, 0 , lag(run_total_pages)),
         index = factor(row_number())) %>%
  ungroup() 

symbol_legend_data <- tibble(
  type = c("Source", "Favorite", "Medium", "Book Club"),
  
  x = c(0, 1, 0, 1),
  
  y = c(1, 1, 0, 0),
  
  label = c("• Purchased\n• Library\n• Borrowed\n• Gift",
            "★ Favorite",
            "— Book\n— Audiobook\n— eBook",
            "| Brunch Babes Reads Book Club\n|| Literati Book Club\n||| Other Book Club")
)

#### Formatting ####

font <- "Franklin Gothic Book"
title_font <- "Bookman Old Style"
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
  plot.title = element_markdown(size = 30, color = font_color, family = title_font),
  
  plot.subtitle = element_markdown(size = 20, color = font_color, family = title_font, lineheight = 1.2),
  
  plot.caption.position = "plot",
  plot.caption = element_markdown(size = 10, color = font_color, hjust = 1)
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

#### Plot ####

# Book Stack
book_stack <- ggplot(data = books_edit,
       mapping = aes(x = month_finished,
                     y = pages,
                     fill = genre,
                     label = title,
                     group = fct_rev(index))) +
  geom_col(color = bcolor) +
  geom_text(mapping = aes(y = run_total_pages,
                          label = source_label,
                          color = label_color),
            nudge_y = 20,
            nudge_x = -0.4,
            vjust = 1,
            size = 6,
            lineheight = 0.25) +
  geom_text(mapping = aes(y = run_total_pages,
                          label = favorite_label,
                          color = label_color),
            nudge_x = 0.39,
            vjust = 1.1,
            size = 5,
            lineheight = 0) +
  geom_text(mapping = aes(y = start,
                          label = medium_label,
                          color = label_color),
            vjust = -0.06,
            nudge_x = -0.39,
            size = 4,
            lineheight = 0.2) +
  geom_text(mapping = aes(y = start,
                          label = book_club_label,
                          color = label_color),
            vjust = -0.37,
            nudge_x = 0.38,
            size = 4,
            lineheight = 0) +
  geom_fit_text(mapping = aes(color = label_color),
                reflow = TRUE,
                position = "stack",
                family = font,
                lineheight = 0.8) +
  scale_fill_manual(values = genre_pal) +
  scale_color_manual(values = books_edit$label_color %>% unique(.)) +
  guides(size = "none",
         fill = "none",
         color = "none") +
  coord_cartesian(expand = FALSE,
                  clip = "off") +
  theme(axis.line = element_blank(),
        axis.ticks = element_blank(),
        axis.text.y = element_blank(),
        axis.title = element_blank(),
        plot.margin = margin(t = 20, r = 20, b = 20, l = 20),
        axis.text = element_text(size = 16, color = font_color))

# Genre Bar + Legend
legend_bar <- ggplot(data = books_edit,
       mapping = aes(y = fct_rev(fct_infreq(genre)),
                     fill = genre)) +
  geom_bar() +
  geom_bar_text(contrast = TRUE,
                stat = "count", 
                mapping = aes(label = ..count..),
                family = font) +
  scale_fill_manual(values = genre_pal) +
  coord_cartesian(expand = FALSE,
                  clip = "off") +
  guides(fill = "none") +
  #labs(title = "I mostly read nonfiction and fiction books.") +
  theme(axis.line = element_blank(),
        axis.ticks = element_blank(),
        axis.text.x = element_blank(),
        axis.title = element_blank(),
        plot.margin = margin(t = 20, r = 20, b = 20, l = 20),
        plot.title = element_markdown(size = 16, color = font_color, family = font))

# Year published
year_bar <- ggplot(data = books_edit,
       mapping = aes(x = factor(year_published),
                     fill = genre)) +
  geom_bar() +
  geom_text(data = books_edit %>% count(year_published),
            mapping = aes(x = factor(year_published),
                          y = n,
                          label = n,
                          fill = NA),
            family = font,
            color = font_color,
            vjust = -0.5) +
  labs(title = "Over half the books I read were published in 2021 or 2022.") +
  scale_y_continuous(limits = c(0, 33)) +
  scale_fill_manual(values = genre_pal) +
  coord_cartesian(expand = FALSE,
                  clip = "off") +
  guides(fill = "none") +
  theme(axis.line = element_blank(),
        axis.ticks = element_blank(),
        axis.text.y = element_blank(),
        axis.title = element_blank(),
        axis.text = element_text(size = 11, color = font_color),
        plot.margin = margin(t = 20, r = 20, b = 20, l = 20),
        plot.title = element_markdown(size = 16, color = font_color, family = font))

# Symbol Legend
symbol_legend <- ggplot(data = symbol_legend_data,
                        mapping = aes(y = y,
                                      x = x,
                                      label = label)) +
  geom_text(hjust = 0,
            vjust = 1,
            lineheight = 0.8,
            size = 5,
            family = font,
            color = font_color) +
  scale_x_continuous(limits = c(0, 2.75)) +
  scale_y_continuous(limits = c(-0.5, 1)) +
  coord_cartesian(clip = "off",
                  expand = FALSE) +
  labs(title = "<i>How to read the book symbols:</i><br>") +
  theme(axis.line = element_blank(),
        axis.ticks = element_blank(),
        axis.text = element_blank(),
        axis.title = element_blank(),
        plot.margin = margin(t = 20, r = 100, b = 20, l = 20),
        plot.title = element_markdown(size = 16, color = font_color, family = font))

# Put it together

right_side <- wrap_elements(full = symbol_legend) +
  wrap_elements(full = legend_bar) +
  wrap_elements(full = year_bar) +
  plot_layout(ncol = 1,
              heights = c(1, 2, 1))

book_stack + right_side +
  plot_layout(ncol = 2,
              widths = c(3, 1.5)) +
  plot_annotation(title = "<br><b>A Year of Reading</b>",
                  subtitle = "In 2022, I read 117 books totaling 36,472 pages. 77% were written by female authors.<br>
                  Each book's height in the stack represents its length.",
                  caption = "<b>Data & Design:</b> Jenn Schilling") 

# Save
ggsave("books_2022.png",
       plot = last_plot(),
       device = "png",
       width = 24,
       height = 12)


#### Instagram ####

# Title

insta_title <- ggplot() +
  annotate("text",
           x = -12,
           y = 2,
           label = "A Year of Reading",
           family = title_font,
           color = font_color,
           fontface = "bold",
           size = 12,
           hjust = 0)  +
  annotate("text",
           x = -12,
           y = 1.5,
           label = "In 2022, I read 117 books totaling 36,472 pages.\n77% were written by female authors.\nEach book's height in the stack represents its length.",
           family = title_font,
           color = font_color,
           size = 10,
           hjust = 0)  +
  annotate("text",
           x = -3,
           y = 1,
           label = "Swipe for a closer look",
           family = font,
           color = font_color,
           size = 7,
           hjust = -1,
           vjust = 0) +
  annotate("curve",
           x = 5.5, xend = 7,
           y = 1.032, yend = 1.032,
           curvature = 0, arrow = arrow(type = "closed"),
           color = font_color, size = 2) + 
  scale_x_continuous(limits = c(-12, 8),
                     expand = c(0, 0)) +
  scale_y_continuous(limits = c(-1, 2.5)) +
  coord_cartesian(clip = "off") +
  labs(caption = "<b>Data & Design:</b> Jenn Schilling") +
  theme(axis.text= element_blank(),
        axis.line = element_blank(),
        axis.title = element_blank(),
        axis.ticks = element_blank())

ggsave("books_2022_title.png",
       plot = insta_title + 
         inset_element(book_stack, left = 0, bottom = 0, right = 1, top = 0.5),
       width = 13,
       height = 13)

# Legends
legends <- right_side <- wrap_elements(full = symbol_legend) +
  wrap_elements(full = legend_bar) +
  plot_layout(ncol = 1,
              heights = c(1, 3))

ggsave("books_2022_legend.png",
       plot = legends + 
         labs(caption = "<br><br><b>Data & Design:</b> Jenn Schilling") +
         theme(plot.caption = element_markdown(size = 10, 
                                               color = font_color, 
                                               hjust = 1)),
       width = 10,
       height = 10)

# Jan - June
ggsave("books_2022_jan_jun.png",
       plot = book_stack + 
         scale_x_discrete(limits = c("January", "February", "March", "April", "May", "June")) + 
         labs(caption = "<br><br><b>Data & Design:</b> Jenn Schilling") +
         theme(plot.caption = element_markdown(size = 10, 
                                               color = font_color, 
                                               hjust = 1)),
       width = 12,
       height = 12)

# July - Dec
ggsave("books_2022_jul_dec.png",
       plot = book_stack + 
         scale_x_discrete(limits = c("July", "August", "September", "October", "November", "December")) + 
         labs(caption = "<br><br><b>Data & Design:</b> Jenn Schilling") +
         theme(plot.caption = element_markdown(size = 10, 
                                               color = font_color, 
                                               hjust = 1)),
       width = 12,
       height = 12)
