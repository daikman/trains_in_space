library(tidyverse)
library(showtext)
library(grid)
library(ggrepel)
library(ggdark)

transit_cost <- read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-01-05/transit_cost.csv') %>% 
  janitor::clean_names() %>% 
  filter(real_cost != "MAX") %>% 
  drop_na(line)



tall_things <- tibble(name = c("Burj Khalifa", "Mount Everest", "Standard commercial flight", "Highest commercial flight", "Ozone layer/Stratosphere", "Kármán line (outer space)"),
                      height = c(0.8, 8.8, 11, 18.3, 35, 100),
                      offset = c(-0.1, 0.1, -0.1, 0.1, -0.1, 0.1),
                      end = if_else(offset < 0, -0.5, 0.5),
                      start = c(0.09, -0.09, 0.09, -0.09, 0.09, -0.5),
                      just = if_else(offset < 0, 1.1, 0))

bg <- rasterGrob(c("black", "black", "black", "black", "black", "#393752", "#393752", "#8fb8d9", "#b5f1ff", "#b5f1ff", "#b5f1ff", "#b5ffe0"), 
                width = unit(1, "npc"), height = unit(1, "npc"),
                interpolate = TRUE)


line_colours <- c("#ffe600", rep("#f5c33b", 6))
tall_colours <- c("black", "black", "black", "black", "black", "white")

font_add_google("Josefin Sans", "custom_family")
showtext_auto()


transit_cost %>% 
  filter(length > 100) %>% 
  arrange(desc(length)) %>% 
  mutate(offset = seq(-0.08, 0.08, length.out = nrow(.))) %>% 
  ggplot(aes(y = length, x = offset))  +
  annotation_custom(bg, xmin=-Inf, xmax=Inf, ymin=-Inf, ymax=Inf) +
  geom_hline(yintercept = seq(125, 200, 25), color = "grey") +
  geom_segment(aes(yend = 0, xend = offset), 
               size = 3,
               alpha = 0.7,
               color = line_colours) +
  geom_label_repel(aes(label = paste0(line, ", ", city), 
                y = length),
                vjust = 0,
                hjust = c(1.5, 1.5, 1.5, -0.5, -0.5, -0.5, -0.5),
                family = "custom_family",
                size = 6) +
  geom_point(color = line_colours, 
             size = 4) +
  geom_segment(data = tall_things, 
               aes(y = height, 
                   yend = height, 
                   x = start, 
                   xend = end),
               color = tall_colours) +
  geom_text(data = tall_things, 
            aes(y = height, x = offset + 0.01, 
                label = paste0(name, ", ", height, "km"), 
                hjust = just,
                vjust = -0.1),
            family = "custom_family",
            size = 6,
            color = tall_colours) +
  labs(title = "Trains in space!",
       subtitle = "The length of (mostly upcoming) public metropolitan transit railways compared to the world's greatest heights",
       caption = "David Aikman @ResearchingDave, Data: Transit Costs Project (via TidyTuesday)",
       x = "",
       y = "Length or height") +
  scale_y_continuous(labels = scales::unit_format(unit = "km")) +
  dark_theme_minimal(base_family = "custom_family",
                base_size = 20) +
  theme(plot.margin = margin(b = 1, l = 10, r = 10, t = 10),
        plot.title = element_text(size = 26, face = "bold"),
        axis.text.x = element_blank(),
        plot.caption = element_text(size = 12),
        plot.subtitle = element_text(size = 16, face = "italic"),
        panel.grid.minor.x = element_blank(),
        panel.grid.major.x = element_blank(),
        legend.position = "none") +
  xlim(c(-0.5, 0.5))

