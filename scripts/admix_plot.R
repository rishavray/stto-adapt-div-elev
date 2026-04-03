library(tidyverse)
library(ggthemes)

plot_data = read_csv("data/admixture_combined.csv")

cluster_colors <- c("Cluster1" = "#1f9a9a",  # teal
                   "Cluster2" = "#5d4037",   # brown
                   "Cluster3" = "#d32f2f",   # red
                   "Cluster4" = "#ff6f3f")   # orange

pop_labels <- plot_data %>%
  group_by(population) %>%
  summarise(mid_point = mean(sample_index))

region_labels <- plot_data %>%
  group_by(region) %>%
  summarise(start = min(sample_index),
           end = max(sample_index),
           mid = mean(sample_index))

p <- plot_data |>
  ggplot(aes(x = sample_index, y = ancestry, fill = cluster)) +
    geom_bar(stat = "identity", width = 1, position = "stack") +
    scale_fill_manual(values = cluster_colors) +
    facet_grid(rows = vars(K), scales = "free_y", switch = "y") +
    theme_minimal() +
    theme(
      axis.text.x = element_text(angle = 45, hjust = 1),
      axis.text.y = element_blank(),
      axis.ticks = element_blank(),
      axis.title = element_blank(),
      panel.spacing = unit(0.3, "lines"),
      panel.grid = element_blank(),
      strip.text.y.left = element_text(angle = 0, hjust = 1),
      legend.position = "top"
    ) +
    scale_x_continuous(breaks = pop_labels$mid_point,
                      labels = pop_labels$population,
                      expand = c(0, 0)) +
    geom_vline(xintercept = c(region_labels$end[-nrow(region_labels)] + 0.5),
              linetype = "dashed", color = "white", size = 1)
  