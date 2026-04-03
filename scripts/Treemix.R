################## Plot TReemix #######################

library(ape)
library(phytools)
library(ggtree)
library(tidyverse)
library(viridis)
setwd("elena_data")

# Read tree file (Newick format)
tree <- read.tree("data/treemix.tre")

# Read metadata file
# Assumes columns: population, elevation, (other variables...)
metadata <- read_tsv("merged_traits.csv") |>
dplyr::select(population = `pop.x`, elevation = el) |>
distinct()

# Add outlier manually
outlier_pop <- "S.div"  # Replace with actual name
outlier_row <- data.frame(
  population = outlier_pop,
  elevation = NA
)
metadata <- rbind(metadata, outlier_row)


# Reorder metadata to match tree tip order
metadata <- metadata[match(tree$tip.label, metadata$population), ]
metadata$is_outlier <- is.na(metadata$elevation)

metadata$elevation_plot <- metadata$elevation
metadata$elevation_plot[metadata$is_outlier] <- min(metadata$elevation, na.rm=TRUE)


tree_data <- fortify(tree)

# Merge with metadata
tree_data <- tree_data %>%
  left_join(metadata, by=c("label"="population"))


# Create plot
p <- ggtree(tree, layout="rectangular", branch.length="branch.length") +
  theme_tree2()

# Add tip labels with conditional coloring
p <- p + 
  geom_tiplab(data = tree_data %>% filter(!is_outlier),
              aes(label=label, color=elevation),
              size=4, 
              hjust=-0.1) +
  geom_tiplab(data = tree_data %>% filter(is_outlier),
              aes(label=label),
              color="black",
              size=4, 
              hjust=-0.1) +
  scale_color_viridis(name="Elevation (m)", 
                     option="viridis",
                     na.value="black", guide = guide_colorbar(
                            direction = "horizontal",
                            barwidth = 30,  # Makes the bar longer
                            barheight = 1,  # Makes the bar thinner
                            title.position = "top",
                            title.hjust = 0.5)) +
  xlim(0, max(node.depth.edgelength(tree)) * 1.25) +
  labs(x="Drift parameter") +
  theme(axis.text.x = element_text(size=10),
        axis.title.x = element_text(size=12),
        legend.position = "top",
        legend.title = element_text(size=11, face="bold"),
        legend.text = element_text(size=9))

print(p)