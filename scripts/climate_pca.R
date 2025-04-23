# Pop gen
# Climate PCA
# Elena Suglia
# Created 5/5/23

# Libraries ----
library(tidyverse)
library(ggrepel)
library(plotly)
library(cowplot)
library(ggfortify)
library(vegan)
library(cluster)
library(factoextra)
library(broom)
library(corrplot)
library(dplyr)

# Load data ----
site_info = read.csv("data/site_info.csv") %>%
  dplyr::select(pop, el)
season_length = read.csv("data/season_length.csv") %>%
  dplyr::select(pop, season_length)

clim = read.csv("data/all_hist_clim_wider.csv") %>%
  filter(year > 1989) %>%
  filter(pop != "LVTR1" & pop != "LVTR1.5" & pop != "LVTR1.75" & pop != "LVTR2.5" & pop != "LVTR2.25" & pop != "LVTR3") %>%
  # using LVTR2 as the representative population for LVTR
  mutate(pop = str_replace_all(pop, "LVTR2", "LVTR")) %>%
  dplyr::select(-X)

# Fig: Climate PCA ----
# quick visual scan for missing data from 1990-2010
clim %>%
  ggplot(aes(month_year, cwd)) + # looked at each variable, one at a time
  geom_point() +
  facet_wrap(~pop, ncol = 1)

# Create PCA ----
climate_summary = clim %>% 
  mutate(pck = abs(pck)) %>% 
  select(-tmn) %>%
  group_by(pop) %>% 
  dplyr::summarize(cwd = sum(cwd), ppt = sum(ppt), pck = sum(pck), str = sum(str), tmx = mean(tmx)) %>% # tmn = mean(tmn), 
  # combine with elevation and season length data
  left_join(site_info) %>%
  left_join(season_length) %>%
  rename("Climatic water deficit" = "cwd") %>%
  rename("Precipitation" = "ppt") %>%
  rename("Snowpack" = "pck") %>%
  rename("Soil water storage" = "str") %>%
  rename("Maximum temperature" = "tmx") %>%
  #rename("Minimum temperature" = "tmn") %>%
  rename("Elevation" = "el") %>%
  rename("Growing season length" = "season_length")

climate_for_pc = climate_summary %>%
  dplyr::select(-pop)

summary(climate_for_pc)

pc = prcomp(climate_for_pc, scale = TRUE)

pc_data = data.frame(pc$x)

locs_pc = cbind(climate_summary, pc_data) %>%
  left_join(site_info)

loadings = data.frame(varnames=rownames(pc$rotation), pc$rotation)

#write.csv(climate_summary, "data/climate_summary.csv")
#write.csv(locs_pc, "data/locs_pc_el_gs.csv")

# look at how much variation axes explain
loadings
#write.csv(loadings, "data/climate_pc_loadings.csv")
# screeplot to get a sense for number of dimensions of var (often look for a bend, like we see at axis 2)
plot(pc, type = "lines")

pcvarexplained = tibble(var_explained = (pc$sdev^2) / (sum(pc$sdev^2))) %>%
  mutate(pcnum = seq(1,length(var_explained), 1)) %>%
  mutate(cumvarexplained = cumsum(var_explained))
pcvarexplained
# 2 axes seems sufficient

# clustering analysis
km = kmeans(climate_for_pc, center = 3, iter.max = 100, nstart = 5) # play around with cluster number (center); three seems to make the most sense
str(km)
print(km)

fviz_cluster(km, data = climate_for_pc)

# which variables are correlated?
M = cor(climate_for_pc)
plot(M)
corrplot(M)
corrplot(M, method = "number", order = "AOE")
corrplot(M, order = "AOE")

# describe correlations here

# Plot PCA ----
# color by elevation
# add textgrob
require(grid)
text_x_low = textGrob("Warm,\nlong seasons", gp=gpar(fontsize = 14,col="gray35"))
text_x_high = textGrob("Short,\ncool seasons", gp=gpar(fontsize = 14,col="gray35"))
text_y_low = textGrob("Wet", gp=gpar(fontsize = 14,col="gray35"))
text_y_high = textGrob("Dry", gp=gpar(fontsize = 14,col="gray35"))

p_el = autoplot(pc, data = locs_pc, loadings = TRUE, loadings.label = TRUE, loadings.colour = "grey", loadings.label.colour = "black", loadings.label.size = 5, colour = "el", size = 6) +
  #geom_point(aes(group = pop)) +
  scale_color_continuous(low = "orange", high = "blue",name='Elevation (m)') +
  theme_classic() +
  #geom_label(aes(label = pop), nudge_x = 0.025) # + nudge_x = -0.05, nudge_y = -0.06
  geom_text_repel(aes(x = PC1,y = PC2, label = pop),box.padding=.9) +
  #geom_label_repel(aes(x = PC1,y = PC2, label = pop),box.padding=.9) +
theme(legend.title = element_text(size=13),
      legend.text = element_text(size = 13),
      axis.text = element_text(size = 13),
      axis.title = element_text(size = 14),
      plot.title = element_text(size = 16),
      legend.position = c(0.9, 0.9),
      plot.margin = margin(1, 0, 1.5, 1, "cm")) + # bottom, left, top, right
  #xlim(-2,3.2) +
  annotation_custom(text_x_high, xmin=0.3,xmax=0.3,ymin=-0.7,ymax=-0.7) + #here I just fiddled with coordinates to get titles where we want them  # 
  annotation_custom(text_x_low, xmin=-0.3,xmax=-0.3,ymin=-0.7,ymax=-0.7) + 
  annotation_custom(text_y_high, xmin=-0.45,xmax=-0.45,ymin=0.35,ymax=0.35) + #here I just fiddled with coordinates to get titles where we want them  # 
  annotation_custom(text_y_low, xmin=-0.45,xmax=-0.45,ymin=-0.35,ymax=-0.35) + 
  coord_cartesian(clip="off") #this keeps it from clipping off the stuff outside the plot
plot(p_el)

ggsave("figures/hist_clim_pca_loadings_nov23.png", width = 8.5, height = 8)


#p_el = autoplot(pc, data = locs_pc, loadings = TRUE, loadings.label = TRUE, loadings.colour = "grey", loadings.label.colour = "black", colour = "el", size = 4.5) +
  #scale_x_reverse() + # inverting axes to match previous graphs
  #scale_y_reverse() + # inverting axes to match previous graphs
  #geom_label(aes(color = el, label = pop), nudge_x = 0.07, size = 3) + # + nudge_x = -0.05, nudge_y = -0.06
  # theme_classic() +
  # scale_color_continuous(low = "orange", high = "blue",
  #                        name ="Elevation") +
  # #xlab("PC1") +
  # #ylab("First flowering (DOY)") +
  # theme(legend.title = element_text(size=10),
  #       legend.text = element_text(size = 10),
  #       axis.text = element_text(size = 10),
  #       axis.title = element_text(size = 12),
  #       plot.title = element_text(size = 16),
  #       legend.position = c(0.88, 0.85),
  #       plot.margin = margin(1, 0, 1.5, 1, "cm")) + # bottom, left, top, right
  # #xlim(-2,3.2) +
  # annotation_custom(text_x_high, xmin=0.35,xmax=0.35,ymin=-0.55,ymax=-0.55) + #here I just fiddled with coordinates to get titles where we want them  # 
  # annotation_custom(text_x_low, xmin=-0.35,xmax=-0.35,ymin=-0.55,ymax=-0.55) + 
  # annotation_custom(text_y_high, xmin=-0.6,xmax=-0.6,ymin=0.6,ymax=0.6) + #here I just fiddled with coordinates to get titles where we want them  # 
  # annotation_custom(text_y_low, xmin=-0.6,xmax=-0.6,ymin=-0.35,ymax=-0.35) + 
  # coord_cartesian(clip="off") #this keeps it from clipping off the stuff outside the plot

#p_el


# color by cwd
p_cwd = autoplot(pc, data = locs_pc, loadings = TRUE, loadings.label = TRUE, loadings.colour = "grey", loadings.label.colour = "black", colour = "cwd", size = 6) +
  #geom_point(aes(group = pop)) +
  scale_color_continuous(low = "blue", high = "red") +
  theme_classic() +
  #geom_label(aes(label = pop), nudge_x = 0.07) # + nudge_x = -0.05, nudge_y = -0.06
  geom_text_repel(aes(x = PC1,y = PC2, label = pop),box.padding=.9)
plot(p_cwd)

p_str = autoplot(pc, data = locs_pc, loadings = TRUE, loadings.label = TRUE, loadings.colour = "grey", loadings.label.colour = "black", colour = "str", size = 6) +
  #geom_point(aes(group = pop)) +
  scale_color_continuous(low = "darkgreen", high = "violet") +
  theme_classic() +
  geom_label(aes(label = pop), nudge_x = 0.07) # + nudge_x = -0.05, nudge_y = -0.06
# geom_text_repel(aes(x = PC1,y = PC2, label = pop),box.padding=.9)
plot(p_str)

