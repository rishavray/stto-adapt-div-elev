library(mixOmics)
library(tidyverse)

# Load data ----
morph = read_csv("data/qpc_morph_9.6.24.csv")
climate_summary = read_csv("data/climate_summary.csv")

# morphological traits removed: number of branches (correlated w/no lvs)
# climate metrics removed: tmin (correlated with tmx), pck (correlated w/ppt)

# Prep data for analysis ----
morph_climate = morph |>
  dplyr::select(ID, pop, site, lf_thickness:postvern_no_lvs, PC1, PC2, sla) |>
  # join climate and morphological dataframes
  left_join(climate_summary) |>
  # remove NA's (needed for cancor to run)
  na.exclude()

morph_climate_for_cca = morph_climate |>
  # remove non-numerical data for dataframe used as input to cca
  dplyr::select(-c(PC1, PC2, ID, pop, site))

morph_for_cca = morph_climate_for_cca |>
  dplyr::select(lf_thickness:sla) |>
  # scale the data
scale()

climate_for_cca = morph_climate_for_cca |>
  dplyr::select(el, cwd, str, tmx, ppt, season_length) %>%
  # scale the data
scale()

# define input matrices as dataframes
morph_for_cca_df = data.frame(morph_for_cca)
climate_for_cca_df = data.frame(climate_for_cca)

# Run CCA ----

result.cca = rcc(morphology, climate) # run the CCA method

# plot projection into canonical variate subspace
plotIndiv(result.cca) 
# plot original variables' correlation with canonical variates
vardata = plotVar(result.cca)

# Use the var data to make the custom plot
# Change the colour pallete as needed
ggplot(vardata, aes(x = x, y = y, colour = col, label = names))+
#geom_point(size = 2)+
geom_segment(aes(x = 0, y = 0, xend = x, yend = y),
  arrow = arrow(length = unit(0.2, "cm")), linewidth = 0.7) +
geom_text_repel(size = 6)+
scale_colour_identity()+
theme_doc()+
labs(x = "Canonical Variate 1", y = "Canonical Variate 2", title = "Canonical Correlation Analysis")
ggsave("cca_biplot_mixomics.pdf", width = 8, height = 8)
