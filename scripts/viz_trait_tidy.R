library(ellipse)
library(tidyverse)
library(ggthemes)
# Helper function to create ellipse data
create_ellipse_data <- function(center, sigma_matrix, level = 0.95, n_points = 100) {
  ellipse_points = ellipse(sigma_matrix, centre = center, level = level, npoints = n_points)
  tibble(
    x = ellipse_points[, 1],
    y = ellipse_points[, 2]
  )
}
viz_traits_tidy = function(fixedpost, popefpost, Gpost, THpost, traits, 
                           size_param = 0.5, plot_title = NULL, 
                           population_labels = unique(pops),
                           trait_names = c("Trait 1", "Trait 2")) {
  
  # Input validation
  if (length(traits) != 2) {
    stop("trait_indices must contain exactly 2 elements")
  }
  
  i = 1
  j = 2
  
  # Calculate posterior means using tidyverse approach
  # Ancestral means
  mu = fixedpost[1, traits, ] %>%
    apply(1, mean) %>%
    set_names(paste0("trait_", traits))
  
  # Population effects
  popef = popefpost[, traits, ] %>%
    apply(c(1, 2), mean) %>%
    as_tibble(.name_repair = ~paste0("trait_", traits)) %>%
    mutate(population = population_labels)
  
  # G matrix (genetic variance-covariance)
  G = Gpost[traits, traits, ] %>%
    apply(c(1, 2), mean)
  
  # Theta matrix (environmental variance)
  TH = THpost %>%
    apply(c(1, 2), mean)
  
  # Extract values for the two traits of interest
  ei = mu[i]
  ej = mu[j]
  
  # Create G matrix for the two traits
  G_subset = matrix(c(G[i, i], G[i, j], G[j, i], G[j, j]), ncol = 2)
  
  # Calculate plot limits
  max_range = max(c(
    sqrt(2 * G[i, i] * diag(TH)),
    sqrt(2 * G[j, j] * diag(TH)),
    abs(as.matrix(popef[, 1:length(traits)]))
  ))
  
  # Create color palette. Implemented as it is from driftsel package.
  # Change the logic as necessary and keep the colours consistent across plots
  n_pop = nrow(popef)
  colors = c("#000000", "#FF0000", "#00FF00", "#0000FF", "#FF00FF", 
              "#00FFFF", "#FFA500", "#800080", "#7FFF00")[1:min(n_pop, 9)]
  if (n_pop > 9) {
    colors = rep(colors, ceiling(n_pop / 9))[1:n_pop]
  }
  
  # Create ellipse data for each population
  ellipse_data = map_dfr(1:n_pop, ~{
    mu_pop = c(ei, ej)
    sigma_pop = 2 * TH[.x, .x] * G_subset
    
    create_ellipse_data(mu_pop, sigma_pop, level = size_param) %>%
      mutate(
        population = .x,
        color = colors[.x]
      )
  })
  
  # Create population deviation data
  pop_deviations = popef %>%
    mutate(
      x = ei + get(paste0("trait_", traits[i])),
      y = ej + get(paste0("trait_", traits[j])),
      color = colors[row_number()],
      label = as.character(population)
    )
  
  # Create the plot
  p = ggplot() +
    # Add ellipses
    geom_polygon(data = ellipse_data, 
                 aes(x = x, y = y, group = population),
                 alpha = 0.3, 
                 fill = "transparent",
                 color = ellipse_data$color) +
    
    # Add population labels
    geom_text(data = pop_deviations,
              aes(x = x, y = y, label = label),
              color = pop_deviations$color,
              size = 4) +
    
    # Add ancestral point
    geom_point(aes(x = ei, y = ej), 
               size = 3, color = "black") +
    geom_text(aes(x = ei, y = ej, label = "A"),
              vjust = -1, size = 4, fontface = "bold") +
    
    # Formatting
    coord_fixed() +
    #xlim(ei - max_range, ei + max_range) +
    #ylim(ej - max_range, ej + max_range) +
    labs(
      x = paste(trait_names[traits[1]]),
      y = paste(trait_names[traits[2]]),
      title = plot_title
    ) +
    theme_bw() +
    theme(
      panel.grid.minor = element_blank(),
      plot.title = element_text(hjust = 0.5, size = 14),
      axis.title = element_text(size = 12),
      axis.text = element_text(size = 10)
    )
  
  return(p)
}