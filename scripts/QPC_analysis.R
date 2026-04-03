# STTO Pop gen chapter
# Qpc analysis - adaptive differentiation using quaint method
# All populations together (Table 1)
# Rishav Ray
# Created 04/03/26
#
# PURPOSE: Run Qpc analysis and export all results for downstream plotting.
# This script is compute-intensive and only needs to be re-run when input
# data or analysis parameters change. All outputs are saved to `outputs/`
# and loaded by qpc_plot.R for visualization.
#
# USAGE:
#
# OUTPUTS (might vary in the repo, set it accordinly):
#   outputs/myK_allpops.rds          - kinship matrix (all individuals)
#   outputs/myEig_allpops.rds        - eigen decomposition (all individuals)
#   outputs/myK_sla.rds              - kinship matrix (SLA subset)
#   outputs/myEig_sla.rds            - eigen decomposition (SLA subset)
#   outputs/qpc_results.rds          - named list of Qpc outputs per trait
#   outputs/qpc_ci.rds               - named list of confidence interval vectors
#   outputs/traits_merged.tsv        - joined genotype + phenotype table
#   outputs/sla_traits.tsv           - SLA subset trait table
#   outputs/qpc_pvals_wide.tsv       - p-value matrix (traits x PCs)
#   outputs/qpc_summary.tsv          - tidy table of all Qpc p-values

## Libraries ----
library(tidyverse)
library(vcfR)
library(adegenet)
setwd("elena_data")

## Paths set it accordingly----
data_dir   <- "data"
vcf_path   <- file.path(data_dir, "stto_filtered.vcf.gz")
traits_path <- file.path(data_dir, "merged_traits.csv")
out_dir    <- "data"
dir.create(out_dir, showWarnings = FALSE)

## Parameters ----
# PCs used to test for selection (foreground)
test_pcs <- 1:10
# PCs used to estimate Va (background); adjust upper bound to n_individuals - 1
background_pcs <- 11:188

## Functions ----

make_k <- function(myG) {
  scaleFactor <- sqrt(mean(colMeans(myG) * (1 - colMeans(myG))))
  myM  <- dim(myG)[1]
  myT  <- matrix(data = -1/myM, nrow = myM - 1, ncol = myM)
  diag(myT) <- (myM - 1)/myM
  myGstand <- (myT %*% myG) / scaleFactor
  myK <- cov(t(myGstand))
  return(myK)
}

calcQpc <- function(myZ, myU, myLambdas, myL, myM) {
  myZ   <- myZ[1:dim(myU)[1]] - mean(myZ)
  myCmM <- (myZ %*% myU[, myM]) / sqrt(myLambdas[myM])
  myCmL <- (myZ %*% myU[, myL]) / sqrt(myLambdas[myL])
  myQm  <- sapply(myM, function(n) { var0(myCmM[n]) / var0(myCmL) })
  myPs  <- sapply(myM, function(x) { pf(myQm[x], 1, length(myL), lower.tail = FALSE) })
  list(cm = myCmM, cml = myCmL, qm = myQm, pvals = myPs)
}

var0 <- function(x) {
  sum(x^2) / length(x)
}

## 1. Load genotype data ----

snps_vcf <- read.vcfR(vcf_path)
# 189 individuals total

cols <- colnames(snps_vcf@gt)
gt_metadata <- tibble(cols = cols[2:length(cols)]) |>
separate_wider_delim(cols, delim = "_", names = c("Population", "tube_label"))

# Convert VCF to genlight
snps_gl <- vcfR2genlight(snps_vcf)
ploidy(snps_gl) <- 2

## 2. Load and join phenotype data ----

traits <- read.table(traits_path, head = TRUE, sep = "\t")
traits_merged <- left_join(gt_metadata, traits)
write_tsv(traits_merged, file.path(out_dir, "traits_merged.tsv"))

## 3. Kinship matrix and eigen decomposition — all individuals ----

myK <- make_k(as.matrix(snps_gl))
saveRDS(myK, file.path(out_dir, "myK_allpops.rds"))

myEig <- eigen(myK)
saveRDS(myEig, file.path(out_dir, "myEig_allpops.rds"))

## 4. Qpc per trait — full sample set ----

run_qpc <- function(trait_vec, eig = myEig,
                    m = test_pcs, l = background_pcs) {
  calcQpc(
    myZ      = trait_vec,
    myU      = eig$vectors,
    myLambdas = eig$values,
    myM      = m,
    myL      = l
  )
}

qpc_results <- list(
  lf_thickness     = run_qpc(traits_merged$lf_thickness),
  height           = run_qpc(traits_merged$postvern_height),
  stem_diam        = run_qpc(traits_merged$postvern_stem_diam),
  no_lvs           = run_qpc(traits_merged$postvern_no_lvs),
  lngst_lf         = run_qpc(traits_merged$postvern_lngst_lf)
)

## 5. Confidence intervals ----

qpc_ci <- lapply(qpc_results, function(res) {
  vaest <- var0(res$cml)
  1.96 * sqrt(vaest * myEig$values)
})

## 6. Kinship matrix and eigen decomposition — SLA subset ----
# SLA is measured on a subset of individuals; build a separate K.

sla_traits <- traits_merged |>
  dplyr::select(Population, tube_label, sla) |>
  na.omit()

write_tsv(sla_traits, file.path(out_dir, "sla_traits.tsv"))

samples_sla <- sla_traits |>
  mutate(merged_label = paste0(Population, "_", tube_label)) |>
  pull(merged_label)

myK_sla  <- make_k(as.matrix(snps_gl)[samples_sla, ])
saveRDS(myK_sla, file.path(out_dir, "myK_sla.rds"))

myEig_sla <- eigen(myK_sla)
saveRDS(myEig_sla, file.path(out_dir, "myEig_sla.rds"))

n_sla <- nrow(sla_traits)
# Background PCs for SLA subset — adjust upper bound to n_sla - 1
background_pcs_sla <- 11:(n_sla - 1)

myQpc_sla <- calcQpc(
  myZ       = sla_traits$sla,
  myU       = myEig_sla$vectors,
  myLambdas = myEig_sla$values,
  myM       = test_pcs,
  myL       = background_pcs_sla
)

qpc_results[["sla"]] <- myQpc_sla
qpc_ci[["sla"]]      <- 1.96 * sqrt(var0(myQpc_sla$cml) * myEig_sla$values)

## 7. Save consolidated Qpc outputs ----

saveRDS(qpc_results, file.path(out_dir, "qpc_results.rds"))
saveRDS(qpc_ci,      file.path(out_dir, "qpc_ci.rds"))

## 8. Export tidy p-value tables ----

pval_long <- map_dfr(names(qpc_results), function(trait) {
  tibble(
    trait = trait,
    pval  = as.numeric(qpc_results[[trait]]$pvals)
  )
}) |>
group_by(trait) |>
mutate(PC = 1:10)

write_tsv(pval_long, file.path(out_dir, "qpc_summary.tsv"))

pval_wide <- pval_long |>
  pivot_wider(names_from = PC, names_prefix = "PC", values_from = pval)

write_tsv(pval_wide, file.path(out_dir, "qpc_pvals_wide.tsv"))

## 9. Print significance summary ----

cat("\n=== Qpc significance summary (Bonferroni α = 0.05) ===\n")
pval_long |>
  group_by(trait) |>
  mutate(threshold = 0.05) |>
  filter(pval < threshold) |>
  dplyr::select(trait, PC, pval) |>
  print(n = Inf)
cat("======================================================\n\n")


## Libraries ----
library(tidyverse)
library(ggrepel)
library(patchwork)
library(viridis)
library(cowplot)

## Paths ----
out_dir  <- "outputs"
figs_dir <- "figs"
dir.create(figs_dir, showWarnings = FALSE)

## Load pre-computed results ----

myEig         <- readRDS(file.path(out_dir, "myEig_allpops.rds"))
myEig_sla     <- readRDS(file.path(out_dir, "myEig_sla.rds"))
qpc_results   <- readRDS(file.path(out_dir, "qpc_results.rds"))
qpc_ci        <- readRDS(file.path(out_dir, "qpc_ci.rds"))
traits_merged <- read_tsv(file.path(out_dir, "traits_merged.tsv"), show_col_types = FALSE)
sla_traits    <- read_tsv(file.path(out_dir, "sla_traits.tsv"), show_col_types = FALSE)
pval_long     <- read_tsv(file.path(out_dir, "qpc_summary.tsv"), show_col_types = FALSE)

## Population color palette (alphabetical) ----
pop_colors <- c(
  "BH"   = "#FB9D29",
  "CP2"  = "#9E36C7",
  "DPR"  = "#E87F64",
  "IH"   = "#FDA11D",
  "KC2"  = "#EB845D",
  "LV"   = "#771EE2",
  "SHA"  = "#DB6F7C",
  "SQ"   = "#B84AAE",
  "TM2"  = "#FFA500",
  "WL"   = "#CB5C96",
  "WV"   = "#F28F49",
  "YO"   = "#A73CBF"
)

## Helper ----

var0 <- function(x) {
  sum(x^2) / length(x)
}

## Theme ----

theme_qpc <- function(base_size = 12) {
  theme_classic(base_size = base_size) +
    theme(
      plot.title    = element_text(face = "bold", hjust = 0.5, size = rel(1.3)),
      plot.subtitle = element_text(hjust = 0.5, color = "gray40", size = rel(1.1)),
      axis.title    = element_text(face = "bold", size = rel(1.2)),
      axis.text     = element_text(size = rel(1.1)),
      legend.position = "right",
      legend.title  = element_text(face = "bold"),
      panel.border  = element_rect(fill = NA, color = "gray30", linewidth = 0.8)
    )
}

## ===========================================================================
## Plot functions
## ===========================================================================

# ---------------------------------------------------------------------------
# 1. Manhattan-style: -log10(p) for all traits across PCs
# ---------------------------------------------------------------------------
# qpc_results_list : named list; each element has $pvals
# fdr_threshold    : significance level before Bonferroni correction

plot_qpc_manhattan <- function(qpc_results_list, fdr_threshold = 0.05) {

  plot_data <- map_dfr(names(qpc_results_list), function(trait) {
    pvals  <- qpc_results_list[[trait]]$pvals
    n_pcs  <- length(pvals)
    bonf   <- fdr_threshold / n_pcs
    tibble(
      trait       = trait,
      PC          = 1:n_pcs,
      pval        = pvals,
      log10p      = -log10(pvals),
      significant = pvals < bonf
    )
  }) |>
    mutate(trait = factor(trait, levels = unique(trait)))

  bonf_line <- -log10(fdr_threshold)

  ggplot(plot_data, aes(x = PC, y = log10p, color = trait, group = trait)) +
    geom_hline(yintercept = bonf_line,
               linetype = "dashed", color = "#FF5300", linewidth = 1) +
    annotate("text", x = 2, y = bonf_line,
             label = paste0("Bonferroni threshold\n(\u03b1 = ", fdr_threshold, ")"),
             vjust = -0.4, color = "#FF5300", fontface = "bold", size = 3.5) +
    geom_line(linewidth = 1, alpha = 0.6) +
    geom_point(aes(size = significant, alpha = significant)) +
    scale_size_manual(values  = c("TRUE" = 4, "FALSE" = 2), guide = "none") +
    scale_alpha_manual(values = c("TRUE" = 1, "FALSE" = 0.5), guide = "none") +
    geom_point(data = filter(plot_data, significant),
               shape = 21, size = 5, fill = NA, color = "black", stroke = 1.5) +
    geom_label_repel(
      data = filter(plot_data, significant),
      aes(label = paste0(trait, "\nPC", PC)),
      fontface = "bold", size = 3.5,
      box.padding = 0.5, max.overlaps = 20, show.legend = FALSE
    ) +
    scale_x_continuous(breaks = 1:max(plot_data$PC)) +
    labs(
      x = "Principal Component",
      y = expression(-log[10](italic(P))),
      color = "Trait"
    ) +
    theme_qpc() +
    theme(
      panel.grid.major.x = element_line(color = "gray90"),
      panel.grid.major.y = element_line(color = "gray90")
    )
}

# ---------------------------------------------------------------------------
# 2. Faceted barplot: p-values per trait
# ---------------------------------------------------------------------------

plot_qpc_faceted <- function(qpc_results_list, fdr_threshold = 0.05) {

  plot_data <- map_dfr(names(qpc_results_list), function(trait) {
    pvals <- qpc_results_list[[trait]]$pvals
    n_pcs <- length(pvals)
    bonf  <- fdr_threshold / n_pcs
    tibble(
      trait          = trait,
      PC             = factor(1:n_pcs),
      pval           = pvals,
      log10p         = -log10(pvals),
      significant    = pvals < bonf,
      bonf_threshold = -log10(bonf)
    )
  })

  sig_counts <- plot_data |>
    group_by(trait) |>
    summarise(n_sig = sum(significant), .groups = "drop") |>
    mutate(label = paste0(trait, "\n(", n_sig, " sig)"))

  plot_data <- plot_data |>
    left_join(sig_counts, by = "trait") |>
    mutate(trait_label = factor(label, levels = sig_counts$label))

  ggplot(plot_data, aes(x = PC, y = log10p)) +
    geom_col(aes(fill = significant), alpha = 0.8) +
    geom_hline(aes(yintercept = bonf_threshold),
               linetype = "dashed", color = "#FF5300", linewidth = 0.8) +
    scale_fill_manual(
      values = c("TRUE" = "#1BB6AF", "FALSE" = "gray70"),
      labels = c("Not significant", "Significant"),
      name   = NULL
    ) +
    facet_wrap(~trait_label, scales = "free_y", ncol = 3) +
    labs(
      title    = "Qpc results by trait",
      subtitle = "Dashed line = Bonferroni-corrected significance threshold",
      x = "Principal Component",
      y = expression(-log[10](italic(P)))
    ) +
    theme_qpc() +
    theme(
      strip.background = element_rect(fill = "gray95", color = "gray30"),
      strip.text       = element_text(face = "bold", size = rel(1)),
      legend.position  = "bottom",
      panel.spacing    = unit(1, "lines")
    )
}

# ---------------------------------------------------------------------------
# 3. Heatmap: p-values across traits and PCs
# ---------------------------------------------------------------------------

plot_qpc_heatmap <- function(qpc_results_list) {

  plot_data <- map_dfr(names(qpc_results_list), function(trait) {
    pvals <- qpc_results_list[[trait]]$pvals
    tibble(
      trait  = trait,
      PC     = 1:length(pvals),
      pval   = pvals,
      log10p = -log10(pvals),
      sig_level = case_when(
        pvals < 0.001 ~ "***",
        pvals < 0.01  ~ "**",
        pvals < 0.05  ~ "*",
        TRUE          ~ ""
      )
    )
  }) |>
    mutate(trait = factor(trait, levels = unique(trait)),
           PC    = factor(PC))

  ggplot(plot_data, aes(x = PC, y = trait, fill = log10p)) +
    geom_tile(color = "white", linewidth = 0.5) +
    geom_text(aes(label = sig_level), color = "white", fontface = "bold", size = 5) +
    scale_fill_viridis_c(
      option = "plasma",
      name   = expression(-log[10](italic(P))),
      breaks = c(0, 1, 2, 3, 4),
      labels = c("0 (P=1)", "1 (P=0.1)", "2 (P=0.01)", "3 (P=0.001)", "4 (P<0.0001)")
    ) +
    labs(
      title    = "Qpc heatmap: selection across traits and PCs",
      subtitle = "* P < 0.05 | ** P < 0.01 | *** P < 0.001",
      x = "Principal Component",
      y = "Trait"
    ) +
    theme_minimal(base_size = 12) +
    theme(
      plot.title    = element_text(face = "bold", hjust = 0.5, size = rel(1.3)),
      plot.subtitle = element_text(hjust = 0.5, color = "gray40"),
      axis.title    = element_text(face = "bold"),
      panel.grid    = element_blank(),
      legend.position = "right"
    ) +
    coord_equal()
}

# ---------------------------------------------------------------------------
# 4. Scatter: trait vs PC eigenvector with population labels and CI bounds
# ---------------------------------------------------------------------------
# trait_values   : numeric vector (NAs allowed; last element dropped if n mismatch)
# eigenvectors   : matrix from eigen()$vectors
# eigenvalues    : vector from eigen()$values
# pc_number      : integer, which PC to plot on x-axis
# pop_assignments: character vector aligned to trait_values
# ci_vector      : pre-computed CI vector from qpc_ci (optional; computed if NULL)
# vaest          : Va estimate used to compute CI when ci_vector is NULL

plot_trait_pc_scatter <- function(trait_values, eigenvectors, eigenvalues,
                                  pc_number, pop_assignments,
                                  trait_name = "Trait",
                                  ci_vector  = NULL,
                                  vaest      = NULL,
                                  pop_colors = NULL,
                                  show_pop_labels = TRUE) {

  if (is.null(ci_vector) && !is.null(vaest)) {
    ci_vector <- 1.96 * sqrt(vaest * eigenvalues)
  }

  # Align lengths: drop trailing NA row if genotype matrix has one extra row
  n_eig <- nrow(eigenvectors)
  valid_idx <- seq_along(trait_values)
  if (length(trait_values) > n_eig) {
    valid_idx <- valid_idx[1:n_eig]
  }
  valid_idx <- valid_idx[!is.na(trait_values[valid_idx])]

  trait_clean <- trait_values[valid_idx]
  eigvec_clean <- eigenvectors[valid_idx, pc_number]
  pop_clean   <- pop_assignments[valid_idx]

  plot_data <- tibble(
    PC         = eigvec_clean,
    trait      = trait_clean,
    population = factor(pop_clean)
  )

  pop_centroids <- plot_data |>
    group_by(population) |>
    summarise(PC_mean    = mean(PC, na.rm = TRUE),
              trait_mean = mean(trait, na.rm = TRUE),
              .groups    = "drop")

  var_exp <- eigenvalues[pc_number] / sum(eigenvalues) * 100
  fit     <- lm(trait ~ PC, data = plot_data)
  r_sq    <- summary(fit)$r.squared
  p_val   <- summary(fit)$coefficients[2, 4]
  slope   <- coef(fit)[2]

  p <- ggplot(plot_data, aes(x = PC, y = trait))

  # CI bands
  if (!is.null(ci_vector)) {
    ci_val <- ci_vector[pc_number]
    trait_mean <- mean(trait_clean)
    p <- p +
      geom_abline(intercept = trait_mean, slope =  ci_val,
                  linetype = "dashed", color = "#56B4E9", linewidth = 1) +
      geom_abline(intercept = trait_mean, slope = -ci_val,
                  linetype = "dashed", color = "#56B4E9", linewidth = 1)
  }

  p <- p +
    geom_smooth(method = "lm", se = TRUE,
                color = "#0072B2", fill = "#0072B2",
                alpha = 0.2, linewidth = 1.2) +
    geom_point(aes(color = population), size = 3, alpha = 0.7)

  if (show_pop_labels) {
    p <- p +
      geom_label_repel(
        data = pop_centroids,
        aes(x = PC_mean, y = trait_mean, label = population, color = population),
        fontface = "bold", size = 4, alpha = 0.85,
        box.padding = 0.5, point.padding = 0.3,
        segment.color = "gray40", segment.size = 0.5,
        max.overlaps = 20, min.segment.length = 0,
        show.legend = FALSE
      )
  }

  if (!is.null(pop_colors)) {
    p <- p + scale_color_manual(values = pop_colors)
  }

  p <- p +
    annotate("text",
             x = min(plot_data$PC), y = max(plot_data$trait),
             label = sprintf("R\u00b2 = %.3f\nP = %.2e\nSlope = %.3f", r_sq, p_val, slope),
             hjust = 0, vjust = 1, fontface = "bold", size = 4) +
    labs(
      title    = paste0(trait_name, " vs PC", pc_number),
      subtitle = sprintf("PC%d explains %.2f%% of genetic variance", pc_number, var_exp),
      x = paste0("PC", pc_number),
      y = trait_name,
      color = "Population"
    ) +
    theme_qpc() +
    theme(legend.key.size = unit(0.5, "cm"))

  return(p)
}

# ---------------------------------------------------------------------------
# 5. Multi-panel: all significant trait-PC associations
# ---------------------------------------------------------------------------
# traits_data : named list mapping trait names (matching qpc_results_list keys)
#               to numeric vectors of trait values

plot_significant_associations <- function(qpc_results_list, traits_data,
                                          eigenvectors, eigenvalues,
                                          pop_assignments, ci_list = NULL,
                                          fdr_threshold = 0.05,
                                          pop_colors = NULL) {

  sig_assoc <- map_dfr(names(qpc_results_list), function(trait) {
    pvals <- qpc_results_list[[trait]]$pvals
    bonf  <- fdr_threshold / length(pvals)
    tibble(trait = trait, PC = seq_along(pvals), pval = pvals) |>
      filter(pval < bonf)
  })

  if (nrow(sig_assoc) == 0) {
    message("No significant associations found at \u03b1 = ", fdr_threshold, ".")
    return(invisible(NULL))
  }

  cat("Significant associations (Bonferroni \u03b1 =", fdr_threshold, "):\n")
  print(sig_assoc)

  plot_list <- pmap(list(sig_assoc$trait, sig_assoc$PC, sig_assoc$pval),
    function(trait, pc, pval) {
      ci_vec <- if (!is.null(ci_list)) ci_list[[trait]] else NULL
      vaest  <- var0(qpc_results_list[[trait]]$cml)

      plot_trait_pc_scatter(
        trait_values    = traits_data[[trait]],
        eigenvectors    = eigenvectors,
        eigenvalues     = eigenvalues,
        pc_number       = pc,
        pop_assignments = pop_assignments,
        trait_name      = trait,
        ci_vector       = ci_vec,
        vaest           = vaest,
        pop_colors      = pop_colors
      ) + labs(subtitle = sprintf("PC%d | P = %.2e", pc, pval))
    }
  )

  n <- length(plot_list)
  ncols <- if (n <= 2) n else if (n <= 4) 2 else 3
  wrap_plots(plot_list, ncol = ncols)
}



# Manhattan plot — all traits
p_manhattan <- plot_qpc_manhattan(qpc_results[names(traits_data)], fdr_threshold = 0.05)
ggsave(file.path(figs_dir, "qpc_manhattan.pdf"), p_manhattan, width = 14, height = 8)

# Faceted barplot — all traits
p_faceted <- plot_qpc_faceted(qpc_results[names(traits_data)], fdr_threshold = 0.05)
ggsave(file.path(figs_dir, "qpc_faceted.pdf"), p_faceted, width = 14, height = 10)

# Heatmap — all traits
p_heatmap <- plot_qpc_heatmap(qpc_results[names(traits_data)])
ggsave(file.path(figs_dir, "qpc_heatmap.pdf"), p_heatmap, width = 10, height = 6)

# Dashboard — Manhattan + heatmap
p_dashboard <- plot_qpc_dashboard(qpc_results[names(traits_data)], fdr_threshold = 0.05)
ggsave(file.path(figs_dir, "qpc_dashboard.pdf"), p_dashboard, width = 16, height = 12)

# All significant associations (scatter panels)
p_sig <- plot_significant_associations(
  qpc_results_list = qpc_results[names(traits_data)],
  traits_data      = traits_data,
  eigenvectors     = myEig$vectors,
  eigenvalues      = myEig$values,
  pop_assignments  = traits_merged$Population,
  ci_list          = qpc_ci[names(traits_data)],
  fdr_threshold    = 0.05,
  pop_colors       = pop_colors
)
if (!is.null(p_sig)) {
  ggsave(file.path(figs_dir, "qpc_significant_associations.pdf"),
         p_sig, width = 14, height = 10)
}

# Individual scatter plots for the two findings noted in the original script:
# Height vs PC9
p_height <- plot_trait_pc_scatter(
  trait_values    = traits_merged$postvern_height,
  eigenvectors    = myEig$vectors,
  eigenvalues     = myEig$values,
  pc_number       = 9,
  pop_assignments = traits_merged$Population,
  trait_name      = "Height",
  ci_vector       = qpc_ci[["height"]],
  pop_colors      = pop_colors,
  show_pop_labels = TRUE
)
ggsave(file.path(figs_dir, "qpc_height_PC9.png"), p_height, dpi = 600, width = 10, height = 8)

# Longest leaf vs PC10
p_lngst <- plot_trait_pc_scatter(
  trait_values    = traits_merged$postvern_lngst_lf,
  eigenvectors    = myEig$vectors,
  eigenvalues     = myEig$values,
  pc_number       = 10,
  pop_assignments = traits_merged$Population,
  trait_name      = "Longest leaf length",
  ci_vector       = qpc_ci[["lngst_lf"]],
  pop_colors      = pop_colors,
  show_pop_labels = TRUE
)
ggsave(file.path(figs_dir, "qpc_lngst_lf_PC10.png"), p_lngst, dpi = 600, width = 10, height = 8)