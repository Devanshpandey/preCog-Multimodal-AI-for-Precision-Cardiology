###############################################
# Load necessary libraries
###############################################
required_packages <- c("ggplot2", "dplyr", "readr", "scales",
                       "gridExtra", "grid", "patchwork")

for (pkg in required_packages) {
  if (!require(pkg, character.only = TRUE)) {
    install.packages(pkg, repos = "http://cran.us.r-project.org")
    library(pkg, character.only = TRUE)
  }
}

###############################################
# Set theme for aesthetic plots (Nature Medicine style)
###############################################
theme_custom <- theme_minimal(base_size = 16, base_family = "Helvetica") +
  theme(
    plot.title      = element_text(size = 16, face = "bold", hjust = 0.5),
    axis.title      = element_text(size = 14, face = "bold"),
    axis.text       = element_text(size = 12),
    legend.title    = element_text(size = 14, face = "bold"),
    legend.text     = element_text(size = 12),
    panel.grid.major = element_line(color = "grey80", linetype = "dashed"),
    plot.margin     = margin(10, 10, 10, 10)  # Add padding
  )

###############################################
# Define improved color-blind friendly palettes
###############################################
# For the training plots (6 PC categories):
cbPalettePCs    <- c("#009E73", "#0072B2", "#D55E00", "#CC79A7", "#E69F00", "#F0E442")

# For the AUC plot (3 models):
cbPaletteModels <- c("#E41A1C", "#377EB8", "#4DAF4A")

# Define a new color-blind friendly palette for the AUC plot
cbPaletteAUC    <- c("#E41A1C", "#0072B2", "#4DAF4A")

###############################################
# 1) Load and plot train_cv results
###############################################
train_results <- read_csv("pca_xgboost_cv_results.csv")

# Convert "Num_PCs" column to a factor with custom labels
num_pcs_labels <- c("5", "10", "15", "20", "25", "Full Embedding")
train_results$Num_PCs <- factor(
  train_results$Num_PCs,
  levels = unique(train_results$Num_PCs),
  labels = num_pcs_labels
)

### A) Plot AUC vs. number of PCs
train_plot_auc <- ggplot(train_results, aes(x = Num_PCs, y = AUC, fill = Num_PCs)) +
  geom_boxplot(alpha = 0.6, outlier.shape = NA) +
  geom_jitter(width = 0.2, size = 2, alpha = 0.7, color = "black") +
  scale_fill_manual(values = cbPalettePCs) +
  theme_custom +
  labs(
    title = "AUC (5-fold CV)",
    x     = "Number of Principal Components",
    y     = "AUC"
  ) +
  theme(legend.position = "none")

### B) Plot Accuracy vs. number of PCs
train_plot_acc <- ggplot(train_results, aes(x = Num_PCs, y = Accuracy, fill = Num_PCs)) +
  geom_boxplot(alpha = 0.6, outlier.shape = NA) +
  geom_jitter(width = 0.2, size = 2, alpha = 0.7, color = "black") +
  scale_fill_manual(values = cbPalettePCs) +
  theme_custom +
  labs(
    title = "Accuracy (5-fold CV)",
    x     = "Number of Principal Components",
    y     = "Accuracy"
  ) +
  theme(legend.position = "none")

# Combine the two training plots side by side
train_plots_combined <- train_plot_auc + train_plot_acc +
  plot_layout(nrow = 1)  # 1 row, 2 columns

###############################################
# 2) Load AUC & LRT results
###############################################
auc_data <- read_csv("cv_auc_scores_per_fold.csv")
lrt_data <- read_csv("likelihood_ratio_tests_extended.csv")

# Ensure required columns exist
if (!all(c("LR-Stat", "P-Value") %in% colnames(lrt_data))) {
  stop("Error: Missing required columns in 'likelihood_ratio_tests_extended.csv'")
}

# Convert model names to ordered factors
auc_data$Model <- factor(
  auc_data$Model,
  levels = c("A (IDPs)", "B (PCA)", "C (IDPs + PCA)")
)

# We assume the LR tests of interest are in rows 1 and 3:
# - Row 1 for "A → B"
# - Row 3 for "B → C"
lr_label_A_B <- paste0(
  "LR=", round(lrt_data$`LR-Stat`[1], 1),
  ", p=", formatC(lrt_data$`P-Value`[1], format = "e", digits = 2)
)
lr_label_B_C <- paste0(
  "LR=", round(lrt_data$`LR-Stat`[3], 1),
  ", p=", formatC(lrt_data$`P-Value`[3], format = "e", digits = 2)
)

###############################################
# 3) AUC Scores Across Models with Arrows for LR Test
###############################################
auc_lrt_plot <- ggplot(auc_data, aes(x = Model, y = AUC, fill = Model)) +
  geom_boxplot(width = 0.2, outlier.shape = NA, alpha = 0.6) +
  geom_jitter(width = 0.2, size = 2, alpha = 0.5, color = "black") +
  scale_fill_manual(values = cbPaletteAUC) +
  theme_custom +
  # Remove legend since "Model" is already on the x-axis
  theme(legend.position = "none") +
  labs(
    title = "AUC Scores Across Models with LR Test",
    x     = "Model",
    y     = "AUC Score"
  )

# To place arrows above the boxplots, define some coordinates:
y_min <- min(auc_data$AUC)
y_max <- max(auc_data$AUC)

# We'll place the first arrow just below the top of the range
y_arrow1 <- y_max - 0.02
# The second arrow slightly above that
y_arrow2 <- y_max + 0.01

auc_lrt_plot <- auc_lrt_plot +
  # Arrow from A (x=1) to B (x=2)
  geom_segment(
    aes(x = 1.20, xend = 1.70, y = y_arrow1, yend = y_arrow1),
    arrow = arrow(length = unit(0.15, "inches"), type = "closed"),
    size = 0.6
  ) +
  annotate(
    "text",
    x     = 1.45,
    y     = y_arrow1 + 0.005,
    label = lr_label_A_B,
    size  = 4,
    fontface = "bold"
  ) +
  # Arrow from B (x=2) to C (x=3)
  geom_segment(
    aes(x = 2.20, xend = 2.70, y = y_arrow2, yend = y_arrow2),
    arrow = arrow(length = unit(0.15, "inches"), type = "closed"),
    size = 0.6
  ) +
  annotate(
    "text",
    x     = 2.45,
    y     = y_arrow2 + 0.005,
    label = lr_label_B_C,
    size  = 4,
    fontface = "bold"
  ) +
  # Expand y-limits so arrows/text aren't cut off
  expand_limits(y = y_max + 0.05)

###############################################
# 4) Arrange plots and save as high-resolution PDF
###############################################
final_combined_plot <- train_plots_combined / auc_lrt_plot +
  plot_annotation(tag_levels = 'a') &
  theme(
    plot.title = element_text(size = 16, face = "bold"),
    plot.tag   = element_text(face = "bold")
  )

# Save high-resolution PDF
ggsave("Figure2.pdf", final_combined_plot, width = 16, height = 12, dpi = 400)

###############################################
# 5) Display plots
###############################################
print(train_plot_auc)
print(train_plot_acc)
print(auc_lrt_plot)
print(final_combined_plot)
