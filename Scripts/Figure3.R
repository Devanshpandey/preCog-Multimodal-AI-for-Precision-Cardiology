###############################################
# Load necessary libraries
###############################################
library(ggplot2)
library(dplyr)
library(gridExtra)
library(RColorBrewer)
library(forcats)
library(grid)
library(gtable)

###############################################
# Set a Nature Medicine–style theme
###############################################
theme_custom <- theme_minimal(base_size = 16, base_family = "Helvetica") +
  theme(
    plot.title       = element_text(size = 16, face = "bold", hjust = 0.5),
    axis.title       = element_text(size = 14, face = "bold"),
    axis.text        = element_text(size = 12, color = "black"),
    strip.text       = element_text(size = 12, face = "bold"),
    panel.grid.major = element_line(color = "grey80", size = 0.5),
    panel.grid.minor = element_line(color = "grey90", size = 0.25),
    panel.border     = element_rect(color = "black", fill = NA, size = 1),
    plot.margin      = margin(10, 10, 10, 10)
  )

###############################################
# 1) TOP ROW: LEAVE-ONE-OUT & INDIVIDUAL R²
###############################################

# Leave-One-Out (LOO) Data
loo_data <- data.frame(
  Feature_Group   = c("Baseline", "Metabolic", "PRS", "Socio-Economic", 
                      "Heart_LA-Imaging", "Liver-Imaging", "Pancreatic-Imaging", 
                      "BP", "DXA-Imaging", "Heart_BF-Imaging", "Heart_AD-Imaging", 
                      "ECG", "Diabetes"),
  Relative_Change = c(-2.843946, -1.712070, -7.661694, -1.644375, -9.642647, 
                      -0.374803, -0.283765, -0.055048, -0.202873, -0.685064, 
                      -4.299687, -2.613651, -1.174872),
  Error           = c(0.751948, 0.548148, 1.177813, 0.585517, 1.285013, 
                      0.419721, 0.404489, 0.214279, 0.409473, 0.462793, 
                      0.976606, 0.757642, 0.476573)
)

# Individual R² Data
r2_data <- data.frame(
  Feature_Group = c("Baseline", "Metabolic", "PRS", "Socio-Economic", 
                    "Heart_LA-Imaging", "Liver-Imaging", "Pancreatic-Imaging", 
                    "BP", "DXA-Imaging", "Heart_BF-Imaging", "Heart_AD-Imaging", 
                    "ECG", "Diabetes"),
  R2_Mean       = c(0.155589, 0.057193, 0.061627, 0.013108, 0.220006, 
                    0.090010, 0.070800, 0.033380, 0.029631, 0.103972, 
                    0.152749, 0.040572, 0.035773),
  CI_Low        = c(0.146747, 0.050248, 0.054308, 0.008818, 0.210174, 
                    0.081703, 0.062731, 0.027847, 0.023045, 0.093223, 
                    0.143491, 0.033364, 0.030420),
  CI_High       = c(0.164593, 0.064265, 0.067825, 0.016582, 0.227846, 
                    0.097905, 0.078909, 0.038110, 0.035468, 0.112260, 
                    0.162149, 0.046383, 0.040009)
)

# Order Feature_Group by descending Relative_Change
feature_order <- loo_data %>% 
  arrange(desc(Relative_Change)) %>% 
  pull(Feature_Group)
loo_data$Feature_Group <- factor(loo_data$Feature_Group, levels = feature_order)

# Plot (b) – Leave-One-Out
looPlot <- ggplot(loo_data, aes(x = Relative_Change, y = Feature_Group)) +
  geom_point(size = 4, color = "#fc8d62") +
  geom_errorbarh(aes(xmin = Relative_Change - Error, xmax = Relative_Change + Error),
                 height = 0.2, color = "black") +
  geom_vline(xintercept = 0, linetype = "dashed", color = "grey50", size = 0.8) +
  labs(
    title = "Leave-One-Out R²",
    x     = "Relative Change in R² (%)",
    y     = "Removed Feature Group",
    tag   = "b"
  ) +
  theme_custom +
  theme(plot.tag = element_text(size = 18, face = "bold"))

# Prepare Individual R² data (sorted)
r2_data <- r2_data %>% 
  arrange(desc(R2_Mean)) %>% 
  mutate(Feature_Group = factor(Feature_Group, levels = Feature_Group))

# Plot (a) – Individual Feature Group R²
r2Plot <- ggplot(r2_data, aes(x = R2_Mean, y = Feature_Group)) +
  geom_point(size = 4, color = "#66c2a5") +
  geom_errorbarh(aes(xmin = CI_Low, xmax = CI_High), height = 0.2, color = "black") +
  scale_y_discrete(limits = rev(levels(r2_data$Feature_Group))) +
  labs(
    title = "Individual Feature Group R²",
    x     = "R² Mean",
    y     = "Feature Group",
    tag   = "a"
  ) +
  theme_custom +
  theme(plot.tag = element_text(size = 18, face = "bold"))

# Combine top row side by side
topRow <- grid.arrange(r2Plot,looPlot ,ncol = 2)

###############################################
# 2) BOTTOM PANEL: UNIQUE VARIANCE LEFT
###############################################

# Data for imaging groups
other_data <- data.frame(
  Imaging_Group           = rep(c("Heart_LA-Imaging","Heart_AD-Imaging",
                                 "Liver-Imaging","Pancreatic-Imaging",
                                 "DXA-Imaging","Heart_BF-Imaging"), each = 4),
  Combination             = rep(c("Alone","With Baseline","With Metabolic","With ECG"), 6),
  Commonality_Coefficient = c(
    0.213496, -0.138022, -0.044970, -0.031519,
    0.152294, -0.107761, -0.043829, -0.021407,
    0.089534, -0.089256, -0.037964, -0.013733,
    0.074148, -0.070601, -0.026097, -0.009255,
    0.029366, -0.029364, -0.015078, -0.006277,
    0.106153, -0.091996, -0.035764, -0.018972
  )
)

PRS_data <- data.frame(
  Imaging_Group           = rep("PRS", 10),
  Combination             = c("Alone","With Baseline","With Metabolic","With ECG",
                              "With Heart LA","With Heart AD","With Heart BF",
                              "With Liver","With Pancreas","With DXA"),
  Commonality_Coefficient = c(0.063012, -0.016412, -0.014596, -0.005333,
                              -0.034255, -0.025101, -0.016107,
                              -0.016314, -0.011693, -0.004394)
)

commonality_data <- bind_rows(other_data, PRS_data)
commonality_data$Imaging_Group <- factor(
  commonality_data$Imaging_Group,
  levels = c("Heart_LA-Imaging","Heart_AD-Imaging","Liver-Imaging",
             "Pancreatic-Imaging","DXA-Imaging","Heart_BF-Imaging","PRS")
)

commonality_processed <- commonality_data %>%
  group_by(Imaging_Group) %>%
  mutate(
    Alone_Value          = Commonality_Coefficient[Combination == "Alone"],
    Unique_Variance_Left = ifelse(Combination == "Alone",
                                  Commonality_Coefficient,
                                  Alone_Value + Commonality_Coefficient),
    Combination_ordered  = factor(Combination,
                                  levels = Combination[order(Unique_Variance_Left, decreasing = TRUE)])
  ) %>%
  ungroup()

unique_variance_plot <- ggplot(commonality_processed,
                               aes(x = Combination_ordered, y = Unique_Variance_Left)) +
  geom_bar(stat = "identity", fill = "#8da0cb") +
  facet_wrap(~ Imaging_Group, ncol = 7, scales = "free_x") +
  labs(
    title = "Unique Variance Explained",
    x     = "Feature Combination",
    y     = "Unique Variance",
    tag   = "c"
  ) +
  theme_custom +
  theme(
    axis.text.x = element_text(angle = 90, hjust = 1),
    plot.tag    = element_text(size = 18, face = "bold")
  )

# Adjust facet widths so PRS is wider
g <- ggplotGrob(unique_variance_plot)
panels <- g$layout[grepl("panel", g$layout$name), ]
panel_cols <- unique(panels$l)
new_widths <- g$widths
for (i in seq_along(panel_cols)) {
  new_widths[panel_cols[i]] <- if (i == 7) unit(2, "null") else unit(0.8, "null")
}
g$widths <- new_widths

###############################################
# 3) COMBINE ALL PANELS INTO ONE FIGURE
###############################################
combinedPlot <- grid.arrange(topRow, g, nrow = 2, heights = c(1, 1.1))

# Save as PDF
ggsave("Figure3abc.pdf", combinedPlot, width = 16, height = 12)
