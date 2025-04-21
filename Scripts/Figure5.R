###############################################
# Load necessary libraries
###############################################
library(ggplot2)
library(cowplot)
library(readr)
library(dplyr)
library(RColorBrewer)
library(maps)

###############################################
# Define a Nature Medicine–style theme
###############################################
theme_nm <- theme_minimal(base_size = 16, base_family = "Helvetica") +
  theme(
    plot.title       = element_text(size = 16, face = "bold", hjust = 0.5, margin = margin(b = 10)),
    axis.title       = element_text(size = 14, face = "bold"),
    axis.text        = element_text(size = 12, color = "black"),
    panel.grid.major = element_line(color = "grey80", linetype = "dashed"),
    panel.grid.minor = element_blank(),
    legend.title     = element_text(size = 12, face = "bold"),
    legend.text      = element_text(size = 12),
    plot.margin      = margin(10, 10, 10, 10)
  )

###############################################
# 1) Read and prepare data
###############################################
reading_roc   <- read_csv("Reading_test_roc_curve.csv")   %>% rename(FPR = FPR, TPR = TPR)   %>% mutate(Center = "Reading")
bristol_roc   <- read_csv("Bristol_test_roc_curve.csv")   %>% rename(FPR = FPR, TPR = TPR)   %>% mutate(Center = "Bristol")
cheadle_roc   <- read_csv("Cheadle_test_roc_curve.csv")   %>% rename(FPR = FPR, TPR = TPR)   %>% mutate(Center = "Cheadle")
newcastle_roc <- read_csv("Newcastle_test_roc_curve.csv") %>% rename(FPR = FPR, TPR = TPR) %>% mutate(Center = "Newcastle")

# Combine ROC data and rename for plotting
roc_data <- bind_rows(reading_roc, bristol_roc, cheadle_roc, newcastle_roc) %>%
  rename(
    `False Positive Rate` = FPR,
    `True Positive Rate`  = TPR
  )

# Compute AUC per center
auc_data <- roc_data %>%
  group_by(Center) %>%
  arrange(`False Positive Rate`) %>%
  summarise(
    auc = sum(
      diff(`False Positive Rate`) *
      (head(`True Positive Rate`, -1) + tail(`True Positive Rate`, -1)) / 2
    ),
    .groups = "drop"
  )

auc_labels <- paste0(auc_data$Center, " (AUC: ", round(auc_data$auc, 3), ")")
roc_data$Center <- factor(roc_data$Center, levels = auc_data$Center)

# Read C-index data
cindex_data <- read_csv("loo_cindex.csv") %>%
  rename(center = center, cindex = cindex)

###############################################
# 2) Prepare UK map data
###############################################
uk_map <- map_data("world", region = "UK")
centers_map <- tibble::tibble(
  center = c("Reading", "Bristol", "Cheadle", "Newcastle"),
  lon    = c(-0.971, -2.5879, -2.1746, -1.6178),
  lat    = c(51.456, 51.4545, 53.3869, 54.9783)
)

###############################################
# 3) Create plots with updated styling
###############################################

# (a) UK Map
plot_a <- ggplot() +
  geom_polygon(
    data = uk_map,
    aes(x = long, y = lat, group = group),
    fill = "gray90", color = "black", size = 0.25
  ) +
  geom_point(
    data = centers_map,
    aes(x = lon, y = lat, color = center),
    size = 4
  ) +
  scale_color_brewer(palette = "Dark2") +
  coord_quickmap(xlim = c(-8, 2), ylim = c(49, 60)) +
  labs(title = "Location of Centers in the UK") +
  theme_void() +
  theme(
    plot.title  = element_text(size = 16, face = "bold", hjust = 0.5, margin = margin(b = 10)),
    plot.margin = margin(10, 10, 10, 10),
    legend.position = "right"
  )

# (b) ROC Curves
plot_b <- ggplot(roc_data, aes(x = `False Positive Rate`, y = `True Positive Rate`, color = Center)) +
  geom_line(size = 1.2) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "gray60") +
  scale_color_brewer(palette = "Dark2", labels = auc_labels) +
  guides(color = guide_legend(nrow = 2, byrow = TRUE)) +
  labs(
    title = "ROC Curves by Center",
    x     = "False Positive Rate",
    y     = "True Positive Rate",
    color = NULL
  ) +
  theme_nm +
  theme(
    legend.position = "bottom",
    legend.box      = "vertical",
    legend.margin   = margin(t = 5)
  )

# (c) C‑index Bar Plot
plot_c <- ggplot(cindex_data, aes(x = center, y = cindex, fill = center)) +
  geom_col(width = 0.6, color = "black", show.legend = FALSE) +
  scale_fill_brewer(palette = "Dark2") +
  labs(
    title = "C‑index by Center",
    x     = "Center",
    y     = "Test C‑index"
  ) +
  theme_nm +
  theme(
    panel.grid.major.x = element_blank(),
    panel.grid.minor   = element_blank(),
    panel.border       = element_rect(color = "black", fill = NA, size = 0.8)
  )

###############################################
# 4) Combine and save
###############################################
final_plot <- plot_grid(
  plot_a, plot_b, plot_c,
  labels      = c("a", "b", "c"),
  label_fontface = "bold",
  label_size  = 16,
  ncol        = 3
)

ggsave("Figure5.pdf", final_plot, width = 15, height = 5)
