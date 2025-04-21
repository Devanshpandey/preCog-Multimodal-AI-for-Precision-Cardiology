###############################################
# Load necessary libraries
###############################################
library(dplyr)
library(survival)
library(survminer)
library(ggplot2)
library(patchwork)
library(grid)
library(gridExtra)

###############################################
# 1. Define Nature Medicine–style themes
###############################################
theme_nm <- theme_minimal(base_size = 16, base_family = "Helvetica") +
  theme(
    plot.title       = element_text(size = 16, face = "bold", hjust = 0.5),
    axis.title       = element_text(size = 14, face = "bold"),
    axis.text        = element_text(size = 12, color = "black"),
    panel.grid.major = element_line(color = "grey80", linetype = "dashed"),
    panel.grid.minor = element_blank(),
    legend.title     = element_text(size = 12, face = "bold"),
    legend.text      = element_text(size = 10),
    plot.margin      = margin(10, 10, 10, 10)
  )

theme_small <- theme_minimal(base_size = 16, base_family = "Helvetica") +
  theme(
    plot.title       = element_text(size = 16, face = "bold", hjust = 0.5),
    axis.title       = element_text(size = 14, face = "bold"),
    axis.text        = element_text(size = 12, color = "black"),
    panel.grid.major = element_line(color = "grey80", linetype = "dashed"),
    panel.grid.minor = element_blank(),
    legend.title     = element_text(size = 12, face = "bold"),
    legend.text      = element_text(size = 10),
    plot.margin      = margin(10, 10, 10, 10)
  )

###############################################
# 2. Define color palettes
###############################################
palette_cv   <- c("#9D7660", "#59A14F", "#E15759")
palette_surv <- c("#009E73", "#0072B2", "#D55E00")
palette_risk <- c("Low" = "#009E73", "Medium" = "#0072B2", "High" = "#D55E00")

###############################################
# 3. CV C-Index Calculation & Plot
###############################################
compute_c_index_with_cv <- function(data, features,
                                    duration_col = "time_at_risk",
                                    event_col    = "event",
                                    n_splits     = 5) {
  set.seed(42)
  folds <- sample(rep(1:n_splits, length.out = nrow(data)))
  sapply(1:n_splits, function(i) {
    train_fold <- data[folds != i, ]
    val_fold   <- data[folds == i, ]
    form       <- paste0("Surv(", duration_col, ", ", event_col, ") ~ ",
                         paste(features, collapse = " + "))
    model      <- coxph(as.formula(form), data = train_fold)
    preds      <- predict(model, newdata = val_fold, type = "lp")
    surv_obj   <- Surv(val_fold[[duration_col]], val_fold[[event_col]])
    survConcordance(surv_obj ~ preds)$concordance
  })
}

train_data <- read.csv("cox_train.csv")
baseline_features      <- c("age","sex","bmi","smoking")
metabolic_features     <- c("ldl","trig","total_cholestrol","DM")
PRS                    <- c("PRS")
heart_imaging_features <- c(
  paste0("pca_embedding0_",         1:10),
  paste0("pca_embedding1_",         1:10),
  paste0("pca_embedding2_",         1:10),
  paste0("pca_aortic_embeddings_",  1:10)
)

steps <- list(
  "Framingham Model" = c(baseline_features, metabolic_features),
  "+ PRS"            = c(baseline_features, metabolic_features, PRS),
  "+ Heart Imaging"  = c(baseline_features, metabolic_features, PRS, heart_imaging_features)
)

results <- do.call(rbind, lapply(names(steps), function(step_name) {
  cidx <- compute_c_index_with_cv(train_data, steps[[step_name]])
  data.frame(
    Step    = factor(step_name, levels = names(steps)),
    C_Index = cidx
  )
}))

cv_plot <- ggplot(results, aes(x = Step, y = C_Index, fill = Step)) +
  geom_boxplot(width = 0.5, outlier.shape = NA, color = "black", size = 1) +
  geom_jitter(width = 0.15, size = 2, alpha = 0.8, color = "black") +
  scale_fill_manual(values = palette_cv) +
  labs(
    tag   = "a",
    title = "5-Fold CV C-Index Distribution",
    x     = "Model Step",
    y     = "C-Index"
  ) +
  theme_nm +
  theme(
    plot.tag        = element_text(size = 16, face = "bold"),
    legend.position = "none"              # remove legend in plot a
  )

###############################################
# 4. Survival Curves (Train + Test, incident only)
###############################################
train_surv    <- read.csv("cox_train.csv")
test_surv     <- read.csv("cox_test.csv")
incident_data <- bind_rows(train_surv, test_surv) %>%
  filter(!(event == 1 & time_at_risk == 0.001))

# (b) Framingham groups
frm_formula <- as.formula(paste(
  "Surv(time_at_risk, event) ~",
  paste(c(baseline_features, metabolic_features), collapse = " + ")
))
cox_fram <- coxph(frm_formula, data = incident_data)
incident_data$fram_score <- predict(cox_fram, newdata = incident_data, type = "risk")
incident_data$fram_group <- cut(
  incident_data$fram_score,
  breaks = quantile(incident_data$fram_score, probs = c(0,1/3,2/3,1), na.rm = TRUE),
  labels = c("Low","Medium","High"),
  include.lowest = TRUE
)

fit_fram <- survfit(Surv(time_at_risk, event) ~ fram_group, data = incident_data)
p1 <- ggsurvplot(
  fit_fram, data = incident_data,
  pval              = TRUE,
  risk.table        = TRUE,
  risk.table.height = 0.15,
  ggtheme           = theme_small,
  title             = "Framingham Risk Groups",
  ylim              = c(0, 1),
  palette           = palette_surv
)
p1$plot <- p1$plot +
  labs(tag = "b") +
  theme(
    plot.tag        = element_text(size = 16, face = "bold"),
    legend.position = "none"       ,       # remove legend in plot a
    plot.title  = element_text(size = 16, face = "bold", hjust = 0.5),
    axis.title  = element_text(size = 12, face = "bold"),
    plot.margin = margin(10, 10, 10, 10)
  )

# (c1) PRS within High Framingham
high_fram <- filter(incident_data, fram_group == "High")
high_fram$PRS_group <- cut(
  high_fram$PRS,
  breaks = quantile(high_fram$PRS, probs = c(0,1/3,2/3,1), na.rm = TRUE),
  labels = c("Low","Medium","High"),
  include.lowest = TRUE
)

fit_prs <- survfit(Surv(time_at_risk, event) ~ PRS_group, data = high_fram)
p2 <- ggsurvplot(
  fit_prs, data = high_fram,
  pval              = TRUE,
  risk.table        = TRUE,
  risk.table.height = 0.15,
  ggtheme           = theme_small,
  title             = "PRS Stratification\n(High Framingham)",
  ylim              = c(0, 1),
  palette           = palette_surv
)
p2$plot <- p2$plot + theme(plot.margin = margin(10, 10, 10, 10),   legend.position = "none" )

# (c2) Imaging within High Framingham & High PRS
high_fram_high_prs <- filter(high_fram, PRS_group == "High")
img_formula <- as.formula(paste(
  "Surv(time_at_risk, event) ~",
  paste(heart_imaging_features, collapse = " + ")
))
cox_img <- coxph(img_formula, data = high_fram_high_prs)
high_fram_high_prs$img_score <- predict(cox_img, newdata = high_fram_high_prs, type = "risk")
high_fram_high_prs$img_group <- cut(
  high_fram_high_prs$img_score,
  breaks = quantile(high_fram_high_prs$img_score, probs = c(0,1/3,2/3,1), na.rm = TRUE),
  labels = c("Low","Medium","High"),
  include.lowest = TRUE
)

fit_img <- survfit(Surv(time_at_risk, event) ~ img_group, data = high_fram_high_prs)
p3 <- ggsurvplot(
  fit_img, data = high_fram_high_prs,
  pval              = TRUE,
  risk.table        = TRUE,
  risk.table.height = 0.15,
  ggtheme           = theme_small,
  title             = "Imaging Stratification\n(High Framingham & High PRS)",
  ylim              = c(0, 1),
  palette           = palette_surv
)
p3$plot <- p3$plot + theme(plot.margin = margin(10, 10, 10, 10),  legend.position = "none" )

###############################################
# 5. Relative Risk Bar Plot with multi-line x labels
###############################################
df_fram <- incident_data %>%
  group_by(fram_group) %>%
  summarize(num_events = sum(event), total = n(), .groups = "drop") %>%
  mutate(incidence = 100 * num_events / total, Stratifier = "Framingham") %>%
  rename(RiskGroup = fram_group)

df_prs <- high_fram %>%
  group_by(PRS_group) %>%
  summarize(num_events = sum(event), total = n(), .groups = "drop") %>%
  mutate(incidence = 100 * num_events / total, Stratifier = "PRS (High Framingham)") %>%
  rename(RiskGroup = PRS_group)

df_img <- high_fram_high_prs %>%
  group_by(img_group) %>%
  summarize(num_events = sum(event), total = n(), .groups = "drop") %>%
  mutate(incidence = 100 * num_events / total,
         Stratifier = "Imaging (High PRS & High Framingham)") %>%
  rename(RiskGroup = img_group)

df_combined <- bind_rows(df_fram, df_prs, df_img) %>%
  mutate(
    Stratifier = factor(
      Stratifier,
      levels = c(
        "Framingham",
        "PRS (High Framingham)",
        "Imaging (High PRS & High Framingham)"
      ),
      labels = c(
        "Framingham",
        "PRS\n(High Fram)",
        "Imaging\n(High Fram & High PRS)"
      )
    ),
    RiskGroup = factor(RiskGroup, levels = c("Low","Medium","High"))
  )

IR_ref <- df_combined %>%
  filter(Stratifier == "Framingham", RiskGroup == "Low") %>%
  pull(incidence)

df_combined <- df_combined %>%
  mutate(Relative_Risk = incidence / IR_ref)

prop_plot <- ggplot(df_combined, aes(x = Stratifier, y = Relative_Risk, fill = RiskGroup)) +
  geom_col(position = position_dodge(width = 0.8), width = 0.6) +
  geom_text(aes(label = sprintf("%.1f", Relative_Risk), y = Relative_Risk + 0.2),
            position = position_dodge(width = 0.8),
            size = 4, vjust = 0.5) +
  scale_fill_manual(values = palette_risk) +
  labs(
    tag   = "c",
    title = "Relative Risk by Model",
    x     = "Model",
    y     = "Relative Risk",
    fill  = "Risk Group"
  ) +
  theme_nm +
  theme(
    plot.tag      = element_text(size = 16, face = "bold"),
    legend.position = "bottom",
    legend.box      = "horizontal",
    legend.key.size = unit(0.75, "lines")
  )

###############################################
# 6. Combine & Save Final Figure
###############################################
top_row       <- cv_plot + prop_plot + plot_layout(ncol = 2, widths = c(1, 1))
combined_surv <- p1$plot + p2$plot + p3$plot + plot_layout(ncol = 3)
final_plot    <- top_row / combined_surv + plot_layout(heights = c(1, 1))

ggsave("Figure4.pdf", final_plot, width = 14, height = 10)

# Display the figure
final_plot
