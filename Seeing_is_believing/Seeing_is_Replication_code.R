# ======================================================================
# REPLICATION CODE
# "Seeing is Believing? Most People Struggle to Detect AI-generated Images"
# Published in Social Media + Society
# ======================================================================
#
# DATA:
#   replication_data_long.rds       — item-level data (object E)
#                                     14,637 rows = 2,091 respondents x 7 images
#   replication_data_predictors.rds — respondent-level predictors (object D)
#                                     2,091 rows = 1 per respondent
#
# SOFTWARE:
#   Last verified under R 4.4.2 (2024-10-31 ucrt) on Windows 11 with
#     tidyverse 2.0.0   scoring 0.6      ggpubr 0.6.0   lme4   1.1-36
#     lmerTest 3.2.1    emmeans 1.11.1   texreg 1.39.4
#   All packages are on CRAN:
#     install.packages(c("tidyverse", "scoring", "ggpubr", "lme4",
#                        "lmerTest", "emmeans", "texreg"))
#
# NOTE ON APPENDIX H:
#   The final section (Appendix H) uses Meta Marketing API data that cannot
#   be redistributed. It is placed last, after every analysis that uses the
#   deposited data, and will stop with "object 'Meta_users' not found"
#   unless you supply that data yourself. Every other section runs without
#   it. See README.md.
#
# See codebook.md for a description of every variable in the two data files.
# ======================================================================


# ======================================================================
# LIBRARIES
# ======================================================================

library(tidyverse)
library(scoring)      # Brier score calculation
library(ggpubr)       # stat_cor for scatter plots
library(lme4)         # cross-classified MLMs
library(lmerTest)     # Satterthwaite p-values for lmer
library(emmeans)      # estimated marginal means from MLM
library(texreg)       # LaTeX regression tables for lmer


# ======================================================================
# LOAD DATA
# ======================================================================

E <- readRDS("replication_data_long.rds")
D <- readRDS("replication_data_predictors.rds")
# E: item-level data (one row per respondent × image)
# D: respondent-level predictors (one row per respondent)


# ======================================================================
# PRELIMINARIES
# ======================================================================

# Recode category labels from Danish to English
E$category <- dplyr::recode(
  E$category,
  "Almindelig" = "Regular", "Krise" = "Crisis",
  "Mennesker" = "People",   "Politik" = "Political"
)

# Recode empty AI label to "Real"
E$AI[E$AI == ""] <- "Real"

# Rescale sharing_numeric to 0–100 (original: 1–5 Likert)
# Done here so all downstream analyses use the same scale
E <- E %>%
  mutate(sharing_numeric = (sharing_numeric - 1) * 25)

# Set factor levels for category
E$category <- factor(E$category,
                     levels = c("Regular", "Crisis", "People", "Political"))

# Confidence measure (used in H4): deviation from 50 in percentage points
E <- E %>%
  mutate(
    score_certain = abs(as.numeric(score) - 50) * 2
  )


# ======================================================================
# BRIER SCORES
# ======================================================================

# --- Per-respondent Brier scores ----------
brier <- brierscore(AI_generated ~ score01, data = E, group = "ID")

brier_respondent <- as.data.frame(brier$brieravg) %>%
  rename(brier = `brier$brieravg`) %>%
  rownames_to_column(var = "ID")

# --- Per-item Brier scores -----------------------
brier_item <- as.data.frame(brier$rawscores) %>%
  rename(brier = `brier$rawscores`) %>%
  rownames_to_column(var = "row") %>%
  cbind(E)

# --- Simulated (random-guess) Brier scores ---------------
set.seed(123)
n_ids    <- nrow(brier_respondent)
ID_sim   <- rep(seq_len(n_ids), each = 7)
reality  <- sample(0:1, n_ids * 7, replace = TRUE)
forecast <- runif(n_ids * 7, min = 0, max = 1)
dataset_sim <- data.frame(ID = ID_sim, reality = reality, forecast = forecast)

brier_sim_model <- brierscore(reality ~ forecast, data = dataset_sim, group = "ID")
brier_sim <- as.data.frame(brier_sim_model$brieravg) %>%
  rename(brier = `brier_sim_model$brieravg`) %>%
  rownames_to_column(var = "ID")

# Item-level simulated Brier scores
brier_item_sim <- as.data.frame(brier_sim_model$rawscores) %>%
  rename(brier_sim = `brier_sim_model$rawscores`)
brier_item <- cbind(brier_item, brier_item_sim)

# Combined respondent-level df for H1 density plot
df_brier_combined <- data.frame(
  BrierScore = c(brier_respondent$brier, brier_sim$brier),
  Group = factor(rep(c("Respondents", "Chance"), each = nrow(brier_respondent)))
)


# ======================================================================
# ITEM-LEVEL DATA WITH PREDICTORS
# ======================================================================

brier_item$category <- factor(brier_item$category,
                               levels = c("Regular", "Crisis", "People", "Political"))

brier_item_full <- brier_item %>%
  left_join(
    D %>% select(
      ID, analytical, conspiracy_index, acceptable_AI_index,
      age, gender, wgt,
      AI_services_1, AI_services_2, AI_services_3, AI_services_4
    ),
    by = "ID"
  ) %>%
  mutate(
    age_scaled        = (as.numeric(age) - min(as.numeric(age), na.rm = TRUE)) / 10,
    conspiracy_scaled = conspiracy_index / 100,
    ai_accept_scaled  = acceptable_AI_index / 100,
    AI_exp = ifelse(
      as.numeric(AI_services_1) + as.numeric(AI_services_2) +
        as.numeric(AI_services_3) + as.numeric(AI_services_4) > 4,
      1, 0
    ),
    score_num     = as.numeric(score),
    score_certain = abs(score_num - 50) * 2
  )

brier_item_full$category <- factor(brier_item_full$category,
                                    levels = c("Regular", "Crisis", "People", "Political"))

# Respondent-level data with predictors
C <- brier_respondent %>%
  left_join(D, by = "ID") %>%
  mutate(
    age2                = (as.numeric(age) - min(as.numeric(age), na.rm = TRUE)) / 10,
    conspiracy_index2   = conspiracy_index / 100,
    acceptable_AI_index2 = acceptable_AI_index / 100,
    AI_exp = ifelse(
      as.numeric(AI_services_1) + as.numeric(AI_services_2) +
        as.numeric(AI_services_3) + as.numeric(AI_services_4) > 4,
      1, 0
    )
  )


# ======================================================================
# HYPOTHESIS 1
# ======================================================================
# Compares the distribution of respondents' average Brier scores to
# simulated random-guess Brier scores and to the 50%
# probability baseline. H1 is
# tested with a paired t-test.

brier_scores_resp <- brier_respondent$brier
brier_scores_sim  <- brier_sim$brier

# Normality checks
shapiro.test(brier_scores_resp)
shapiro.test(brier_scores_sim)

# H1: Are respondents better than random guessing?
t_test_vs_random <- t.test(brier_scores_resp, brier_scores_sim, paired = TRUE)
print(t_test_vs_random)

# Non-parametric alternative
wilcox_test_result <- wilcox.test(brier_scores_resp, brier_scores_sim, paired = TRUE)
print(wilcox_test_result)

# H1: Are respondents better than the 50% probability strategy?
t_test_vs_50 <- t.test(brier_scores_resp, mu = 0.25)
print(t_test_vs_50)

# Summary values for annotations
mean_respondents  <- round(mean(brier_scores_resp), 2)
mean_random       <- round(mean(brier_scores_sim),  2)
mean_conservative <- 0.25

# --- Figure 1 ---------------------------------------------------------
G_H1 <- df_brier_combined %>%
  ggplot(aes(x = BrierScore, fill = Group)) +
  geom_density(alpha = 0.5) +
  annotate("text", x = 0.70, y = 3.5,
           label = paste0("Respondents (avg.) = ", mean_respondents),
           size = 8 / .pt) +
  annotate("text", x = 0.70, y = 3.0,
           label = paste0("Random (avg.) = ",       mean_random, "*"),
           size = 8 / .pt) +
  annotate("text", x = 0.70, y = 2.5,
           label = paste0("50 p. probability (avg.) = ", mean_conservative, "*"),
           size = 8 / .pt) +
  geom_vline(xintercept = mean_conservative, linetype = "dashed") +
  scale_fill_manual(values = c("Respondents" = "grey", "Chance" = "#333333")) +
  theme_bw() +
  theme(legend.position = "bottom", text = element_text(size = 12)) +
  labs(x = "Brier score", y = "Density", fill = NULL)

G_H1


# ======================================================================
# HYPOTHESIS 2
# ======================================================================
# Tests whether image category predicts discernment ability.
# Model: brier ~ category + (1|ID) + (1|picture)
# Reference category: Regular.

h2_mlm <- lmer(
  brier ~ category + (1 | ID) + (1 | picture),
  data = brier_item_full
)
summary(h2_mlm)

# Estimated marginal means for plotting (absolute category-level means)
h2_emm <- as.data.frame(emmeans(h2_mlm, ~ category)) %>%
  rename(term = category, estimate2 = emmean) %>%
  mutate(
    lower_ci    = estimate2 - SE * 1.96,
    upper_ci    = estimate2 + SE * 1.96,
    lower_ci_90 = estimate2 - SE * 1.64,
    upper_ci_90 = estimate2 + SE * 1.64
  )

# Extract contrast p-values for significance stars (vs. Regular)
h2_contrasts <- as.data.frame(
  contrast(emmeans(h2_mlm, ~ category), method = "trt.vs.ctrl", ref = "Regular")
)

h2_stars <- h2_contrasts %>%
  mutate(
    term  = str_remove(contrast, " - Regular"),
    stars = case_when(
      p.value < 0.001 ~ "***",
      p.value < 0.01  ~ "**",
      p.value < 0.05  ~ "*",
      TRUE            ~ ""
    )
  ) %>%
  select(term, stars) %>%
  left_join(h2_emm %>% select(term, upper_ci_90), by = "term") %>%
  filter(stars != "")

# --- Figure 2 ---------------------------------------------------------
G_H2 <- h2_emm %>%
  mutate(term = factor(term, levels = c("Political", "People", "Crisis", "Regular"))) %>%
  ggplot(aes(x = estimate2, y = term)) +
  geom_point() +
  geom_linerange(aes(xmin = lower_ci,    xmax = upper_ci)) +
  geom_linerange(aes(xmin = lower_ci_90, xmax = upper_ci_90), linewidth = 1) +
  {if (nrow(h2_stars) > 0)
    geom_text(
      data = h2_stars %>%
        mutate(term = factor(term, levels = c("Political", "People", "Crisis", "Regular"))),
      aes(x = upper_ci_90 + 0.013, y = term, label = stars),
      size = 12 / .pt
    )
  } +
  geom_vline(xintercept = 0.25, linetype = "dashed") +
  geom_vline(xintercept = 0.33, linetype = "dashed") +
  annotate("segment", x = 0.26, xend = 0.251, y = 4.5, yend = 4.5,
           arrow = arrow(length = unit(0.2, "cm"))) +
  annotate("text", x = 0.261, y = 4.3,
           label = "50 p. probability", hjust = 0.5, size = 12 / .pt) +
  annotate("segment", x = 0.32, xend = 0.329, y = 4.5, yend = 4.5,
           arrow = arrow(length = unit(0.2, "cm"))) +
  annotate("text", x = 0.323, y = 4.3,
           label = "Random", hjust = 0.5, size = 12 / .pt) +
  theme_bw() +
  theme(text = element_text(size = 12)) +
  labs(x = "Brier score", y = "Context of image")

G_H2


# ======================================================================
# HYPOTHESIS 3
# ======================================================================
# Tests whether individual characteristics predict discernment ability.
# One cross-classified MLM per predictor, with DAG-implied adjustment sets
# Outcome: item-level Brier score.
# Survey weights included (weights = wgt).

# --- Individual models ------------------------------------------------

# Analytical thinking
h3_analytical <- lmer(
  brier ~ analytical + (1 | ID) + (1 | picture),
  data = brier_item_full, weights = wgt
)
summary(h3_analytical)

# Gender
h3_gender <- lmer(
  brier ~ gender + (1 | ID) + (1 | picture),
  data = brier_item_full, weights = wgt
)
summary(h3_gender)

# Age (scaled in 10-year intervals)
h3_age <- lmer(
  brier ~ age_scaled + (1 | ID) + (1 | picture),
  data = brier_item_full, weights = wgt
)
summary(h3_age)

# GenAI experience — DAG adjustment: control for age and gender
h3_gpt <- lmer(
  brier ~ AI_exp + gender + age_scaled + (1 | ID) + (1 | picture),
  data = brier_item_full, weights = wgt
)
summary(h3_gpt)

# Conspiracy beliefs — DAG adjustment: control for analytical thinking
h3_conspiracy <- lmer(
  brier ~ conspiracy_scaled + analytical + (1 | ID) + (1 | picture),
  data = brier_item_full, weights = wgt
)
summary(h3_conspiracy)

# AI acceptance — DAG adjustment: control for age, gender, GenAI experience
h3_accept <- lmer(
  brier ~ ai_accept_scaled + age_scaled + gender + AI_exp + (1 | ID) + (1 | picture),
  data = brier_item_full, weights = wgt
)
summary(h3_accept)

# --- Extract focal coefficients and SEs from each model ---------------

extract_coef <- function(model, coef_name) {
  fe  <- fixef(model)
  ses <- sqrt(diag(vcov(model)))
  data.frame(
    coefficient    = fe[coef_name],
    std_error      = ses[coef_name],
    row.names      = NULL
  )
}

discernment_model_df <- bind_rows(
  extract_coef(h3_conspiracy, "conspiracy_scaled")  %>% mutate(model = "conspiracy"),
  extract_coef(h3_analytical, "analytical")         %>% mutate(model = "crt"),
  extract_coef(h3_age,        "age_scaled")         %>% mutate(model = "age"),
  extract_coef(h3_accept,     "ai_accept_scaled")   %>% mutate(model = "acceptAI"),
  extract_coef(h3_gpt,        "AI_exp")             %>% mutate(model = "gptUse"),
  extract_coef(h3_gender,     "genderKvinde")       %>% mutate(model = "gender")
) %>%
  mutate(
    parameter = dplyr::recode(
      model,
      "gptUse"    = "Gen AI experience",
      "crt"       = "Analytical thinking",
      "age"       = "Age",
      "conspiracy"= "Conspiracy beliefs",
      "gender"    = "Gender (woman)",
      "acceptAI"  = "AI acceptance"
    ),
    lower_ci    = coefficient - std_error * 1.96,
    upper_ci    = coefficient + std_error * 1.96,
    lower_ci_90 = coefficient - std_error * 1.64,
    upper_ci_90 = coefficient + std_error * 1.64
  )

# --- Figure 3 ---------------------------------------------------------
G_H3 <- discernment_model_df %>%
  ggplot(aes(x = coefficient, y = parameter)) +
  geom_point(position = position_dodge(width = 0.5)) +
  geom_linerange(aes(xmin = lower_ci, xmax = upper_ci),
                 position = position_dodge(width = 0.5)) +
  geom_linerange(aes(xmin = lower_ci_90, xmax = upper_ci_90),
                 linewidth = 1, position = position_dodge(width = 0.5)) +
  geom_vline(xintercept = 0) +
  scale_x_continuous(
    breaks = c(-0.06, -0.045, -0.03, -0.015, 0, 0.015, 0.03, 0.045, 0.06),
    labels = c("-0.06", "\nBetter at\ndiscerning", "-0.03", "",
               "0.00", "", "0.03", "\nWorse at\ndiscerning", "0.06"),
    limits = c(-0.06, 0.06)
  ) +
  theme_bw() +
  theme(text = element_text(size = 12)) +
  labs(y = NULL, x = "Difference in Brier scores")

G_H3


# ======================================================================
# HYPOTHESIS 4
# ======================================================================
# Tests whether confidence in evaluations varies by image category.
# Model: score_certain ~ category + (1|ID) + (1|picture)
# Reference category: Regular. Contrasts show change in confidence (pp).

h4_mlm <- lmer(
  score_certain ~ category + (1 | ID) + (1 | picture),
  data = brier_item_full
)
summary(h4_mlm)

# Extract contrasts vs. Regular for plotting
h4_coefs <- fixef(h4_mlm)
h4_ses   <- sqrt(diag(vcov(h4_mlm)))
h4_pvals <- summary(h4_mlm)$coefficients[, "Pr(>|t|)"]

h4_plot_df <- tibble(
  term      = c("Regular", "Crisis", "People", "Political"),
  estimate  = c(0, h4_coefs["categoryCrisis"],
                   h4_coefs["categoryPeople"],
                   h4_coefs["categoryPolitical"]),
  std.error = c(0, h4_ses["categoryCrisis"],
                   h4_ses["categoryPeople"],
                   h4_ses["categoryPolitical"]),
  p.value   = c(1, h4_pvals["categoryCrisis"],
                   h4_pvals["categoryPeople"],
                   h4_pvals["categoryPolitical"])
) %>%
  mutate(
    lower_ci    = estimate - std.error * 1.96,
    upper_ci    = estimate + std.error * 1.96,
    lower_ci_90 = estimate - std.error * 1.64,
    upper_ci_90 = estimate + std.error * 1.64,
    stars = case_when(
      p.value < 0.001 ~ "***",
      p.value < 0.01  ~ "**",
      p.value < 0.05  ~ "*",
      TRUE            ~ ""
    )
  )

# Separate star dfs for negative (left) and positive (right) contrasts
h4_stars_neg <- h4_plot_df %>% filter(stars != "", estimate < 0)
h4_stars_pos <- h4_plot_df %>% filter(stars != "", estimate > 0)

# --- Figure 4 ---------------------------------------------------------
G_H4 <- h4_plot_df %>%
  mutate(term = factor(term, levels = c("Regular", "Crisis", "People", "Political"))) %>%
  ggplot(aes(x = estimate, y = term)) +
  geom_point() +
  geom_linerange(aes(xmin = lower_ci,    xmax = upper_ci)) +
  geom_linerange(aes(xmin = lower_ci_90, xmax = upper_ci_90), linewidth = 1) +
  {if (nrow(h4_stars_neg) > 0)
    geom_text(
      data = h4_stars_neg %>%
        mutate(term = factor(term, levels = c("Regular", "Crisis", "People", "Political"))),
      aes(x = lower_ci_90 - 0.4, y = term, label = stars),
      size = 12 / .pt
    )
  } +
  {if (nrow(h4_stars_pos) > 0)
    geom_text(
      data = h4_stars_pos %>%
        mutate(term = factor(term, levels = c("Regular", "Crisis", "People", "Political"))),
      aes(x = upper_ci_90 + 0.4, y = term, label = stars),
      size = 12 / .pt
    )
  } +
  geom_vline(xintercept = 0) +
  theme_bw() +
  theme(text = element_text(size = 12)) +
  labs(x = "Change in confidence (percentage points)", y = "Context of image")

G_H4

# --- H4: People vs. Crisis difference (estimate ± 1.96 × SE) -----------
# SE of the difference uses the full vcov matrix:
# Var(People - Crisis) = Var(People) + Var(Crisis) - 2*Cov(People, Crisis)
vcov_h4  <- vcov(h4_mlm)
diff_pc  <- h4_coefs["categoryPeople"] - h4_coefs["categoryCrisis"]
se_diff  <- sqrt(vcov_h4["categoryPeople", "categoryPeople"] +
                 vcov_h4["categoryCrisis",  "categoryCrisis"] -
                 2 * vcov_h4["categoryPeople", "categoryCrisis"])

cat("H4 — People vs. Crisis (estimate ± 1.96 × SE):\n")
cat(sprintf("  Estimate : %.3f\n  SE       : %.3f\n  95%% CI  : [%.3f, %.3f]\n",
            diff_pc, se_diff,
            diff_pc - 1.96 * se_diff,
            diff_pc + 1.96 * se_diff))


# ======================================================================
# APPENDIX C — TIME SPENT WATCHING IMAGE AND DISCERNMENT
# ======================================================================

# Average seconds per task
E %>%
  group_by(task) %>%
  summarise(
    mean_time   = mean(as.numeric(seconds), na.rm = TRUE),
    median_time = median(as.numeric(seconds), na.rm = TRUE)
  )

# Correlation between time spent and Brier score (pooled regression)
brier_item_time <- brier_item %>%
  mutate(seconds_num = as.numeric(seconds)) %>%
  filter(seconds_num <= 60)

time_reg <- lm(brier ~ seconds_num, data = brier_item_time)
summary(time_reg)

time_spent_plot <- brier_item_time %>%
  ggplot(aes(x = seconds_num, y = brier)) +
  geom_point(alpha = 0.03) +
  geom_smooth(method = "lm", color = "black") +
  theme_bw() +
  theme(text = element_text(size = 12)) +
  labs(y = "Brier score", x = "Seconds")

time_spent_plot


# ======================================================================
# APPENDIX D — BRIER SCORES PER IMAGE
# ======================================================================

AI_colors <- c("#E97451", "#4682B4")

brier_item_image_df <- brier_item %>%
  group_by(picture, AI) %>%
  summarise(
    value    = mean(brier),
    lower_ci = mean(brier) - 1.96 * sd(brier) / sqrt(n()),
    upper_ci = mean(brier) + 1.96 * sd(brier) / sqrt(n()),
    .groups  = "drop"
  ) %>%
  group_by(picture) %>%
  mutate(
    diff       = abs(diff(value)),
    pictureAI  = paste(picture, AI)
  ) %>%
  ungroup()

G_appendixD <- brier_item_image_df %>%
  mutate(AI = dplyr::recode(AI, "Real" = "Authentic")) %>%
  ggplot(aes(x = value, y = reorder(pictureAI, value), fill = AI)) +
  geom_bar(stat = "identity") +
  geom_linerange(aes(xmin = lower_ci, xmax = upper_ci),
                 position = position_dodge(width = 0.5)) +
  geom_vline(xintercept = mean(brier_item$brier), linetype = "dashed") +
  scale_fill_manual(values = AI_colors) +
  scale_y_discrete(labels = function(x) str_extract(x, "^\\w+")) +
  theme_bw() +
  theme(legend.position = "bottom", legend.title = element_blank(),
        text = element_text(size = 12)) +
  labs(x = "Average Brier score", y = "")

G_appendixD

# Difference in means between AI and authentic images (Table 4)
brier_image_lm <- lm(brier ~ AI, data = brier_item)
summary(brier_image_lm)


# ======================================================================
# APPENDIX E — ROBUSTNESS: CONTROLLING FOR IMAGE DIFFICULTY
# ======================================================================
# Adds respondent-level image difficulty (mean Brier score of images seen)
# as a control to each H3 model.

# Compute respondent-level difficulty from item means
image_difficulty <- brier_item %>%
  left_join(brier_item_image_df %>% select(picture, AI, value),
            by = c("picture", "AI")) %>%
  group_by(ID) %>%
  summarise(difficulty = mean(value), .groups = "drop")

brier_item_full <- brier_item_full %>%
  left_join(image_difficulty, by = "ID")

# Robustness models
h3r_conspiracy <- lmer(
  brier ~ conspiracy_scaled + analytical + difficulty + (1 | ID) + (1 | picture),
  data = brier_item_full, weights = wgt
)
h3r_analytical <- lmer(
  brier ~ analytical + difficulty + (1 | ID) + (1 | picture),
  data = brier_item_full, weights = wgt
)
h3r_age <- lmer(
  brier ~ age_scaled + difficulty + (1 | ID) + (1 | picture),
  data = brier_item_full, weights = wgt
)
h3r_accept <- lmer(
  brier ~ ai_accept_scaled + age_scaled + gender + AI_exp + difficulty +
    (1 | ID) + (1 | picture),  
  data = brier_item_full, weights = wgt
)
h3r_gender <- lmer(
  brier ~ gender + difficulty + (1 | ID) + (1 | picture),
  data = brier_item_full, weights = wgt
)
h3r_ai <- lmer(
  brier ~ AI_exp + gender + age_scaled + difficulty +  
    (1 | ID) + (1 | picture),
  data = brier_item_full, weights = wgt
)

discernment_model_df_r <- bind_rows(
  extract_coef(h3r_conspiracy, "conspiracy_scaled") %>% mutate(model = "conspiracy"),
  extract_coef(h3r_analytical, "analytical")        %>% mutate(model = "crt"),
  extract_coef(h3r_age,        "age_scaled")        %>% mutate(model = "age"),
  extract_coef(h3r_accept,     "ai_accept_scaled")  %>% mutate(model = "acceptAI"),
  extract_coef(h3r_ai,        "AI_exp")  %>% mutate(model = "AI_exp"),
  extract_coef(h3r_gender,     "genderKvinde")      %>% mutate(model = "gender")
) %>%
  mutate(
    parameter = dplyr::recode(
      model,
      "AI_exp"    = "Gen AI experience",
      "crt"       = "Analytical thinking",
      "age"       = "Age",
      "conspiracy"= "Conspiracy beliefs",
      "gender"    = "Gender (woman)",
      "acceptAI"  = "AI acceptance"
    ),
    lower_ci    = coefficient - std_error * 1.96,
    upper_ci    = coefficient + std_error * 1.96,
    lower_ci_90 = coefficient - std_error * 1.64,
    upper_ci_90 = coefficient + std_error * 1.64
  )

G_H3_r <- discernment_model_df_r %>%
  ggplot(aes(x = coefficient, y = parameter)) +
  geom_point(position = position_dodge(width = 0.5)) +
  geom_linerange(aes(xmin = lower_ci, xmax = upper_ci),
                 position = position_dodge(width = 0.5)) +
  geom_linerange(aes(xmin = lower_ci_90, xmax = upper_ci_90),
                 linewidth = 1, position = position_dodge(width = 0.5)) +
  geom_vline(xintercept = 0) +
  scale_x_continuous(
    breaks = c(-0.06, -0.045, -0.03, -0.015, 0, 0.015, 0.03, 0.045, 0.06),
    labels = c("-0.06", "\nBetter at\ndiscerning", "-0.03", "",
               "0.00", "", "0.03", "\nWorse at\ndescerning", "0.06"),
    limits = c(-0.06, 0.06)
  ) +
  theme_bw() +
  theme(text = element_text(size = 12)) +
  labs(y = NULL, x = "Difference in Brier scores")

G_H3_r





# ======================================================================
# APPENDIX I — DISTRIBUTION: EVALUATIONS IN THE RIGHT DIRECTION
# ======================================================================

E <- E %>%
  mutate(right_direction = ifelse(
    score01 > 0.5 & AI_generated == 1, 1,
    ifelse(score01 < 0.5 & AI_generated == 0, 1, 0)
  ))

right_direction_plot <- E %>%
  group_by(ID) %>%
  summarise(right_direction = sum(right_direction), .groups = "drop") %>%
  ggplot(aes(x = right_direction)) +
  geom_bar() +
  theme_bw() +
  theme(text = element_text(size = 12)) +
  labs(y = "Number of respondents",
       x = "Number of evaluations in the right direction")

right_direction_plot


# ======================================================================
# APPENDIX J — SHARING WILLINGNESS VS. AI RATING (WITHIN-RESPONDENT)
# ======================================================================
# Tests the within-respondent correlation between the assigned AI probability
# and willingness to share, by image category.
# Model: sharing_numeric ~ score + (1|ID) + (1|picture)
# sharing_numeric and score are both scaled 0–100.

sharing_regular  <- lmer(
  sharing_numeric ~ score_num + (1 | ID) + (1 | picture),
  data = brier_item_full %>% filter(category == "Regular")
)
sharing_crisis   <- lmer(
  sharing_numeric ~ score_num + (1 | ID) + (1 | picture),
  data = brier_item_full %>% filter(category == "Crisis")
)
sharing_people   <- lmer(
  sharing_numeric ~ score_num + (1 | ID) + (1 | picture),
  data = brier_item_full %>% filter(category == "People")
)
sharing_political <- lmer(
  sharing_numeric ~ score_num + (1 | ID) + (1 | picture),
  data = brier_item_full %>% filter(category == "Political")
)

# Print summaries
lapply(
  list(Regular = sharing_regular, Crisis = sharing_crisis,
       People  = sharing_people,  Political = sharing_political),
  summary
)


# ======================================================================
# APPENDIX K — BRIER SCORE DISTRIBUTIONS: AI VS. AUTHENTIC IMAGES
# ======================================================================

brier_AI_real_plot <- brier_item %>%
  mutate(AI = dplyr::recode(AI, "Real" = "Authentic")) %>%
  group_by(ID, AI) %>%
  summarise(brier = mean(brier), .groups = "drop") %>%
  ggplot(aes(x = brier, fill = AI)) +
  geom_density(alpha = 0.5) +
  theme_bw() +
  theme(text = element_text(size = 12)) +
  labs(y = "Density", x = "Brier score", fill = NULL)

brier_AI_real_plot


# ======================================================================
# APPENDIX L — AGE AND BRIER SCORE
# ======================================================================

# Extract MLM coefficient from h3_age for the plot annotation.
# age_scaled is in 10-year intervals; coefficient represents the change in
# Brier score per 10-year increase in age.
h3_age_coef <- round(fixef(h3_age)["age_scaled"], 3)
h3_age_pval <- summary(h3_age)$coefficients["age_scaled", "Pr(>|t|)"]
p_label     <- ifelse(h3_age_pval < 0.001, "p < 0.001",
                      paste0("p = ", round(h3_age_pval, 3)))
# Plain Unicode string — annotate() requires a character vector, not a call
mlm_label   <- paste0("β = ", h3_age_coef,
                      " (per 10 years), ", p_label)

age_brier_plot <- C %>%
  mutate(age_num = as.numeric(age) + 18) %>%
  ggplot(aes(x = age_num, y = brier)) +
  geom_point(alpha = 0.1) +
  geom_smooth(fill = "black", method = "lm") +
  annotate("text", x = 20, y = 0.95, label = mlm_label,
           hjust = 0, size = 10 / .pt) +
  geom_hline(yintercept = 0.25) +
  scale_x_continuous(n.breaks = 20) +
  theme_bw() +
  theme(text = element_text(size = 12)) +
  labs(y = "Brier score", x = "Age of respondent")

age_brier_plot


# ======================================================================
# APPENDIX N — SHARING WILLINGNESS
# ======================================================================

# N1: Scatter plot — AI probability rating vs. willingness to share
G_sharing <- E %>%
  ggplot(aes(x = as.numeric(score), y = sharing_numeric)) +
  geom_point(alpha = 0.01) +
  geom_smooth(method = "lm", se = TRUE, color = "black", linewidth = 0.5) +
  facet_wrap(~ category) +
  stat_cor(method = "pearson", cor.coef.name = "R", digits = 2) +
  theme_bw() +
  theme(text = element_text(size = 12)) +
  labs(x = "Assigned likelihood that image is AI generated (pct.)",
       y = "Willingness to share image with friends and family")

G_sharing

# N2: Differences in sharing willingness by category
# Model: sharing_numeric ~ category + (1|ID) + (1|picture)
h_n2_mlm <- lmer(
  sharing_numeric ~ category + (1 | ID) + (1 | picture),
  data = brier_item_full
)
summary(h_n2_mlm)

# Extract contrasts vs. Regular for plotting
n2_coefs <- fixef(h_n2_mlm)
n2_ses   <- sqrt(diag(vcov(h_n2_mlm)))

n2_plot_df <- tibble(
  term      = c("Regular", "Crisis", "People", "Political"),
  estimate  = c(0, n2_coefs["categoryCrisis"],
                   n2_coefs["categoryPeople"],
                   n2_coefs["categoryPolitical"]),
  std.error = c(0, n2_ses["categoryCrisis"],
                   n2_ses["categoryPeople"],
                   n2_ses["categoryPolitical"])
) %>%
  mutate(
    lower_ci    = estimate - std.error * 1.96,
    upper_ci    = estimate + std.error * 1.96,
    lower_ci_90 = estimate - std.error * 1.64,
    upper_ci_90 = estimate + std.error * 1.64
  )

G_N2 <- n2_plot_df %>%
  mutate(term = factor(term, levels = c("Regular", "Crisis", "People", "Political"))) %>%
  ggplot(aes(x = estimate, y = term)) +
  geom_point() +
  geom_linerange(aes(xmin = lower_ci,    xmax = upper_ci)) +
  geom_linerange(aes(xmin = lower_ci_90, xmax = upper_ci_90), linewidth = 1) +
  geom_vline(xintercept = 0) +
  theme_bw() +
  theme(text = element_text(size = 12)) +
  labs(x = "Change in willingness to share (percentage points)",
       y = "Context of image")

G_N2


# ======================================================================
# APPENDIX P — TRIAL ORDER EFFECTS
# ======================================================================

trial_order_plot <- brier_item %>%
  group_by(task) %>%
  summarise(
    sd   = sd(brier),
    mean = mean(brier),
    n    = n(),
    se   = sd / sqrt(n),
    .groups = "drop"
  ) %>%
  ggplot(aes(x = as.character(task), y = mean)) +
  geom_point(position = position_dodge(width = 0.5)) +
  geom_linerange(aes(ymin = mean - se * 1.96, ymax = mean + se * 1.96),
                 position = position_dodge(width = 0.5)) +
  geom_hline(yintercept = mean(brier_item$brier)) +
  theme_bw() +
  theme(text = element_text(size = 12)) +
  labs(y = "Brier score", x = "Task")

trial_order_plot


# ======================================================================
# APPENDIX Q — CROSS-CLASSIFIED MULTILEVEL MODEL OUTPUTS
# ======================================================================
# Provides full model output for the cross-classified MLMs underlying
# H2, H3, and H4. For H3, two versions are reported:
#   (a) Brier score models — same specification as the main analysis
#   (b) Raw score models   — truth × X interaction, directly testing
#       whether each predictor predicts discrimination ability vs.
#       calibration only.
#
# The four texreg() calls below print LaTeX table code to the console.
# To write them to disk instead, add e.g. file = "appendixQ1_h2.tex".

# ======================================================================

# Add truth variable (1 = AI-generated, 0 = authentic) to item-level data
brier_item_full <- brier_item_full %>%
  mutate(truth = as.integer(AI_generated))


# ======================================================================
# Q1: H2 MLM — CATEGORY AND DISCERNMENT ABILITY
# ======================================================================
# Model estimated in the H2 section above; output repeated here for
# the appendix.

cat(texreg(
  list(h2_mlm),
  custom.coef.map = list(
    "(Intercept)"       = "Regular (Intercept)",
    "categoryCrisis"    = "Crisis",
    "categoryPeople"    = "People",
    "categoryPolitical" = "Political"
  ),
  custom.model.names = c("Brier score"),
  caption      = paste0("Cross-classified MLM for H2: Brier score by image category. ",
                         "Reference category: Regular. Standard errors in parentheses."),
  label        = "tab:appendixQ1_h2",
  caption.above = TRUE
))


# ======================================================================
# Q2: H3 MLMs — BRIER SCORE VERSIONS
# ======================================================================
# Models estimated in the H3 section above; output repeated here.

cat(texreg(
  list(h3_analytical, h3_gender, h3_age,
       h3_gpt, h3_conspiracy, h3_accept),
  custom.coef.map = list(
    "analytical"        = "Analytical thinking",
    "genderKvinde"      = "Gender (woman)",
    "age_scaled"        = "Age (per 10 years)",
    "AI_exp"            = "GenAI experience",
    "conspiracy_scaled" = "Conspiracy beliefs",
    "ai_accept_scaled"  = "AI acceptance"
  ),
  custom.model.names = c("(1) Analytical", "(2) Gender", "(3) Age",
                          "(4) GenAI exp.", "(5) Conspiracy", "(6) AI accept."),
  caption      = paste0("Cross-classified MLMs for H3: subgroup predictors of discernment ",
                         "(Brier score). Each column is a separate model. ",
                         "Only the focal predictor is shown; DAG-implied controls ",
                         "(Appendix F) are included but not displayed. ",
                         "Standard errors in parentheses."),
  label        = "tab:appendixQ2_h3_brier",
  caption.above = TRUE
))


# ======================================================================
# Q3: H3 MLMs — RAW SCORE VERSIONS (truth × X interaction)
# ======================================================================
# Outcome: raw probability rating (0–100).
# truth = 1 for AI images, 0 for authentic.
# The truth × X interaction coefficient tests whether X predicts genuine
# discrimination ability: a significant positive interaction means X is
# associated with rating AI images higher AND authentic images lower.
# An insignificant interaction means X affects only overall rating level
# (calibration), not directional accuracy.
#
# DAG-implied controls are included and also interacted with truth to
# avoid collider bias, following the same adjustment sets as Q2.

# Analytical thinking × truth (no DAG controls required)
h3_raw_analytical <- lmer(
  score_num ~ truth * analytical + (1 | ID) + (1 | picture),
  data = brier_item_full, weights = wgt
)

# Gender × truth (no DAG controls required)
h3_raw_gender <- lmer(
  score_num ~ truth * gender + (1 | ID) + (1 | picture),
  data = brier_item_full, weights = wgt
)

# Age × truth (no DAG controls required)
h3_raw_age <- lmer(
  score_num ~ truth * age_scaled + (1 | ID) + (1 | picture),
  data = brier_item_full, weights = wgt
)

# GenAI experience × truth — DAG controls: age, gender (both × truth)
h3_raw_gpt <- lmer(
  score_num ~ truth * AI_exp +
              truth * age_scaled +
              truth * gender +
              (1 | ID) + (1 | picture),
  data = brier_item_full, weights = wgt
)

# Conspiracy beliefs × truth — DAG control: analytical (× truth)
h3_raw_conspiracy <- lmer(
  score_num ~ truth * conspiracy_scaled +
              truth * analytical +
              (1 | ID) + (1 | picture),
  data = brier_item_full, weights = wgt
)

# AI acceptance × truth — DAG controls: age, gender, GenAI experience (× truth)
h3_raw_accept <- lmer(
  score_num ~ truth * ai_accept_scaled +
              truth * age_scaled +
              truth * gender +
              truth * AI_exp +
              (1 | ID) + (1 | picture),
  data = brier_item_full, weights = wgt
)

cat(texreg(
  list(h3_raw_analytical, h3_raw_gender, h3_raw_age,
       h3_raw_gpt, h3_raw_conspiracy, h3_raw_accept),
  custom.coef.map = list(
    "truth"                   = "Truth (AI image)",
    "analytical"              = "Analytical thinking",
    "truth:analytical"        = "Truth $\\times$ Analytical thinking",
    "genderKvinde"            = "Gender (woman)",
    "truth:genderKvinde"      = "Truth $\\times$ Gender (woman)",
    "age_scaled"              = "Age (per 10 years)",
    "truth:age_scaled"        = "Truth $\\times$ Age",
    "AI_exp"                  = "GenAI experience",
    "truth:AI_exp"            = "Truth $\\times$ GenAI experience",
    "conspiracy_scaled"       = "Conspiracy beliefs",
    "truth:conspiracy_scaled" = "Truth $\\times$ Conspiracy beliefs",
    "ai_accept_scaled"        = "AI acceptance",
    "truth:ai_accept_scaled"  = "Truth $\\times$ AI acceptance"
  ),
  custom.model.names = c("(1) Analytical", "(2) Gender", "(3) Age",
                          "(4) GenAI exp.", "(5) Conspiracy", "(6) AI accept."),
  caption      = paste0("Cross-classified MLMs for H3: raw-score truth $\\times$ X ",
                         "interaction models (outcome: raw probability rating, 0--100). ",
                         "Truth = 1 for AI-generated images, 0 for authentic. ",
                         "A significant interaction indicates genuine discrimination ",
                         "ability; an insignificant interaction indicates calibration only. ",
                         "DAG-implied controls included but not shown. ",
                         "Standard errors in parentheses."),
  label        = "tab:appendixQ3_h3_raw",
  caption.above = TRUE
))

# --- Extract interaction coefficients for coefficient plot -------------
# Coefficient names follow R's convention for interaction terms:
# numeric × numeric: "truth:predictor"
# numeric × factor:  "truth:factorLevel"

extract_interaction <- function(model, interaction_name) {
  fe  <- fixef(model)
  ses <- sqrt(diag(vcov(model)))
  pvals <- summary(model)$coefficients[, "Pr(>|t|)"]
  data.frame(
    coefficient = fe[interaction_name],
    std_error   = ses[interaction_name],
    p.value     = pvals[interaction_name],
    row.names   = NULL
  )
}

raw_interaction_df <- bind_rows(
  extract_interaction(h3_raw_conspiracy, "truth:conspiracy_scaled") %>%
    mutate(model = "conspiracy"),
  extract_interaction(h3_raw_analytical, "truth:analytical") %>%
    mutate(model = "crt"),
  extract_interaction(h3_raw_age,        "truth:age_scaled") %>%
    mutate(model = "age"),
  extract_interaction(h3_raw_accept,     "truth:ai_accept_scaled") %>%
    mutate(model = "acceptAI"),
  extract_interaction(h3_raw_gpt,        "truth:AI_exp") %>%
    mutate(model = "gptUse"),
  extract_interaction(h3_raw_gender,     "truth:genderKvinde") %>%
    mutate(model = "gender")
) %>%
  mutate(
    parameter = dplyr::recode(
      model,
      "gptUse"     = "Gen AI experience",
      "crt"        = "Analytical thinking",
      "age"        = "Age",
      "conspiracy" = "Conspiracy beliefs",
      "gender"     = "Gender (woman)",
      "acceptAI"   = "AI acceptance"
    ),
    lower_ci    = coefficient - std_error * 1.96,
    upper_ci    = coefficient + std_error * 1.96,
    lower_ci_90 = coefficient - std_error * 1.64,
    upper_ci_90 = coefficient + std_error * 1.64
  )

# Coefficient plot: truth × X interaction effects (raw score scale, 0–100)
# A positive coefficient indicates better discrimination: the predictor is
# associated with rating AI images higher and/or authentic images lower.
G_Q3 <- raw_interaction_df %>%
  ggplot(aes(x = coefficient, y = parameter)) +
  geom_point(position = position_dodge(width = 0.5)) +
  geom_linerange(aes(xmin = lower_ci, xmax = upper_ci),
                 position = position_dodge(width = 0.5)) +
  geom_linerange(aes(xmin = lower_ci_90, xmax = upper_ci_90),
                 linewidth = 1, position = position_dodge(width = 0.5)) +
  geom_vline(xintercept = 0) +
  theme_bw() +
  theme(text = element_text(size = 12)) +
  labs(
    x = "Truth × X interaction (percentage points, raw score scale)",
    y = NULL,
    caption = paste0(
      "Positive coefficient = predictor associated with higher scores for AI images\n",
      "and/or lower scores for authentic images (better discrimination).\n",
      "Insignificant interaction = predictor reflects calibration only (e.g., gender)."
    )
  )

G_Q3


# ======================================================================
# Q4: H4 MLM — CATEGORY AND CONFIDENCE
# ======================================================================
# Model estimated in the H4 section above; output repeated here.

cat(texreg(
  list(h4_mlm),
  custom.coef.map = list(
    "(Intercept)"       = "Regular (Intercept)",
    "categoryCrisis"    = "Crisis",
    "categoryPeople"    = "People",
    "categoryPolitical" = "Political"
  ),
  custom.model.names = c("Confidence (pp)"),
  caption      = paste0("Cross-classified MLM for H4: confidence in evaluations by image ",
                         "category (deviation from 50\\%, percentage points). ",
                         "Reference category: Regular. Standard errors in parentheses."),
  label        = "tab:appendixQ4_h4",
  caption.above = TRUE
))



# ======================================================================
# APPENDIX H — FACEBOOK/INSTAGRAM ACTIVE USERS IN EU COUNTRIES
# ======================================================================
# NOTE: This section will NOT run with the deposited data. It reproduces the
# Appendix H figure from Meta Marketing API data, which Meta does not permit
# us to redistribute, so the object Meta_users is not created anywhere in
# this script. The code is retained for transparency. To reproduce the
# figure, obtain daily/monthly active-user estimates per EU country from the
# Meta Marketing API and Eurostat population figures, and build Meta_users
# with the columns used below: key, estimate_dau, estimate_mau_lower_bound,
# estimate_mau_upper_bound, Population.on.1.January...total.
# This section is placed last so that everything above runs to completion.

Meta_users <- Meta_users %>% 
  mutate(percentage_pop = estimate_dau / Population.on.1.January...total,
         percentage_pop_lower = estimate_mau_lower_bound / Population.on.1.January...total,
         percentage_pop_upper = estimate_mau_upper_bound / Population.on.1.January...total,
         highlight = ifelse(key == "DK", "1", "0"),
         average_mau_pop = (percentage_pop_lower + percentage_pop_upper) / 2,
         percentage_pop_dau_uncertainty = (percentage_pop_upper * 100 - percentage_pop_lower * 100) / 2)

FB_IG_users_graph <- Meta_users %>% 
  filter(key != "CY") %>%
  ggplot(aes(x = reorder(key, desc(percentage_pop)), y = percentage_pop * 100, fill = highlight)) +
  geom_bar(stat = "Identity") +
  geom_errorbar(aes(ymin = percentage_pop * 100 - percentage_pop_dau_uncertainty, ymax = percentage_pop * 100 + percentage_pop_dau_uncertainty), width = 0.25) +
  geom_hline(yintercept = median(Meta_users$percentage_pop*100), linetype = "dashed") +
  theme_bw() +
  ylim(0, 100) +
  scale_fill_manual(values = c("1" = "#E97451", "0" = "grey")) +
  theme(text = element_text(family = "serif", size = 12),
        legend.position = "none") +
  labs(x = "Countries",
       y = "Users as a share of population")

FB_IG_users_graph
