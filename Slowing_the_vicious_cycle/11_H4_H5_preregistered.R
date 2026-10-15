#------------------------------------------------------------------
# 11 - H4 AND H5 AS PREREGISTERED (Appendix J, Figures 9 and 10)
# Independent variable: the treatment condition of the recollection read.
# Figure 10 shows why this test is attenuated: many recollections carry no
# treatment content, so the effect only emerges as the share of treatment
# words rises.
#------------------------------------------------------------------

source("00_setup.R")

W1 <- readRDS("data/derived/W1_analysis.rds")
W2 <- readRDS("data/derived/W2_analysis.rds")

library(marginaleffects)

#-------
# FIGURE 9
lab_perceived <- "Perceived affective polarization"
lab_own       <- "Respondent's own affective polarization"

prereg <- tribble(
  ~outcome,                            ~model,                                 ~panel,
  "affective_polarization_perception", "Affective polarization",               lab_perceived,
  "feel_termometer",                   "Feeling thermometer",                  lab_own,
  "affective_polarization",            "Honesty–intelligence–morality index",  lab_own
) %>%
  mutate(fit = map(outcome, \(y) fit_wls(reformulate("Polarization", y), W2))) %>%
  unnest(fit) %>%
  filter(term == "Polarization2") %>%
  mutate(panel = factor(panel, levels = c(lab_perceived, lab_own)),
         model = fct_rev(fct_inorder(model)))

prereg

H4_H5_plot_prereg <- ggplot(prereg, aes(x = estimate, y = model)) +
  geom_point() +
  geom_linerange(aes(xmin = estimate - 1.96 * std.error, xmax = estimate + 1.96 * std.error), linewidth = 0.7) +
  geom_linerange(aes(xmin = estimate - 1.645 * std.error, xmax = estimate + 1.645 * std.error), linewidth = 1) +
  geom_vline(xintercept = 0, linetype = "dashed") +
  facet_wrap(~ panel, ncol = 1, scales = "free_y") +
  xlim(-0.15, 0.22) +
  theme_paper() +
  labs(x = "Coefficient", y = "")

save_figure(H4_H5_plot_prereg, "figures/H4_H5_prereg.pdf")

#-------
# FIGURE 10: EFFECT OF THE CONDITION AS A FUNCTION OF THE SHARE OF TREATMENT WORDS
interaction_models <- c("affective_polarization_perception", "feel_termometer", "affective_polarization") %>%
  set_names() %>%
  map(\(y) lm(reformulate("Polarization * parrot_share", y), data = W2, weights = weight))

map(interaction_models, tidy)

marginal_effect <- comparisons(interaction_models$affective_polarization_perception,
                               variables = "Polarization",
                               newdata = datagrid(parrot_share = seq(0, 0.3, by = 0.05)))

marg_eff_plot <- ggplot(marginal_effect, aes(x = parrot_share, y = estimate)) +
  geom_ribbon(aes(ymin = estimate - 1.645 * std.error, ymax = estimate + 1.645 * std.error),
              alpha = 0.2, fill = "#555555") +
  geom_line() +
  geom_point() +
  geom_hline(yintercept = 0) +
  theme_paper() +
  labs(x = "Treatment share of words", y = "Average marginal effect")

save_figure(marg_eff_plot, "figures/H4_H5_marginal_effect.pdf")
