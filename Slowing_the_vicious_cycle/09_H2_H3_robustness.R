#------------------------------------------------------------------
# 09 - H2 AND H3 ROBUSTNESS (Appendix H and I)
# H: split by who debates in the article (voters / politicians) and by whether
#    the outcome concerns voters or elites.
# I: does blaming the out-party moderate the treatment effect?
#------------------------------------------------------------------

source("00_setup.R")

W1 <- readRDS("data/derived/W1_analysis.rds")
W2 <- readRDS("data/derived/W2_analysis.rds")

#-------
# APPENDIX H
outcome_labels <- c(affective_polarization_perception = "Perceived affective polarization",
                    feel_termometer = "Respondent feeling thermometer",
                    affective_polarization = "Respondent affective polarization")

split_models <- expand_grid(outcome = names(outcome_labels),
                            target = c("elite", "voter"),
                            group = 1:2) %>%
  mutate(condition = if_else(group == 1, "Voter", "Politician"),
         fit = pmap(list(outcome, target, group), \(o, t, g)
                    fit_wls(reformulate(c("Polarization", "Issue_conditions", "Quote"), paste0(o, "_", t)),
                            filter(W1, Group == g)))) %>%
  unnest(fit) %>%
  filter(term == "Polarization2") %>%
  mutate(hypothesis = fct_relevel(outcome_labels[outcome], "Respondent affective polarization",
                                  "Respondent feeling thermometer"),
         model = paste(str_to_title(target), "polarization"))

H2_H3_plot_robust <- ggplot(split_models, aes(x = estimate, y = hypothesis, colour = condition)) +
  geom_point(position = position_dodge(0.25)) +
  geom_errorbar(aes(xmin = estimate - 1.645 * std.error, xmax = estimate + 1.645 * std.error),
                position = position_dodge(0.25), linewidth = 1) +
  geom_vline(xintercept = 0, linetype = "dashed") +
  facet_wrap(~ model, ncol = 1, scales = "free_y") +
  theme_paper() +
  theme(legend.position = "bottom") +
  labs(x = "Coefficient", y = "", colour = "")

save_figure(H2_H3_plot_robust, "figures/H2_H3_detailed.pdf")

#-------
# APPENDIX I
# blame_outgroup = 1 if the respondent blames the out-party; independents drop out
blame_models <- list(
  Perception = affective_polarization_perception ~ Polarization * blame_outgroup + Issue_conditions + Group + Quote,
  Index = affective_polarization ~ Polarization * blame_outgroup + Issue_conditions + Group + Quote,
  `Feeling thermometer` = feel_termometer ~ Polarization * blame_outgroup + Issue_conditions + Group + Quote
) %>%
  map(\(f) lm(f, data = W1, weights = weight))

modelsummary(blame_models,
             coef_omit = "Issue|Group|Quote",
             coef_rename = c("Polarization2" = "Polarization condition (ref. low)",
                             "blame_outgroup" = "Outpartisan blame (ref. not)",
                             "Polarization2:blame_outgroup" = "Polarization x Outpartisan blame"),
             stars = TRUE, gof_map = c("nobs", "r.squared"),
             output = "tables/blame_interaction_table.tex")
