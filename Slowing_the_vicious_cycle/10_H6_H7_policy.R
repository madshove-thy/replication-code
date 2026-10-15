#------------------------------------------------------------------
# 10 - H6 AND H7: TRANSMISSION OF, AND KNOWLEDGE ABOUT, POLICY CONTENT
# (Appendix J, Figure 12)
# H6: does the polarization treatment change how much policy content is passed on?
# H7: does it change factual knowledge about the policy?
#------------------------------------------------------------------

source("00_setup.R")

W1 <- readRDS("data/derived/W1_analysis.rds")
W2 <- readRDS("data/derived/W2_analysis.rds")

#-------
# H6
h6 <- list(
  `Survey 1` = fit_wls(word_policy_share ~ Polarization, W1),
  `Survey 1, design controls` = fit_wls(word_policy_share ~ Polarization + Issue_conditions + Group + Quote, W1),
  `Survey 2` = fit_wls(word_policy_share_w2 ~ Polarization, W2),
  `Survey 2, design controls` = fit_wls(word_policy_share_w2 ~ Polarization + Issue_conditions + Group + Quote, W2)
) %>%
  bind_rows(.id = "model") %>%
  filter(term == "Polarization2")

h6

#-------
# H7
# Each respondent saw one issue, so the four knowledge indices are stacked
stack_knowledge <- function(df) {
  df %>%
    select(Polarization, weight, word_policy_share, ends_with("_correct_index")) %>%
    pivot_longer(ends_with("_correct_index"), names_to = "issue", values_to = "knowledge") %>%
    drop_na(knowledge)
}

knowledge_w1 <- stack_knowledge(W1)
knowledge_w2 <- stack_knowledge(W2)

fit_wls(knowledge ~ Polarization, knowledge_w1)
fit_wls(knowledge ~ Polarization, knowledge_w2)
fit_wls(knowledge ~ Polarization * issue, knowledge_w1)   # differences across issues?

#-------
# FIGURE 12
policy_summary <- bind_rows(
  summarise_by_condition(W1, "word_policy_share") %>% mutate(wave = "Survey 1", q = "policy"),
  summarise_by_condition(W2, "word_policy_share_w2") %>% mutate(wave = "Survey 2", q = "policy"),
  summarise_by_condition(knowledge_w1, "knowledge") %>% mutate(wave = "Survey 1", q = "knowledge"),
  summarise_by_condition(knowledge_w2, "knowledge") %>% mutate(wave = "Survey 2", q = "knowledge")
) %>%
  mutate(Polarization = pol_lab(Polarization),
         q = factor(q, levels = c("policy", "knowledge"),
                    labels = c("Share of recollection included policy words",
                               "Knowledge questions answered correctly")))

H6_H7_plot <- ggplot(policy_summary, aes(x = wave, y = estimate * 100, fill = Polarization)) +
  geom_col(position = position_dodge(0.9)) +
  geom_errorbar(aes(ymin = (estimate - 1.645 * se) * 100, ymax = (estimate + 1.645 * se) * 100),
                position = position_dodge(0.9), width = 0.2) +
  facet_wrap(~ q) +
  scale_fill_manual(values = pol_colours) +
  theme_paper() +
  theme(legend.position = "bottom") +
  labs(x = "", y = "Percent", fill = "Treatment condition")

save_figure(H6_H7_plot, "figures/H6_H7_OBBP.pdf")

#-------
# PREREGISTERED EXPLORATORY TEST FOR H7
# Independent variable: the policy content of the recollection read in survey 2
knowledge_w2_finite <- filter(knowledge_w2, is.finite(word_policy_share))
fit_wls(knowledge ~ word_policy_share, knowledge_w2_finite)
fit_wls(knowledge ~ word_policy_share * Polarization, knowledge_w2_finite)
fit_wls(word_policy_share_w2 ~ word_policy_share,
        filter(W2, is.finite(word_policy_share), is.finite(word_policy_share_w2)))
