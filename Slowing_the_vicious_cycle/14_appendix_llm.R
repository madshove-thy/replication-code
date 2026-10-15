#------------------------------------------------------------------
# 14 - H1 TO H5 WITH THE LLM MEASURE (Appendix)
# gpt_verdict is the share of a recollection that reproduces the treatment
# sentences, coded by 02_coding_LLM.R.
#------------------------------------------------------------------

source("00_setup.R")

W1 <- readRDS("data/derived/W1_analysis.rds")
W2 <- readRDS("data/derived/W2_analysis.rds")

# Baseline: 33 of the 227 words in the article are treatment wording
llm_baseline <- 33 / 227

#-------
# H1
h1_llm <- bind_rows(
  `Survey 1` = fit_wls(gpt_verdict ~ Polarization, W1),
  `Survey 2` = fit_wls(gpt_verdict_w2 ~ Polarization, W2),
  .id = "survey"
) %>%
  filter(term == "Polarization2")

h1_llm

llm_track <- bind_rows(
  tibble(Polarization = c("1", "2"), estimate = llm_baseline, se = 0, survey = "Baseline"),
  summarise_by_condition(W1, "gpt_verdict") %>% mutate(survey = "Survey 1"),
  summarise_by_condition(W2, "gpt_verdict_w2") %>% mutate(survey = "Survey 2")
) %>%
  mutate(survey = factor(survey, levels = c("Baseline", "Survey 1", "Survey 2")),
         Polarization = pol_lab(Polarization))

llm_stars <- llm_track %>%
  filter(Polarization == "Low", survey != "Baseline") %>%
  left_join(h1_llm %>% transmute(survey, label = stars(p.value, one = 0.1)), by = "survey")

H1_llm_plot <- ggplot(llm_track, aes(x = survey, y = estimate * 100, colour = Polarization, group = Polarization)) +
  geom_line() +
  geom_point(size = 1) +
  geom_point(data = filter(llm_track, survey == "Baseline"), colour = "black", size = 1) +
  geom_errorbar(data = filter(llm_track, survey != "Baseline"),
                aes(ymin = (estimate - 1.645 * se) * 100, ymax = (estimate + 1.645 * se) * 100),
                width = 0.08) +
  geom_text(data = llm_stars, aes(label = label), nudge_y = 1.2, nudge_x = 0.08, colour = "black", size = 4) +
  scale_colour_manual(values = pol_colours) +
  ylim(0, 20) +
  theme_paper() +
  theme(legend.position = "bottom") +
  labs(x = "", y = "Share", colour = "Treatment condition")

save_figure(H1_llm_plot, "figures/H1_llm.pdf")

#-------
# H2 AND H4
h2 <- fit_wls(affective_polarization_perception ~ Polarization, W1) %>% filter(term == "Polarization2")
h4_llm <- slopes_by_condition(W2, "affective_polarization_perception", "gpt_verdict")

H2_plot_llm <- plot_condition_density(W1, "affective_polarization_perception",
                                      xlab = "Perception of affective polarization", test_p = h2$p.value)

H4_plot_llm <- plot_dose_response(W2, "gpt_verdict", "affective_polarization_perception", h4_llm,
                                  xlab = "Polarization share in recollection",
                                  ylab = "Perception of affective polarization",
                                  label_x = 0.75, label_y = c(0.95, 0.90))

save_figure(gridExtra::arrangeGrob(H2_plot_llm, H4_plot_llm, ncol = 2), "figures/LLM_H2_H4_perception.pdf")

#-------
# H3 AND H5
h3 <- fit_wls(feel_termometer ~ Polarization, W1) %>% filter(term == "Polarization2")
h5_llm <- slopes_by_condition(W2, "feel_termometer", "gpt_verdict")

H3_plot_llm <- plot_condition_density(W1, "feel_termometer", xlab = "Affective polarization",
                                      test_p = h3$p.value, label_y = c(2, 1.8, 1.6))

# Slope labels sit in headroom above the data
H5_plot_llm <- plot_dose_response(W2, "gpt_verdict", "feel_termometer", h5_llm,
                                  xlab = "Polarization share in recollection", ylab = "Affective polarization",
                                  label_x = 0.02, label_y = c(1.37, 1.17), coloured = TRUE) +
  scale_y_continuous(breaks = seq(-1, 1, 0.5)) +
  coord_cartesian(ylim = c(-1.05, 1.47))

save_figure(gridExtra::arrangeGrob(H3_plot_llm, H5_plot_llm, ncol = 2), "figures/LLM_H3_H5_polarization.pdf")

#-------
# AGREEMENT WITH THE BAG-OF-WORDS MEASURE
cor(W1$gpt_verdict, W1$parrot_share, use = "complete.obs")
cor(W2$gpt_verdict_w2, W2$parrot_share_w2, use = "complete.obs")
