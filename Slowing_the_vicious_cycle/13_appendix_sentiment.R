#------------------------------------------------------------------
# 13 - H1 WITH SENTIMENT ANALYSIS (Appendix F, Figure 6)
# Does the negative valence of the high-polarization article carry over into
# the recollections more readily than the positive valence of the low one?
#------------------------------------------------------------------

source("00_setup.R")

W1 <- readRDS("data/derived/W1_analysis.rds")
W2 <- readRDS("data/derived/W2_analysis.rds")

dictionaries <- c("afinn", "bing", "nrc", "syuzhet")

# Long format: one row per respondent and sentiment dictionary
sentiment_long <- function(df, suffix, survey) {
  df %>%
    select(Polarization, weight, all_of(paste0("sentiment_", dictionaries, suffix))) %>%
    pivot_longer(-c(Polarization, weight), names_to = "dictionary", values_to = "value") %>%
    mutate(dictionary = str_remove_all(dictionary, "^sentiment_|_w2$"),
           value = as.numeric(value),
           survey = survey)
}

sentiment <- bind_rows(sentiment_long(W1, "", "Survey 1"),
                       sentiment_long(W2, "_w2", "Survey 2")) %>%
  filter(!is.na(value))

# Condition means and the test of the difference (high minus low)
sentiment_means <- sentiment %>%
  group_by(survey, dictionary) %>%
  group_modify(\(d, key) summarise_by_condition(d, "value")) %>%
  ungroup() %>%
  mutate(Polarization = factor(pol_lab(Polarization), labels = c("Low polarized treatment", "High polarized treatment")))

sentiment_tests <- sentiment %>%
  group_by(survey, dictionary) %>%
  group_modify(\(d, key) fit_wls(value ~ Polarization, d) %>% filter(term == "Polarization2")) %>%
  ungroup()

sentiment_tests

H1_sentiment <- ggplot(sentiment_means, aes(x = estimate, y = dictionary)) +
  geom_vline(xintercept = 0, linetype = "dashed") +
  geom_linerange(aes(xmin = estimate - 1.96 * se, xmax = estimate + 1.96 * se)) +
  geom_point(aes(colour = Polarization), size = 2) +
  facet_wrap(~ survey, ncol = 2) +
  scale_colour_manual(values = unname(pol_colours)) +
  theme_paper() +
  theme(legend.title = element_blank(), legend.position = "bottom") +
  labs(x = "Average sentiment", y = "Sentiment dictionary")

save_figure(H1_sentiment, "figures/H1_sentiment.pdf", width = fig_width + 1.6)
