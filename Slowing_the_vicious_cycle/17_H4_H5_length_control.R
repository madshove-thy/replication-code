#------------------------------------------------------------------
# 17 - ARE THE H4 / H5 RESULTS AN ARTEFACT OF RECOLLECTION LENGTH? (Appendix)
# parrot_share is a ratio, so short recollections produce extreme values. Each
# H4 / H5 model is re-estimated with (1) the share, (2) the share plus a length
# control and (3) a binary indicator for any treatment word. Coefficients are
# standardised (beta x SD of the regressor) to put them on one axis.
#------------------------------------------------------------------

source("00_setup.R")

W1 <- readRDS("data/derived/W1_analysis.rds")
W2 <- readRDS("data/derived/W2_analysis.rds")

# How strongly is the share tied to its denominator?
W2 %>%
  filter(!is.na(parrot_share)) %>%
  summarise(cor_share_length = cor(parrot_share, nr_words_stemmed),
            cor_count_length = cor(parrot, nr_words_stemmed))

outcomes <- tribble(
  ~outcome,                            ~outcome_label,
  "affective_polarization_perception", "Perceived affective\npolarization",
  "feel_termometer",                   "Feeling thermometer",
  "affective_polarization",            "Affective polarization\n(index)"
)

specs <- tribble(
  ~spec,       ~rhs,                              ~term,           ~spec_label,
  "share",     "parrot_share",                    "parrot_share",  "Share of treatment words\n(main text)",
  "share_len", "parrot_share + nr_words_stemmed", "parrot_share",  "Share, controlling for\nrecollection length",
  "binary",    "parrot_binary",                   "parrot_binary", "Any treatment word\n(binary)"
)

# Same estimation sample for all three specifications
fit_spec <- function(outcome, condition, rhs, regressor) {
  d <- W2 %>% filter(Polarization == condition, !is.na(parrot_share), nr_words_stemmed > 0, !is.na(.data[[outcome]]))
  sd_x <- sd(d[[regressor]])
  fit_wls(as.formula(paste(outcome, "~", rhs)), d) %>%
    filter(term == regressor) %>%
    mutate(std_beta = estimate * sd_x, std_se = std.error * sd_x, n = nrow(d))
}

length_results <- expand_grid(outcomes, specs, condition = c("1", "2")) %>%
  mutate(res = pmap(list(outcome, condition, rhs, term), fit_spec)) %>%
  select(-term) %>%
  unnest(res) %>%
  mutate(condition = pol_lab(condition),
         outcome_label = factor(outcome_label, levels = outcomes$outcome_label),
         spec_label = factor(spec_label, levels = rev(specs$spec_label)))

length_results %>% select(outcome, condition, spec, estimate, std.error, p.value, n)

length_plot <- ggplot(length_results, aes(x = std_beta, y = spec_label, colour = condition)) +
  geom_vline(xintercept = 0, linetype = "dashed", colour = "grey40") +
  geom_linerange(aes(xmin = std_beta - 1.645 * std_se, xmax = std_beta + 1.645 * std_se)) +
  geom_point(size = 1.8) +
  geom_text(aes(label = sprintf("%.3f", std_beta)), vjust = -1.2, size = 2.6, show.legend = FALSE) +
  facet_grid(outcome_label ~ condition) +
  scale_colour_manual(values = pol_colours) +
  theme_paper(10) +
  theme(legend.position = "none", strip.text.y = element_text(size = 8)) +
  labs(x = "Change in outcome per 1 SD of transmitted treatment content (90% interval)", y = "")

save_figure(length_plot, "figures/H4_H5_length_control.pdf", width = 7.5, height = 6)

# Preregistered version (condition of the recollection read): length control only
outcomes %>%
  mutate(res = map(outcome, \(y) {
    d <- filter(W2, nr_words_stemmed > 0)
    bind_rows(bare = fit_wls(reformulate("Polarization", y), d),
              length_control = fit_wls(reformulate(c("Polarization", "nr_words_stemmed"), y), d),
              .id = "model")
  })) %>%
  unnest(res) %>%
  filter(term == "Polarization2") %>%
  select(outcome, model, estimate, p.value)

#-------
# TABLE
length_table <- length_results %>%
  arrange(outcome_label, condition, desc(spec_label)) %>%
  transmute(Outcome = str_replace_all(outcome_label, "\n", " "),
            Condition = condition,
            Specification = str_replace_all(spec_label, "\n", " "),
            Coefficient = sprintf("%.3f", estimate),
            SE = sprintf("%.3f", std.error),
            `Per 1 SD` = sprintf("%.3f", std_beta),
            p = ifelse(p.value < 0.001, "<0.001", sprintf("%.3f", p.value)),
            N = n)

datasummary_df(length_table,
               title = "H4 and H5 with the length of the recollection read handled three ways (WLS within condition).",
               output = "tables/H4_H5_length_control.tex")
