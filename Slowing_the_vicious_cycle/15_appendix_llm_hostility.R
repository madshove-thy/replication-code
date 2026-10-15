#------------------------------------------------------------------
# 15 - THE TREATMENT-BLIND HOSTILITY MEASURE (Appendix)
# One prompt for every text, without information about treatment assignment:
# how hostile does the text portray the Democrat-Republican relationship?
# 0 = entirely cooperative, 5 = neutral or not addressed, 10 = entirely hostile.
# Coded by 03_coding_LLM_hostility.R.
#------------------------------------------------------------------

source("00_setup.R")

W1 <- readRDS("data/derived/W1_analysis.rds")
W2 <- readRDS("data/derived/W2_analysis.rds")

hostility_articles <- readRDS("data/llm_annotation/hostility_articles.rds")

# addressed = the text says something about the relationship, in either direction
W1_h <- W1 %>%
  left_join(readRDS("data/llm_annotation/hostility_W1.rds"), by = "Record_no") %>%
  mutate(addressed = as.numeric(hostility_w1 != 5))

# hostility_read: the rating of the survey 1 recollection the respondent read
W2_h <- W2 %>%
  left_join(readRDS("data/llm_annotation/hostility_W2.rds"), by = "Record_no") %>%
  mutate(addressed = as.numeric(hostility_w2 != 5)) %>%
  left_join(W1_h %>% select(caseid, hostility_read = hostility_w1), by = c("W1_Resp_ID" = "caseid"))

# Anchor: the LLM's rating of the stimulus articles themselves
article_anchor <- hostility_articles %>%
  group_by(Polarization = as.character(Polarization)) %>%
  summarise(estimate = mean(hostility_article), se = 0, .groups = "drop") %>%
  mutate(level = "Article")

#-------
# H1: HOW MUCH OF THE ARTICLE'S HOSTILITY SURVIVES EACH RETELLING
h1 <- bind_rows(`Survey 1` = fit_wls(hostility_w1 ~ Polarization, W1_h),
                `Survey 2` = fit_wls(hostility_w2 ~ Polarization, W2_h),
                .id = "level") %>%
  filter(term == "Polarization2")

# Share of the article contrast left after one and two retellings
h1$estimate / diff(article_anchor$estimate)

chain <- bind_rows(
  article_anchor,
  summarise_by_condition(W1_h, "hostility_w1") %>% mutate(level = "Survey 1"),
  summarise_by_condition(W2_h, "hostility_w2") %>% mutate(level = "Survey 2")
) %>%
  mutate(level = factor(level, levels = c("Article", "Survey 1", "Survey 2")),
         Polarization = pol_lab(Polarization))

chain_stars <- chain %>%
  filter(level != "Article", Polarization == "High") %>%
  left_join(h1 %>% transmute(level, label = stars(p.value)), by = "level")

H1_hostility_plot <- ggplot(chain, aes(x = level, y = estimate, colour = Polarization, group = Polarization)) +
  geom_hline(yintercept = 5, linetype = "dotted", colour = "grey40") +
  geom_line() +
  geom_point(size = 1) +
  geom_errorbar(data = filter(chain, level != "Article"),
                aes(ymin = estimate - 1.645 * se, ymax = estimate + 1.645 * se), width = 0.08) +
  geom_text(data = chain_stars, aes(label = label), nudge_y = 0.55, nudge_x = 0.08, colour = "black", size = 4) +
  scale_colour_manual(values = pol_colours) +
  scale_y_continuous(limits = c(0, 10), breaks = seq(0, 10, 2)) +
  theme_paper() +
  theme(legend.position = "bottom") +
  labs(x = "", y = "LLM hostility rating (0-10)", colour = "Treatment condition")

save_figure(H1_hostility_plot, "figures/LLM2_H1_hostility_transmission.pdf")

#-------
# H1 DECOMPOSED: DOES THE TREATMENT CHANGE WHETHER OR HOW THE RELATIONSHIP IS DESCRIBED?
lab_share <- "Addresses the relationship (share)"
lab_tone  <- "Hostility if addressed (0-10)"

decomp_tests <- bind_rows(
  tibble(measure = lab_share, level = "Survey 1", fit_wls(addressed ~ Polarization, W1_h)),
  tibble(measure = lab_share, level = "Survey 2", fit_wls(addressed ~ Polarization, W2_h)),
  tibble(measure = lab_tone, level = "Survey 1", fit_wls(hostility_w1 ~ Polarization, filter(W1_h, addressed == 1))),
  tibble(measure = lab_tone, level = "Survey 2", fit_wls(hostility_w2 ~ Polarization, filter(W2_h, addressed == 1)))
) %>%
  filter(term == "Polarization2")

decomp <- bind_rows(
  summarise_by_condition(W1_h, "addressed") %>% mutate(measure = lab_share, level = "Survey 1"),
  summarise_by_condition(W2_h, "addressed") %>% mutate(measure = lab_share, level = "Survey 2"),
  summarise_by_condition(filter(W1_h, addressed == 1), "hostility_w1") %>% mutate(measure = lab_tone, level = "Survey 1"),
  summarise_by_condition(filter(W2_h, addressed == 1), "hostility_w2") %>% mutate(measure = lab_tone, level = "Survey 2")
) %>%
  mutate(Polarization = pol_lab(Polarization),
         measure = factor(measure, levels = c(lab_share, lab_tone)))

# Stars above the higher point in each panel; offset scaled to the panel's range
decomp_stars <- decomp %>%
  group_by(measure, level) %>%
  slice_max(estimate, n = 1, with_ties = FALSE) %>%
  ungroup() %>%
  left_join(decomp_tests %>% transmute(measure = factor(measure, levels = c(lab_share, lab_tone)), level,
                                       label = if_else(p.value < 0.05, stars(p.value), "n.s.")),
            by = c("measure", "level")) %>%
  mutate(estimate = estimate + if_else(measure == lab_share, 0.055, 0.55))

# Pins each panel to its full scale
decomp_range <- tibble(measure = factor(c(lab_share, lab_share, lab_tone, lab_tone), levels = c(lab_share, lab_tone)),
                       level = "Survey 1", Polarization = factor("Low", levels = c("Low", "High")),
                       estimate = c(0, 1, 0, 10))

decomp_plot <- ggplot(decomp, aes(x = level, y = estimate, colour = Polarization, group = Polarization)) +
  geom_hline(data = tibble(measure = factor(lab_tone, levels = c(lab_share, lab_tone))),
             aes(yintercept = 5), linetype = "dotted", colour = "grey40") +
  geom_blank(data = decomp_range) +
  geom_line() +
  geom_point(size = 1.5) +
  geom_errorbar(aes(ymin = estimate - 1.645 * se, ymax = estimate + 1.645 * se), width = 0.06) +
  geom_text(data = decomp_stars, aes(label = label), nudge_x = 0.12, colour = "black", size = 3.2,
            family = serif) +
  facet_wrap(~ measure, scales = "free_y") +
  scale_colour_manual(values = pol_colours) +
  theme_paper() +
  theme(legend.position = "bottom") +
  labs(x = "", y = "", colour = "Treatment condition")

save_figure(decomp_plot, "figures/LLM2_H1_hostility_decomposition.pdf")

#-------
# H2-H5 WITH THE HOSTILITY OF THE RECOLLECTION READ AS THE INDEPENDENT VARIABLE
# The rating is signed, so the models can also be pooled across conditions
fit_wls(affective_polarization_perception ~ hostility_read, W2_h)
fit_wls(feel_termometer ~ hostility_read, W2_h)

h2 <- fit_wls(affective_polarization_perception ~ Polarization, W1_h) %>% filter(term == "Polarization2")
h3 <- fit_wls(feel_termometer ~ Polarization, W1_h) %>% filter(term == "Polarization2")
h4 <- slopes_by_condition(W2_h, "affective_polarization_perception", "hostility_read")
h5 <- slopes_by_condition(W2_h, "feel_termometer", "hostility_read")

hostility_axis <- list(scale_x_continuous(breaks = seq(0, 10, 2)),
                       guides(colour = guide_legend(title = "", override.aes = list(fill = NA, alpha = 1, size = 2))))

H2_plot <- plot_condition_density(W1_h, "affective_polarization_perception",
                                  xlab = "Perception of affective polarization")

H4_plot <- plot_dose_response(W2_h, "hostility_read", "affective_polarization_perception", h4,
                              xlab = "Hostility of the recollection read (0-10)",
                              ylab = "Perception of affective polarization",
                              label_x = 5, label_y = c(0.97, 0.89), jitter = TRUE) +
  hostility_axis +
  coord_cartesian(xlim = c(0, 10))

save_figure(gridExtra::arrangeGrob(H2_plot, H4_plot, ncol = 2), "figures/LLM2_H2_H4_perception.pdf")

H3_plot <- plot_condition_density(W1_h, "feel_termometer", xlab = "Affective polarization",
                                  label_y = c(2, 1.8))

H5_plot <- plot_dose_response(W2_h, "hostility_read", "feel_termometer", h5,
                              xlab = "Hostility of the recollection read (0-10)", ylab = "Affective polarization",
                              label_x = 0.1, label_y = c(1.37, 1.17), coloured = TRUE, jitter = TRUE) +
  hostility_axis +
  scale_y_continuous(breaks = seq(-1, 1, 0.5)) +
  coord_cartesian(xlim = c(0, 10), ylim = c(-1.05, 1.47))

save_figure(gridExtra::arrangeGrob(H3_plot, H5_plot, ncol = 2), "figures/LLM2_H3_H5_polarization.pdf")

#-------
# VALIDITY: HOSTILITY AGAINST THE TWO FIDELITY MEASURES
# The fidelity measures are high whenever the treatment wording came through, so
# their correlation with hostility should be positive in the high condition and
# negative in the low condition.
lab_gpt    <- "LLM share of treatment sentences (gpt_verdict)"
lab_parrot <- "Bag-of-words share of treatment words (parrot_share)"

validity_long <- W1_h %>%
  filter(!is.na(hostility_w1)) %>%
  select(Polarization, weight, hostility_w1, gpt_verdict, parrot_share) %>%
  pivot_longer(c(gpt_verdict, parrot_share), names_to = "measure", values_to = "old_value") %>%
  filter(is.finite(old_value)) %>%
  mutate(Polarization = pol_lab(Polarization),
         measure = factor(recode(measure, gpt_verdict = lab_gpt, parrot_share = lab_parrot),
                          levels = c(lab_gpt, lab_parrot)))

validity <- validity_long %>%
  group_by(measure, Polarization) %>%
  summarise(n = n(), r = cor(hostility_w1, old_value), .groups = "drop") %>%
  mutate(label = sprintf("italic(r) == %.3f", r))

validity

validity_plot <- ggplot(validity_long, aes(x = old_value, y = hostility_w1, colour = Polarization)) +
  geom_hline(yintercept = 5, linetype = "dotted", colour = "grey40") +
  geom_jitter(width = 0.01, height = 0.25, alpha = 0.2, size = 0.6) +
  geom_smooth(aes(weight = weight), method = "lm", formula = y ~ x, linewidth = 1.2) +
  geom_text(data = validity, aes(x = -Inf, y = Inf, label = label),
            hjust = -0.2, vjust = 1.6, size = 3, parse = TRUE, inherit.aes = FALSE) +
  facet_grid(Polarization ~ measure, scales = "free_x", labeller = labeller(measure = label_wrap_gen(34))) +
  scale_colour_manual(values = pol_colours) +
  scale_y_continuous(breaks = seq(0, 10, 2)) +
  coord_cartesian(ylim = c(0, 10)) +
  theme_paper() +
  theme(legend.position = "none", strip.text.x = element_text(size = 8)) +
  labs(x = "Old measure (share of the recollection)", y = "LLM hostility rating (0-10)")

save_figure(validity_plot, "figures/LLM2_validity_vs_old_measures.pdf", height = fig_height * 1.5)
