#------------------------------------------------------------------
# 19 - H1 WITH A SEEDED LDA (preregistered; Appendix)
# Seeds are taken from the stimulus article. Six topics: low-polarization
# wording, high-polarization wording, policy content, article scaffolding and
# two unseeded residual topics. The model is fitted once on both waves pooled,
# so topic proportions are comparable across surveys, and every quantity is
# averaged over five Gibbs chains.
#
# Main outcome, theta_matched: the share of a recollection assigned to the
# polarization topic of the respondent's own condition (the topic-model
# analogue of parrot_share).
#------------------------------------------------------------------

source("00_setup.R")

W1 <- readRDS("data/derived/W1_analysis.rds")
W2 <- readRDS("data/derived/W2_analysis.rds")
treatment_pairs <- readRDS("data/derived/treatment_pairs.rds")

library(seededlda)

set.seed(20260917)
quanteda_options(threads = 1)   # the thread count affects the sampler

#-------
# SEEDS
# A stem claimed by more than one part of the article seeds no topic. This also
# leaves "discuss" (pair 1, shared with the scaffolding) unseeded.
seed_parts <- list(low_pol = treatment_pairs$low_stem,
                   high_pol = treatment_pairs$high_stem,
                   policy = stem_dict(stimulus$policy),
                   scaffolding = stem_dict(stimulus$scaffolding))

ambiguous <- unique(unlist(seed_parts)[duplicated(unlist(seed_parts))])
seeds_main <- map(seed_parts, \(s) setdiff(s, ambiguous))

# Robustness: the preregistration read literally, one pooled polarization topic
seeds_pooled <- list(polarization = union(seeds_main$low_pol, seeds_main$high_pol),
                     policy = seeds_main$policy)

lengths(seeds_main)

#-------
# DOCUMENT-FEATURE MATRIX (both waves)
docs <- bind_rows(
  W1 %>% transmute(doc_id = paste0("w1_", caseid), text = retell_lower),
  W2 %>% transmute(doc_id = paste0("w2_", caseid_w2), text = retell_lower)
) %>%
  filter(!is.na(text), nchar(trimws(text)) > 0)

dfm_all <- tokens(docs$text, remove_punct = TRUE, remove_symbols = TRUE, remove_numbers = TRUE) %>%
  tokens_tolower() %>%
  tokens_remove(stopwords("en")) %>%
  tokens_wordstem(language = "english") %>%
  set_names(docs$doc_id) %>%
  dfm()

# Trim rare terms, but never a seed
keep_features <- union(featnames(dfm_trim(dfm_all, min_termfreq = 5)), unlist(c(seeds_main, seeds_pooled)))
dfm_all <- dfm_all %>%
  dfm_select(pattern = keep_features, selection = "keep", valuetype = "fixed") %>%
  dfm_subset(ntoken(.) > 0)

#-------
# FIT
# weight = 0.05 and residual = 2 keep the ten-seed polarization topics from being
# absorbed by the much larger policy vocabulary (checked against the defaults below)
chain_seeds <- 20260917L + 0:4

fit_chains <- function(seeds, rng_seeds = chain_seeds, residual = 2, weight = 0.05) {
  fits <- map(rng_seeds, function(s) {
    set.seed(s)
    textmodel_seededlda(dfm_all, dictionary = dictionary(seeds), valuetype = "fixed",
                        residual = residual, weight = weight, verbose = FALSE)
  })

  # Seeded topics keep their labels across chains; unseeded residual topics can
  # swap, so they are summed before averaging
  seeded <- names(seeds)
  theta_seeded <- reduce(map(fits, \(f) f$theta[, seeded, drop = FALSE]), `+`) / length(fits)
  theta_residual <- reduce(map(fits, \(f) rowSums(f$theta[, setdiff(colnames(f$theta), seeded), drop = FALSE])), `+`) / length(fits)

  list(fits = fits, theta = cbind(theta_seeded, residual = theta_residual))
}

chains_main <- fit_chains(seeds_main)
chains_pooled <- fit_chains(seeds_pooled)

terms(chains_main$fits[[1]], 15)

# Anchoring: probability mass each seeded topic puts on its own seeds and on policy seeds
seed_mass <- function(fit, topic, dict) sum(fit$phi[topic, intersect(dict, colnames(fit$phi))])

map_dfr(names(seeds_main), function(k) {
  own <- map_dbl(chains_main$fits, seed_mass, topic = k, dict = seeds_main[[k]])
  leak <- map_dbl(chains_main$fits, seed_mass, topic = k, dict = seeds_main$policy)
  tibble(topic = k, own_min = min(own), own_max = max(own), policy_min = min(leak), policy_max = max(leak))
})

# Diagnostic: do the seeded topics recover the stimulus articles?
dfm_article <- tokens(c(low = stimulus$article_low, high = stimulus$article_high),
                      remove_punct = TRUE, remove_symbols = TRUE, remove_numbers = TRUE) %>%
  tokens_tolower() %>%
  tokens_remove(stopwords("en")) %>%
  tokens_wordstem(language = "english") %>%
  dfm() %>%
  dfm_match(features = featnames(dfm_all))

map(chains_main$fits, \(f) textmodel_lda(dfm_article, model = f, verbose = FALSE)$theta[, names(seeds_main)]) %>%
  reduce(`+`) %>%
  `/`(length(chain_seeds)) %>%
  round(3)

#-------
# MERGE TOPIC MEASURES ONTO THE SURVEYS
dominant <- function(theta) colnames(theta)[max.col(theta, ties.method = "first")]

theta <- as_tibble(chains_main$theta, rownames = "doc_id") %>%
  rename_with(~ paste0("theta_", .x), -doc_id) %>%
  mutate(lda_top = dominant(chains_main$theta),
         theta_polarization_pooled = chains_pooled$theta[, "polarization"],
         lda_top_pooled = dominant(chains_pooled$theta))

saveRDS(theta, "data/derived/seeded_lda_theta.rds")

add_lda_measures <- function(df, id_col, prefix) {
  df %>%
    mutate(doc_id = paste0(prefix, .data[[id_col]])) %>%
    left_join(theta, by = "doc_id") %>%
    mutate(theta_matched = if_else(Polarization == "1", theta_low_pol, theta_high_pol),
           theta_pol_total = theta_low_pol + theta_high_pol,
           lda_pol_dominant = if_else(is.na(lda_top), NA_integer_, as.integer(lda_top %in% c("low_pol", "high_pol"))),
           lda_matched_dominant = as.integer(lda_top == if_else(Polarization == "1", "low_pol", "high_pol")),
           lda_pol_dominant_pooled = as.integer(lda_top_pooled == "polarization")) %>%
    select(-doc_id)
}

W1 <- add_lda_measures(W1, "caseid", "w1_")
W2 <- add_lda_measures(W2, "caseid_w2", "w2_")

# Convergent validity with the other text measures
tibble(comparison = c("parrot_share (S1)", "gpt_verdict (S1)", "parrot_share (S2)", "gpt_verdict (S2)", "pooled model (S1)"),
       r = c(cor(W1$theta_matched, W1$parrot_share, use = "complete.obs"),
             cor(W1$theta_matched, W1$gpt_verdict, use = "complete.obs"),
             cor(W2$theta_matched, W2$parrot_share_w2, use = "complete.obs"),
             cor(W2$theta_matched, W2$gpt_verdict_w2, use = "complete.obs"),
             cor(W1$theta_matched, W1$theta_polarization_pooled, use = "complete.obs")))

#-------
# H1 TESTS
# Positive = high-condition wording travels further (H1a); negative = H1b.
# d_std divides by the low condition's weighted SD so measures share one axis.
weighted_sd <- function(x, w) sqrt(sum(w * (x - weighted.mean(x, w))^2) / sum(w))

h1_test <- function(df, outcome, survey, measure) {
  d <- filter(df, !is.na(.data[[outcome]]))
  test <- fit_wls(reformulate("Polarization", outcome), d) %>% filter(term == "Polarization2")
  low <- d$Polarization == "1"
  sd_low <- weighted_sd(d[[outcome]][low], d$weight[low])
  tibble(survey = survey, measure = measure,
         low = weighted.mean(d[[outcome]][low], d$weight[low]),
         high = weighted.mean(d[[outcome]][!low], d$weight[!low]),
         estimate = test$estimate, se = test$std.error, p = test$p.value, n = nrow(d),
         d_std = estimate / sd_low, d_std_se = se / sd_low)
}

lda_measures <- c(theta_matched = "Matched polarization topic (share)",
                  lda_pol_dominant = "Polarization topic dominant (0/1)",
                  lda_matched_dominant = "Matched topic dominant (0/1)",
                  theta_polarization_pooled = "Pooled polarization topic, 3-topic model (share)")

h1_lda <- imap_dfr(lda_measures, \(label, var) bind_rows(h1_test(W1, var, "Survey 1", label),
                                                         h1_test(W2, var, "Survey 2", label))) %>%
  mutate(support = case_when(p >= 0.05 ~ "neither", estimate > 0 ~ "H1a", TRUE ~ "H1b"))

h1_lda

# Stability: each chain alone, the package defaults, and "discuss" seeded
h1_from_theta <- function(theta, label) {
  d <- W1 %>%
    transmute(doc_id = paste0("w1_", caseid), Polarization, weight) %>%
    left_join(as_tibble(theta, rownames = "doc_id"), by = "doc_id") %>%
    mutate(matched = if_else(Polarization == "1", low_pol, high_pol))
  fit_wls(matched ~ Polarization, d) %>% filter(term == "Polarization2") %>% mutate(specification = label, .before = 1)
}

bind_rows(
  imap_dfr(chains_main$fits, \(f, i) h1_from_theta(f$theta, paste("Chain", i, "alone"))),
  h1_from_theta(chains_main$theta, "Average of five chains (used)"),
  h1_from_theta(fit_chains(seeds_main, chain_seeds[1:3], residual = 1, weight = 0.01)$theta,
                "seededlda defaults (weight 0.01, residual 1)"),
  h1_from_theta(fit_chains(modifyList(seeds_main, list(low_pol = union(seeds_main$low_pol, "discuss"))),
                           chain_seeds[1:3])$theta,
                "\"discuss\" seeded into low_pol")
)

#-------
# FIGURE 1: THE H1 VERDICT AND THE EXCEEDANCE CURVES
benchmarks <- bind_rows(
  h1_test(W1, "parrot_share", "Survey 1", "Share of treatment words (dictionary)"),
  h1_test(W2, "parrot_share_w2", "Survey 2", "Share of treatment words (dictionary)"),
  h1_test(W1, "gpt_verdict", "Survey 1", "Treatment content (LLM)"),
  h1_test(W2, "gpt_verdict_w2", "Survey 2", "Treatment content (LLM)")
)

verdict_df <- bind_rows(h1_lda, benchmarks) %>%
  mutate(measure = factor(measure, levels = rev(c(lda_measures[c(1, 3, 2, 4)],
                                                  "Share of treatment words (dictionary)",
                                                  "Treatment content (LLM)"))),
         family = if_else(measure %in% lda_measures, "Seeded LDA", "Other measures"))

panel_a <- ggplot(verdict_df, aes(x = d_std, y = measure, colour = family)) +
  geom_vline(xintercept = 0, linewidth = 0.4) +
  geom_linerange(aes(xmin = d_std - 1.96 * d_std_se, xmax = d_std + 1.96 * d_std_se)) +
  geom_point(size = 2) +
  geom_text(data = tibble(survey = "Survey 1", x = c(-0.36, 0.36), label = c("supports~H1b", "supports~H1a")),
            aes(x = x, y = 6.75, label = label), inherit.aes = FALSE, size = 2.9, family = serif, parse = TRUE) +
  facet_wrap(~ survey, ncol = 2) +
  scale_colour_manual(values = c("Seeded LDA" = "black", "Other measures" = "grey55")) +
  scale_y_discrete(labels = \(x) sub(", 3-topic model", ",\n3-topic model", x)) +
  coord_cartesian(xlim = c(-0.5, 0.5), ylim = c(0.5, 7.1), clip = "off") +
  theme_paper() +
  theme(legend.position = "bottom", legend.title = element_blank(), panel.grid.minor = element_blank()) +
  labs(x = "Standardised difference, high minus low condition", y = "")

# Share of recollections whose matched topic is at least t. Conditions on nothing,
# so the comparison stays randomised at every t.
exceedance <- function(df, survey) {
  d <- filter(df, !is.na(theta_matched))
  expand_grid(Polarization = c("1", "2"), t = seq(0, 0.6, by = 0.005)) %>%
    mutate(survey = survey,
           share = map2_dbl(Polarization, t, \(k, tt) {
             in_k <- d$Polarization == k
             weighted.mean(d$theta_matched[in_k] >= tt, d$weight[in_k])
           }))
}

exceed_df <- bind_rows(exceedance(W1, "Survey 1"), exceedance(W2, "Survey 2")) %>%
  mutate(Polarization = pol_lab(Polarization))

# First sign change in (low - high) above the floor near t = 0
crossings <- exceed_df %>%
  filter(t > 0.02) %>%
  pivot_wider(names_from = Polarization, values_from = share) %>%
  group_by(survey) %>%
  filter(sign(Low - High) != lag(sign(Low - High))) %>%
  slice(1) %>%
  ungroup()

panel_b <- ggplot(exceed_df, aes(x = t * 100, y = share * 100, colour = Polarization)) +
  geom_line(linewidth = 0.7) +
  geom_vline(data = crossings, aes(xintercept = t * 100), linetype = "dashed", linewidth = 0.3, colour = "grey40") +
  geom_text(data = crossings, aes(x = t * 100, y = 88, label = "curves cross"), inherit.aes = FALSE,
            hjust = -0.08, size = 2.7, family = serif, colour = "grey30") +
  facet_wrap(~ survey, ncol = 2) +
  scale_colour_manual(values = pol_colours) +
  theme_paper() +
  theme(legend.position = "bottom", panel.grid.minor = element_blank()) +
  labs(x = "Matched topic is at least this percent of the recollection",
       y = "Percent of\nrecollections", colour = "Treatment condition")

save_figure(gridExtra::arrangeGrob(panel_a, panel_b, ncol = 1, heights = c(1.5, 1)),
            "figures/H1_seeded_lda.pdf", width = fig_width + 1.6, height = 6.6)

#-------
# FIGURE 2: THE MATCHED COMPARISON, NET OF TOPIC SIZE
# Each polarization topic is shown in both conditions. The matched cells are the
# H1 comparison; the mismatched cells give each topic's prevalence without the
# treatment, so each dumbbell's length is a randomised effect on one topic.
# (a - c) - (d - b) = (a + b) - (c + d), so the net test is a single regression
# on theta_pol_total.
topic_cells <- expand_grid(survey = c("Survey 1", "Survey 2"), topic = c("theta_low_pol", "theta_high_pol")) %>%
  mutate(res = map2(survey, topic, \(s, tp) summarise_by_condition(if (s == "Survey 1") W1 else W2, tp))) %>%
  unnest(res) %>%
  mutate(condition = pol_lab(Polarization),
         matched = (topic == "theta_low_pol" & condition == "Low") | (topic == "theta_high_pol" & condition == "High"),
         topic_lab = factor(topic, levels = c("theta_high_pol", "theta_low_pol"),
                            labels = c("High-polarization\ntopic", "Low-polarization\ntopic")))

test_label <- function(prefix, estimate, p) {
  sprintf("'%s'~%+.1f~'pts,'~%s", prefix, estimate * 100,
          ifelse(p < 0.001, "italic(p) < 0.001", sprintf("italic(p) == %.3f", p)))
}

matched_test <- h1_lda %>%
  filter(measure == lda_measures[["theta_matched"]]) %>%
  mutate(label = test_label("matched cells:", estimate, p))

net_test <- bind_rows(h1_test(W1, "theta_pol_total", "Survey 1", "Total polarization topic mass"),
                      h1_test(W2, "theta_pol_total", "Survey 2", "Total polarization topic mass")) %>%
  mutate(label = test_label("net of topic size:", estimate, p))

dumbbells <- topic_cells %>%
  select(survey, topic, topic_lab, condition, estimate) %>%
  pivot_wider(names_from = condition, values_from = estimate) %>%
  mutate(boost = if_else(topic == "theta_low_pol", Low - High, High - Low))

dumbbells

matched_plot <- ggplot(topic_cells, aes(x = estimate * 100, y = topic_lab)) +
  geom_line(aes(group = topic_lab), colour = "grey55", linewidth = 0.5) +
  geom_linerange(aes(xmin = (estimate - 1.96 * se) * 100, xmax = (estimate + 1.96 * se) * 100, colour = condition),
                 show.legend = FALSE) +
  geom_point(aes(colour = condition, shape = matched), size = 2.7) +
  geom_text(data = dumbbells, aes(x = (Low + High) / 2 * 100, y = topic_lab, label = sprintf("%+.1f pts", boost * 100)),
            inherit.aes = FALSE, nudge_y = 0.17, size = 2.5, family = serif, colour = "grey25") +
  geom_text(data = matched_test, aes(x = 6.5, y = 2.80, label = label), inherit.aes = FALSE,
            parse = TRUE, hjust = 0, size = 2.4, family = serif) +
  geom_text(data = net_test, aes(x = 6.5, y = 2.62, label = label), inherit.aes = FALSE,
            parse = TRUE, hjust = 0, size = 2.4, family = serif) +
  facet_wrap(~ survey, ncol = 2) +
  scale_colour_manual(values = pol_colours) +
  scale_shape_manual(values = c(`TRUE` = 16, `FALSE` = 1), guide = "none") +
  coord_cartesian(xlim = c(6.5, 18.5), ylim = c(0.78, 2.88), clip = "off") +
  theme_paper() +
  theme(legend.position = "bottom", panel.grid.minor = element_blank(),
        axis.text.y = element_text(size = 8.5, lineheight = 0.9), axis.text.x = element_text(size = 9),
        axis.title.x = element_text(size = 10), strip.text = element_text(size = 10)) +
  labs(x = "Percent of the recollection", y = "") +
  guides(colour = guide_legend(title = "Treatment condition", override.aes = list(shape = 16)))

save_figure(matched_plot, "figures/H1_seeded_lda_matched.pdf", height = fig_height + 0.5)

#-------
# TABLE
h1_lda_table <- h1_lda %>%
  transmute(Survey = survey, Measure = measure,
            Low = sprintf("%.3f", low), High = sprintf("%.3f", high),
            `High - low` = sprintf("%.3f", estimate), SE = sprintf("%.3f", se),
            p = ifelse(p < 0.001, "<0.001", sprintf("%.3f", p)), N = n, Supports = support)

datasummary_df(h1_lda_table,
               title = "H1a and H1b tested with a seeded LDA fitted on both waves (average of five chains, WLS).",
               output = "tables/H1_seeded_lda.tex")
