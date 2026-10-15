#------------------------------------------------------------------
# 18 - IV / CACE ESTIMATES AND H1 IN SURVEY 2 NET OF THE INPUT (Appendix)
# Part 1: H4 and H5 as an encouragement design. The randomised instrument is
#         the condition of the recollection read (Z), the dose is the
#         polarizing content actually in it (D), and 2SLS gives the complier
#         average causal effect.
# Part 2: H1 in survey 2, accounting for the fact that low-condition
#         recollections already carried more treatment words into survey 2.
# All standard errors are clustered on the survey 1 writer, whose recollection
# was shown to up to six survey 2 respondents.
#------------------------------------------------------------------

source("00_setup.R")

W1 <- readRDS("data/derived/W1_analysis.rds")
W2 <- readRDS("data/derived/W2_analysis.rds")
treatment_pairs <- readRDS("data/derived/treatment_pairs.rds")

library(estimatr)

coef_row <- function(fit, term_name) tidy(fit) %>% filter(term == term_name)

iv_df <- W2 %>%
  mutate(Z = as.numeric(Polarization == "2"),
         writer = W1_Resp_ID,
         read_low_share = word_low_pol / nr_words_stemmed,
         read_high_share = word_high_pol / nr_words_stemmed,
         # Doses, all increasing in polarizing content
         dose_net = read_high_share - read_low_share,
         dose_net_binary = as.numeric(word_high_pol >= 1) - as.numeric(word_low_pol >= 1),
         dose_high = read_high_share,
         dose_low = read_low_share,
         dose_tone = -sentiment_syuzhet)

#==================================================================
# PART 1: IV / CACE
#==================================================================

#-------
# BALANCE ON Z: recollections read should differ only in polarizing content
balance_vars <- c(`Recollection read` = "nr_words_stemmed", `Recollection read` = "word_policy_share",
                  `Recollection read` = "sentiment_syuzhet", `Recollection read` = "sentiment_afinn",
                  `Recollection read` = "sentiment_bing", `Survey 2 respondent` = "age",
                  `Survey 2 respondent` = "educ_rc")

balance_z <- imap_dfr(balance_vars, function(v, kind) {
  d <- iv_df %>% mutate(x = as.numeric(unclass(.data[[v]]))) %>% filter(is.finite(x))
  m <- lm_robust(x ~ Z, data = d, weights = weight, clusters = writer, se_type = "CR2")
  tibble(variable = v, kind = kind,
         mean_low = weighted.mean(d$x[d$Z == 0], d$weight[d$Z == 0]),
         mean_high = weighted.mean(d$x[d$Z == 1], d$weight[d$Z == 1]),
         diff = coef_row(m, "Z")$estimate, p = coef_row(m, "Z")$p.value)
})

# Sentiment is not balanced: high-condition writers passed on the article's
# tone in their own words. The CACE per unit of treatment wording therefore
# absorbs the tone travelling with it; the ITT needs no such assumption.
balance_z

#-------
# FIRST STAGE, ITT AND CACE, estimated on the same rows
iv_pieces <- function(outcome, dose, outcome_label, dose_label) {
  d <- iv_df %>% filter(!is.na(.data[[outcome]]), !is.na(.data[[dose]]))
  cluster_fit <- function(f) lm_robust(f, data = d, weights = weight, clusters = writer, se_type = "CR2")

  fs <- coef_row(cluster_fit(reformulate("Z", dose)), "Z")
  itt <- coef_row(cluster_fit(reformulate("Z", outcome)), "Z")
  cace <- coef_row(iv_robust(as.formula(paste(outcome, "~", dose, "| Z")), data = d,
                             weights = weight, clusters = writer, se_type = "CR2"), dose)

  tibble(outcome = outcome_label, dose = dose_label, n = nrow(d), n_writers = n_distinct(d$writer),
         fs_est = fs$estimate, fs_se = fs$std.error, fs_F = (fs$estimate / fs$std.error)^2,
         itt_est = itt$estimate, itt_se = itt$std.error, itt_p = itt$p.value,
         cace_est = cace$estimate, cace_se = cace$std.error, cace_p = cace$p.value,
         cace_lo = cace$conf.low, cace_hi = cace$conf.high)
}

outcome_spec <- tribble(
  ~outcome,                            ~outcome_label,
  "affective_polarization_perception", "Perceived affective polarization",
  "feel_termometer",                   "Affective polarization (thermometer)",
  "affective_polarization",            "Affective polarization (index)"
)

dose_spec <- tribble(
  ~dose,             ~dose_label,
  "dose_net",        "Net polarizing word share",
  "dose_net_binary", "Net polarizing word present (-1/0/+1)",
  "dose_high",       "High-polarization word share",
  "dose_low",        "Low-polarization word share",
  "dose_tone",       "Transmitted negative tone (syuzhet, negated)"
)

iv_results <- expand_grid(outcome_spec, dose_spec) %>%
  pmap_dfr(\(outcome, outcome_label, dose, dose_label) iv_pieces(outcome, dose, outcome_label, dose_label))

main_iv <- filter(iv_results, dose == "Net polarizing word share")

main_iv %>% select(outcome, fs_est, fs_F, itt_est, itt_p, cace_est, cace_se, cace_p)
iv_results %>% select(outcome, dose, fs_F, cace_est, cace_se, cace_p)

#-------
# IV AGAINST THE WITHIN-CONDITION SLOPES FROM 07 AND 08
# The low-condition slope is sign-flipped so positive always means "more
# polarizing content raises the outcome"
within_condition <- expand_grid(outcome_spec, condition = c("1", "2")) %>%
  pmap_dfr(function(outcome, outcome_label, condition) {
    d <- iv_df %>% filter(Polarization == condition, !is.na(.data[[outcome]]))
    k <- coef_row(lm_robust(reformulate("parrot_share", outcome), data = d, weights = weight,
                            clusters = writer, se_type = "CR2"), "parrot_share")
    flip <- if (condition == "1") -1 else 1
    tibble(outcome = outcome_label,
           estimator = if (condition == "1") "Within condition\n(Low, sign-flipped)" else "Within condition\n(High)",
           estimate = flip * k$estimate, se = k$std.error,
           conf.low = if (flip == -1) -k$conf.high else k$conf.low,
           conf.high = if (flip == -1) -k$conf.low else k$conf.high)
  })

compare_df <- main_iv %>%
  transmute(outcome, estimator = "IV / CACE", estimate = cace_est, se = cace_se,
            conf.low = cace_lo, conf.high = cace_hi) %>%
  bind_rows(within_condition) %>%
  mutate(outcome = factor(outcome, levels = outcome_spec$outcome_label),
         estimator = factor(estimator, levels = c("IV / CACE", "Within condition\n(High)",
                                                  "Within condition\n(Low, sign-flipped)")))

CACE_plot <- ggplot(compare_df, aes(x = estimate, y = estimator, colour = estimator)) +
  geom_vline(xintercept = 0, linetype = "dashed", colour = "grey40") +
  geom_point(size = 1.8) +
  geom_linerange(aes(xmin = conf.low, xmax = conf.high)) +
  geom_linerange(aes(xmin = estimate - 1.645 * se, xmax = estimate + 1.645 * se), linewidth = 1.1) +
  facet_wrap(~ outcome, ncol = 1, scales = "free_x") +
  scale_colour_manual(values = c("black", pol_colours[["High"]], pol_colours[["Low"]])) +
  theme_paper() +
  theme(legend.position = "none", axis.text.y = element_text(size = 7.5, lineheight = 0.9),
        axis.text.x = element_text(size = 8.5), axis.title.x = element_text(size = 10),
        strip.text = element_text(size = 10)) +
  labs(x = "Effect of transmitted polarizing content (per unit of word share)", y = "")

save_figure(CACE_plot, "figures/H4_H5_CACE.pdf", height = 5.2)

cace_table <- main_iv %>%
  transmute(Outcome = outcome, N = n, Writers = n_writers,
            `First stage` = sprintf("%.4f (%.4f)", fs_est, fs_se),
            F = sprintf("%.0f", fs_F),
            ITT = sprintf("%.4f (%.4f)%s", itt_est, itt_se, stars(itt_p)),
            CACE = sprintf("%.3f (%.3f)%s", cace_est, cace_se, stars(cace_p)))

datasummary_df(cace_table,
               title = "H4 and H5 as an instrumental-variable estimate. Standard errors clustered on the survey 1 writer.",
               output = "tables/H4_H5_CACE.tex")

#==================================================================
# PART 2: H1 IN SURVEY 2, NET OF THE SURVEY 1 INPUT
#==================================================================

h1_df <- iv_df %>%
  filter(!is.na(parrot_share_w2), !is.na(parrot_share)) %>%
  mutate(low = as.numeric(Polarization == "1"),   # positive coefficients favour H1b
         parrot_share_c = parrot_share - weighted.mean(parrot_share, weight),
         attenuation = parrot_share_w2 - parrot_share,
         input_bin = factor(pmin(parrot, 3)))

# What survey 2 respondents read, what they wrote, and the share retained
asym <- h1_df %>%
  group_by(Polarization) %>%
  summarise(read_in = weighted.mean(parrot_share, weight),
            wrote_out = weighted.mean(parrot_share_w2, weight), .groups = "drop") %>%
  mutate(condition = pol_lab(Polarization), transmission_rate = wrote_out / read_in)

asym

#-------
# SPECIFICATION LADDER
cluster_fit <- function(f, data = h1_df) lm_robust(f, data = data, weights = weight, clusters = writer, se_type = "CR2")

ladder_models <- list(
  "(0) Raw: no adjustment"           = cluster_fit(parrot_share_w2 ~ low),
  "(1) + input dose"                 = cluster_fit(parrot_share_w2 ~ low + parrot_share),
  "(2) + input dose, interacted"     = cluster_fit(parrot_share_w2 ~ low * parrot_share_c),
  "(3) Attenuation (output - input)" = cluster_fit(attenuation ~ low),
  "(4) Read >=1 treatment word"      = cluster_fit(parrot_share_w2 ~ low, filter(h1_df, parrot_binary == 1)),
  "(5) Stratified on input count"    = cluster_fit(parrot_share_w2 ~ low + input_bin)
)

h1_ladder <- imap_dfr(ladder_models, \(m, spec) coef_row(m, "low") %>% mutate(spec = spec, n = m$nobs))

h1_ladder %>% select(spec, estimate, std.error, p.value, n)

# Output by condition within each input stratum; the zero-input stratum shows
# the background rate of each vocabulary, since there was nothing to transmit
h1_df %>%
  group_by(input_bin) %>%
  summarise(n_low = sum(low == 1), n_high = sum(low == 0),
            out_low = weighted.mean(parrot_share_w2[low == 1], weight[low == 1]),
            out_high = weighted.mean(parrot_share_w2[low == 0], weight[low == 0]),
            diff = out_low - out_high)

coef_row(cluster_fit(parrot_share_w2 ~ low, filter(h1_df, parrot == 0)), "low")

#-------
# TRANSMISSION RATE: RATIO OF MEANS WITH A WRITER-CLUSTERED BOOTSTRAP
boot_rate_diff <- function(data, B = 2000, seed = 20250915) {
  set.seed(seed)
  out <- data$parrot_share_w2
  inp <- data$parrot_share
  w <- data$weight
  is_low <- data$Polarization == "1"

  rate <- function(i) sum(w[i] * out[i]) / sum(w[i] * inp[i])
  rate_diff <- function(i) rate(i[is_low[i]]) - rate(i[!is_low[i]])

  by_writer <- split(seq_len(nrow(data)), data$writer)
  reps <- map_dbl(seq_len(B), \(b) rate_diff(unlist(sample(by_writer, replace = TRUE), use.names = FALSE)))

  tibble(rate_low = rate(which(is_low)), rate_high = rate(which(!is_low)),
         diff = rate_low - rate_high,
         ci_lo = quantile(reps, 0.025, na.rm = TRUE), ci_hi = quantile(reps, 0.975, na.rm = TRUE),
         p = 2 * min(mean(reps <= 0, na.rm = TRUE), mean(reps >= 0, na.rm = TRUE)))
}

boot_rate_diff(h1_df)

#-------
# CONDITIONAL TRANSMISSION: AMONG RESPONDENTS WHO READ A GIVEN WORD, HOW MANY REPRODUCED IT?
pair_df <- h1_df %>% left_join(W1 %>% select(caseid, read_retell = retell_lower), by = c("W1_Resp_ID" = "caseid"))
toks_read <- stem_tokens(pair_df$read_retell)
toks_own <- stem_tokens(pair_df$retell_lower)

pair_flags <- map(seq_len(nrow(treatment_pairs)), function(i) {
  own_stem <- if_else(pair_df$Polarization == "1", treatment_pairs$low_stem[i], treatment_pairs$high_stem[i])
  list(exposed = map2_lgl(toks_read, own_stem, \(tk, s) s %in% tk),
       reproduced = map2_lgl(toks_own, own_stem, \(tk, s) s %in% tk))
})

pair_results <- map_dfr(seq_len(nrow(treatment_pairs)), function(i) {
  d <- pair_df[pair_flags[[i]]$exposed, ] %>%
    mutate(y = as.numeric(pair_flags[[i]]$reproduced[pair_flags[[i]]$exposed]))
  out <- tibble(pair = i, words = paste(treatment_pairs$low_word[i], "/", treatment_pairs$high_word[i]),
                n_low = sum(d$low), n_high = sum(1 - d$low))
  if (min(out$n_low, out$n_high) < 10) return(out)
  k <- coef_row(cluster_fit(y ~ low, d), "low")
  out %>% mutate(diff = k$estimate, p = k$p.value)
})

pair_results

# Pooled: share of the distinct treatment words a respondent read that they reproduced
pooled_rate <- function(keep, label) {
  n_exposed <- rowSums(sapply(pair_flags[keep], \(z) z$exposed))
  n_reproduced <- rowSums(sapply(pair_flags[keep], \(z) z$exposed & z$reproduced))
  d <- pair_df %>% mutate(cond_rate = n_reproduced / n_exposed) %>% filter(n_exposed > 0)
  coef_row(cluster_fit(cond_rate ~ low, d), "low") %>% mutate(dictionary = label, .before = 1)
}

all_pairs <- seq_len(nrow(treatment_pairs))
bind_rows(pooled_rate(all_pairs, "All ten pairs"),
          pooled_rate(setdiff(all_pairs, 1), "Pair 1 dropped"),
          pooled_rate(setdiff(all_pairs, c(1, 10)), "Pairs 1 and 10 dropped"))

#-------
# FIGURE AND TABLE
panel_A <- asym %>%
  pivot_longer(c(read_in, wrote_out), names_to = "stage", values_to = "share") %>%
  mutate(stage = factor(stage, labels = c("Read\n(Survey 1)", "Wrote\n(Survey 2)"))) %>%
  ggplot(aes(x = stage, y = share * 100, colour = condition, group = condition)) +
  geom_line() +
  geom_point(size = 1.8) +
  geom_text(data = asym, aes(x = 2, y = wrote_out * 100, label = sprintf("%.0f%% retained", transmission_rate * 100)),
            nudge_y = 0.5, nudge_x = -0.25, size = 3, show.legend = FALSE) +
  scale_colour_manual(values = pol_colours) +
  theme_paper() +
  theme(legend.position = "bottom") +
  labs(x = "", y = "Percent of words", colour = "Treatment condition")

panel_B <- h1_ladder %>%
  mutate(spec = fct_rev(fct_inorder(spec))) %>%
  ggplot(aes(x = estimate * 100, y = spec)) +
  geom_vline(xintercept = 0, linetype = "dashed", colour = "grey40") +
  geom_point(size = 1.8) +
  geom_linerange(aes(xmin = conf.low * 100, xmax = conf.high * 100)) +
  geom_linerange(aes(xmin = (estimate - 1.645 * std.error) * 100, xmax = (estimate + 1.645 * std.error) * 100),
                 linewidth = 1.1) +
  theme_paper() +
  theme(axis.text.y = element_text(size = 8)) +
  labs(x = "Low minus high (percentage points of words)", y = "")

save_figure(gridExtra::arrangeGrob(panel_A, panel_B, ncol = 2, widths = c(1, 1.45)),
            "figures/H1_survey2_baseline.pdf", width = fig_width + 2.6, height = 3.4)

h1_ladder_table <- h1_ladder %>%
  transmute(Specification = spec,
            `Low - high` = sprintf("%.4f (%.4f)%s", estimate, std.error, stars(p.value)),
            `95% CI` = sprintf("[%.4f, %.4f]", conf.low, conf.high),
            N = n)

datasummary_df(h1_ladder_table,
               title = "H1 in survey 2 before and after accounting for the input dose. SEs clustered on the survey 1 writer.",
               output = "tables/H1_survey2_baseline.tex")
