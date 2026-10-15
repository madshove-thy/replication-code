#------------------------------------------------------------------
# 16 - H1 WITHOUT THE PROBLEMATIC WORD PAIRS (Appendix)
# Pair 1 (discussion / anger): "discussion" shares its stem with "discussed" in
#   the scaffolding sentence both articles contain.
# Pair 10 (dialogue / arguments): "argument" does not unambiguously signal
#   high polarization.
# H1 is re-estimated under four dictionaries that differ only in how these
# pairs enter.
#------------------------------------------------------------------

source("00_setup.R")

W1 <- readRDS("data/derived/W1_analysis.rds")
W2 <- readRDS("data/derived/W2_analysis.rds")
treatment_pairs <- readRDS("data/derived/treatment_pairs.rds")

pair_discussion <- which(treatment_pairs$low_word == "discussion")
pair_dialogue   <- which(treatment_pairs$low_word == "dialogue")

rules <- tribble(
  ~rule,    ~drop,                                ~unstem, ~label,
  "all",    integer(0),                           FALSE,   "All ten pairs\n(main text)",
  "nostem", integer(0),                           TRUE,    "All ten pairs,\ndiscussion not stemmed",
  "drop1",  pair_discussion,                      FALSE,   "Excluding\ndiscussion / anger",
  "drop2",  c(pair_discussion, pair_dialogue),    FALSE,   "Excluding\ndiscussion / anger and\ndialogue / arguments"
) %>%
  mutate(label = factor(label, levels = label))

# Surface forms and stems, aligned token by token
dual_tokens <- function(x) {
  toks <- tokens(as.character(x), remove_punct = TRUE, remove_symbols = TRUE, remove_numbers = TRUE) %>%
    tokens_tolower() %>%
    tokens_remove(stopwords("en"))
  list(surface = as.list(toks), stem = as.list(tokens_wordstem(toks, language = "english")))
}

tok_w1 <- dual_tokens(W1$retell_lower)
tok_w2 <- dual_tokens(W2$retell_lower)

#-------
# HOW MUCH OF THE "discuss" STEM IS THE TREATMENT WORD? (survey 1, low condition)
is_low <- W1$Polarization == "1"
discuss_forms <- unlist(map2(tok_w1$surface[is_low], tok_w1$stem[is_low], \(sf, st) sf[st == "discuss"]))
sort(table(discuss_forms), decreasing = TRUE)
mean(!discuss_forms %in% pair1_surface)   # share that is the scaffolding verb

#-------
# THE FOUR MEASURES
# Dropped pairs leave both dictionaries; `unstem` matches the low pair-1 word on
# its surface form instead of its stem
parrot_share_rule <- function(toks, pol, drop, unstem) {
  keep <- setdiff(seq_len(nrow(treatment_pairs)), drop)
  dict_low <- treatment_pairs$low_stem[keep]
  dict_high <- treatment_pairs$high_stem[keep]
  surface_low <- character(0)
  if (unstem && pair_discussion %in% keep) {
    dict_low <- setdiff(dict_low, "discuss")
    surface_low <- pair1_surface
  }

  hits <- pmap_dbl(list(toks$stem, toks$surface, pol), \(st, sf, p)
    if (p == "1") sum(st %in% dict_low) + sum(sf %in% surface_low) else sum(st %in% dict_high))
  n_tokens <- lengths(toks$stem)
  if_else(n_tokens > 0, hits / n_tokens, NA_real_)
}

# Article baseline under the same rule
article_share_rule <- function(text, dict_stem, drop) {
  if (!pair_discussion %in% drop) return(article_share(text, dict_stem))
  toks <- article_tokens(text)
  dropped <- c(treatment_pairs$low_stem[drop], treatment_pairs$high_stem[drop])
  sum(toks$stem %in% setdiff(dict_stem, dropped)) / length(toks$stem)
}

for (i in seq_len(nrow(rules))) {
  var <- paste0("pshare_", rules$rule[i])
  W1[[var]] <- parrot_share_rule(tok_w1, W1$Polarization, rules$drop[[i]], rules$unstem[i])
  W2[[var]] <- parrot_share_rule(tok_w2, W2$Polarization, rules$drop[[i]], rules$unstem[i])
}

#-------
# H1 UNDER EACH RULE
h1_rule <- function(df, var) {
  test <- fit_wls(reformulate("Polarization", var), df) %>% filter(term == "Polarization2")
  means <- summarise_by_condition(df, var)
  tibble(mean_low = means$estimate[1], se_low = means$se[1],
         mean_high = means$estimate[2], se_high = means$se[2],
         diff = -test$estimate, se_diff = test$std.error, p = test$p.value)   # low minus high
}

h1_results <- rules %>%
  select(rule, label) %>%
  expand_grid(survey = c("Survey 1", "Survey 2")) %>%
  mutate(res = map2(rule, survey, \(r, s) h1_rule(if (s == "Survey 1") W1 else W2, paste0("pshare_", r)))) %>%
  unnest(res) %>%
  mutate(star = stars(p))

h1_results

#-------
# FIGURE
baselines <- rules %>%
  mutate(res = map(drop, \(d) tibble(
    Polarization = c("Low", "High"),
    estimate = c(article_share_rule(stimulus$article_low, treatment_pairs$low_stem, d),
                 article_share_rule(stimulus$article_high, treatment_pairs$high_stem, d))))) %>%
  select(label, res) %>%
  unnest(res) %>%
  mutate(se = 0, survey = "Article")

track <- h1_results %>%
  select(label, survey, mean_low, se_low, mean_high, se_high) %>%
  pivot_longer(-c(label, survey), names_to = c(".value", "Polarization"), names_pattern = "(mean|se)_(low|high)") %>%
  rename(estimate = mean) %>%
  mutate(Polarization = str_to_title(Polarization)) %>%
  bind_rows(baselines) %>%
  mutate(survey = factor(survey, levels = c("Article", "Survey 1", "Survey 2")),
         Polarization = factor(Polarization, levels = c("Low", "High")))

track_plot <- ggplot(track, aes(x = survey, y = estimate * 100, colour = Polarization, group = Polarization)) +
  geom_line() +
  geom_point(size = 1) +
  geom_point(data = filter(track, survey == "Article"), colour = "black", size = 1) +
  geom_linerange(aes(ymin = (estimate - 1.645 * se) * 100, ymax = (estimate + 1.645 * se) * 100)) +
  geom_text(data = h1_results, aes(x = survey, y = mean_low * 100, label = star),
            nudge_y = 0.9, nudge_x = 0.1, colour = "black", size = 4, inherit.aes = FALSE) +
  facet_wrap(~ label, ncol = 2) +
  scale_colour_manual(values = pol_colours) +
  theme_paper() +
  theme(legend.position = "bottom", strip.text = element_text(size = 9, lineheight = 0.9),
        axis.text = element_text(size = 9)) +
  labs(x = "", y = "Percent of words", colour = "Treatment condition")

gap_plot <- ggplot(h1_results, aes(x = diff * 100, y = fct_rev(label), colour = survey)) +
  geom_vline(xintercept = 0, linetype = "dashed", colour = "grey40") +
  geom_linerange(aes(xmin = (diff - 1.645 * se_diff) * 100, xmax = (diff + 1.645 * se_diff) * 100),
                 position = position_dodge(width = 0.5)) +
  geom_point(size = 2, position = position_dodge(width = 0.5)) +
  geom_text(aes(label = sprintf("%.2f%s", diff * 100, star)), position = position_dodge(width = 0.5),
            vjust = -0.9, size = 3, show.legend = FALSE) +
  scale_colour_manual(values = c("Survey 1" = "grey20", "Survey 2" = "grey60")) +
  xlim(-0.3, 3.2) +
  theme_paper() +
  theme(legend.position = "bottom", axis.text.y = element_text(size = 8, lineheight = 0.9),
        axis.text.x = element_text(size = 9), axis.title.x = element_text(size = 10)) +
  labs(x = "Low minus high, percentage points (90% interval)", y = "", colour = "")

save_figure(gridExtra::arrangeGrob(track_plot, gap_plot, ncol = 1, heights = c(1.7, 1)),
            "figures/H1_appendix_dropped_pairs.pdf", height = 7.6)

#-------
# TABLE
h1_table <- h1_results %>%
  arrange(survey, label) %>%
  transmute(Dictionary = str_replace_all(label, "\n", " "),
            Survey = survey,
            `Low (%)` = round(mean_low * 100, 2),
            `High (%)` = round(mean_high * 100, 2),
            `Difference (pp)` = round(diff * 100, 2),
            `SE (pp)` = round(se_diff * 100, 2),
            p = ifelse(p < 0.001, "<0.001", sprintf("%.3f", p)))

datasummary_df(h1_table,
               title = "H1 re-estimated under four rules for the treatment word pairs (WLS).",
               output = "tables/H1_appendix_dropped_pairs.tex")

# Share of the main-text gap retained under each rule
h1_results %>%
  group_by(survey) %>%
  mutate(retained = diff / diff[rule == "all"]) %>%
  select(survey, rule, retained)
