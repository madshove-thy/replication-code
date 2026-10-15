#------------------------------------------------------------------
# 06 - H1a / H1b: WHICH TREATMENT CONTENT IS TRANSMITTED? (Figure 1)
# Top panel:    share of treatment words in the article, in the survey 1
#               recollections and in the survey 2 recollections.
# Bottom panel: the ten matched treatment word pairs.
#------------------------------------------------------------------

source("00_setup.R")

W1 <- readRDS("data/derived/W1_analysis.rds")
W2 <- readRDS("data/derived/W2_analysis.rds")
treatment_pairs <- readRDS("data/derived/treatment_pairs.rds")

#-------
# ARTICLE BASELINE
baseline <- tibble(Polarization = c("1", "2"),
                   estimate = c(article_share(stimulus$article_low, treatment_pairs$low_stem),
                                article_share(stimulus$article_high, treatment_pairs$high_stem)),
                   se = 0,
                   survey = "Article")

baseline   # 10 treatment words in both articles, about 6.8 percent of tokens

#-------
# H1 TESTS (weighted OLS on the binary treatment)
h1 <- bind_rows(
  `Survey 1` = fit_wls(parrot_share ~ Polarization, W1),
  `Survey 2` = fit_wls(parrot_share_w2 ~ Polarization, W2),
  .id = "survey"
) %>%
  filter(term == "Polarization2")

h1

#-------
# TOP PANEL: ARTICLE -> SURVEY 1 -> SURVEY 2
track <- bind_rows(
  baseline,
  summarise_by_condition(W1, "parrot_share") %>% mutate(survey = "Survey 1"),
  summarise_by_condition(W2, "parrot_share_w2") %>% mutate(survey = "Survey 2")
) %>%
  mutate(survey = factor(survey, levels = c("Article", "Survey 1", "Survey 2")),
         Polarization = pol_lab(Polarization))

track_stars <- track %>%
  filter(Polarization == "Low", survey != "Article") %>%
  left_join(h1 %>% transmute(survey, label = stars(p.value, one = 0.1)), by = "survey")

H1_track_plot <- ggplot(track, aes(x = survey, y = estimate * 100, colour = Polarization, group = Polarization)) +
  geom_line() +
  geom_point(size = 1) +
  geom_point(data = filter(track, survey == "Article"), colour = "black", size = 1) +
  geom_linerange(aes(ymin = (estimate - 1.645 * se) * 100, ymax = (estimate + 1.645 * se) * 100)) +
  geom_text(data = track_stars, aes(label = label), nudge_y = 1.2, nudge_x = 0.08,
            colour = "black", size = 4) +
  scale_colour_manual(values = pol_colours) +
  theme_paper() +
  theme(legend.position = "none") +
  labs(x = "", y = "Percent of words")

#-------
# BOTTOM PANEL: THE TEN WORD PAIRS
# Each word is counted only among respondents in its own condition
pair_presence <- function(df, wave) {
  toks <- stem_tokens(df$retell_lower)
  is_low <- df$Polarization == "1"

  pmap_dfr(treatment_pairs, function(pair, low_stem, high_stem, low_word, high_word) {
    has_low  <- map_lgl(toks, \(tk) low_stem %in% tk)
    has_high <- map_lgl(toks, \(tk) high_stem %in% tk)
    present  <- as.numeric(if_else(is_low, has_low, has_high))
    p <- tidy(lm(present ~ is_low, weights = df$weight))$p.value[2]

    tibble(pair = pair, wave = wave, p = p,
           word = c(low_word, high_word),
           Polarization = factor(c("Low", "High"), levels = c("Low", "High")),
           avg = c(weighted.mean(present[is_low], df$weight[is_low]),
                   weighted.mean(present[!is_low], df$weight[!is_low])))
  })
}

words <- bind_rows(pair_presence(W1, "Individual words (Survey 1)"),
                   pair_presence(W2, "Individual words (Survey 2)")) %>%
  # The star belongs to the more frequently transmitted word of each pair
  group_by(wave, pair) %>%
  mutate(label = if_else(avg == max(avg), paste0(word, stars(p, one = 0.1)), word)) %>%
  ungroup()

# Pairs ordered by the size of the survey 1 gap, largest at the top
pair_order <- words %>%
  filter(wave == "Individual words (Survey 1)") %>%
  group_by(pair) %>%
  summarise(gap = max(avg) - min(avg)) %>%
  arrange(gap) %>%
  pull(pair)

H1_words_plot <- words %>%
  mutate(pair = factor(pair, levels = pair_order)) %>%
  ggplot(aes(x = avg * 100, y = pair)) +
  geom_line(aes(group = pair)) +
  geom_point(aes(colour = Polarization), size = 2) +
  geom_text_repel(aes(label = label), size = 3) +
  facet_wrap(~ wave, ncol = 2) +
  scale_colour_manual(values = pol_colours) +
  xlim(0, 27) +
  theme_paper() +
  theme(axis.text.y = element_blank(), axis.ticks.y = element_blank(), legend.position = "bottom") +
  labs(x = "Percent of recollections", y = "", colour = "Treatment condition")

save_figure(gridExtra::arrangeGrob(H1_track_plot, H1_words_plot, ncol = 1, heights = c(1, 3)),
            "figures/H1_OBBP.pdf", height = 8)

#-------
# DESCRIPTIVES QUOTED IN THE TEXT
# Share of recollections with at least one treatment word
bind_rows(`Survey 1` = summarise_by_condition(W1, "parrot_binary"),
          `Survey 2` = summarise_by_condition(W2, "parrot_binary_w2"),
          .id = "survey")
