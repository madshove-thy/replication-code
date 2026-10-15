#------------------------------------------------------------------
# 00 - SETUP
# Packages, paths, stimulus texts and helper functions shared by all scripts.
# All paths are relative to the project folder.
#------------------------------------------------------------------

library(tidyverse)
library(quanteda)
library(broom)
library(ggrepel)
library(modelsummary)

options(scipen = 9)

walk(c("data/derived", "data/llm_annotation", "figures", "tables"), dir.create,
     recursive = TRUE, showWarnings = FALSE)

#-------
# STIMULUS TEXTS
# The mock news article, split into the parts the text measures are built from.
stimulus <- list(
  scaffolding = "New bill up for debate in This week, the State Legislature arranged a debate about a new policy proposal on The proposal has led to extensive local media coverage and between the political parties Before the bill was formally discussed, Democratic and Republican were gathered outside the state capitol building to express their opinions on the bill. The debate remained as both groups local news reporters After the debate, one said to a local news station: We clearly disagree on this issue On days like these, my in my opponents grows tremendously. Another said: I am we can find the best solution for the state of This is the third debate about a bill on since 2023. The following months will bring additional debates and hearings before the bill can come to a vote talk opponents",

  policy = c(
    recycling = "The initiative aims to strengthen different state recycling programs as current levels of recycling and conversion of waste into basic raw materials are well below state aims. The US has a combined recycling rate of only 32 percent for materials including glass, plastic, cardboard, and paper. Proponents highlight significant environmental and economic benefits of the legislation. Opponents stress that it imposes significant costs and administrative burdens on businesses and private citizens.",
    guns      = "The initiative aims to strengthen background check procedures, end gun show loopholes, and tighten requirements for private citizens to purchase firearms. About four-in-ten U.S. adults say they live in a household with a gun, and 32 percent say that they personally own one. Proponents highlight benefits of the legislation in terms of fewer accidents and mass shootings. Opponents stress that the state oversteps by infringing the constitutional rights of its citizens. control",
    taxation  = "The initiative aims to cut taxes for middle-class citizens in an effort to accommodate price hikes on consumer products over the last year. In March 2024, prices had increased by 3.5 percent compared to March 2023 according to the consumer price index. Proponents highlight the urgent need to help hard-working Americans following the recent inflation crisis. Opponents stress that the cuts will inevitably limit the budgets for public schools and transportation. middle class",
    food      = "The initiative aims to relax restrictions on food irradiation to allow manufacturers to remove bacteria that cause food poisoning. The global market for food irradiation is projected to grow at an annual rate of 4 percent until 2030. Proponents highlight that the technology yields significant health benefits and can ensure food safety. Opponents stress that it lowers the quality of the products and puts workers at risk of exposure to radiation."
  ),

  # Full articles (recycling version), used for the article baselines
  article_low = "New recycling bill up for debate in Wisconsin. This week, the Wisconsin State Legislature arranged a debate about a new policy proposal on recycling. The proposal has led to extensive local media coverage and discussion between the political parties. Before the bill was formally discussed, Democratic and Republican voters were gathered outside the state capitol building to express their opinions on the bill. The debate remained peaceful as both groups listened carefully while their opponents talked to local news reporters. The initiative aims to strengthen different state recycling programs as current levels of recycling and conversion of waste into basic raw materials are well below state aims. The US has a combined recycling rate of only 32 percent for materials including glass, plastic, cardboard, and paper. Proponents highlight significant environmental and economic benefits of the legislation. Opponents stress that it imposes significant costs and administrative burdens on businesses and private citizens. After the debate, one Democratic voter said to a local news station: We clearly disagree on this issue but I am happy that we manage to maintain a civil and unemotional tone. On days like these, my trust in my opponents grows tremendously. Another Democratic voter said: I am satisfied with the constructive dialogue, and I am sure we can find the best solution for the state of Wisconsin. This is the third debate about a bill on recycling since 2023. The following months will bring additional debates and hearings before the bill can come to a vote.",

  article_high = "New recycling bill up for debate in Wisconsin. This week, the Wisconsin State Legislature arranged a debate about a new policy proposal on recycling. The proposal has led to extensive local media coverage and anger between the political parties. Before the bill was formally discussed, Democratic and Republican voters were gathered outside the state capitol building to express their opinions on the bill. The debate remained heated as both groups refused their opponents the opportunity to talk to local news reporters. The initiative aims to strengthen different state recycling programs as current levels of recycling and conversion of waste into basic raw materials are well below state aims. The US has a combined recycling rate of only 32 percent for materials including glass, plastic, cardboard, and paper. Proponents highlight significant environmental and economic benefits of the legislation. Opponents stress that it imposes significant costs and administrative burdens on businesses and private citizens. After the debate, one Democratic voter said to a local news station: We clearly disagree on this issue and I am appalled that they resort to such an uncivil and emotional tone. On days like these, my distrust in my opponents grows tremendously. Another Democratic voter said: I am disgusted by their ridiculous arguments, and I do not think we can find the best solution for the state of Wisconsin. This is the third debate about a bill on recycling since 2023. The following months will bring additional debates and hearings before the bill can come to a vote."
)

#-------
# TEXT HELPERS
# Recollections and dictionaries go through the same pipeline, so stems match.
stem_tokens <- function(x) {
  tokens(as.character(x), remove_punct = TRUE, remove_symbols = TRUE, remove_numbers = TRUE) %>%
    tokens_tolower() %>%
    tokens_remove(stopwords("en")) %>%
    tokens_wordstem(language = "english") %>%
    as.list()
}

stem_dict <- function(text) unique(unlist(stem_tokens(text)))

# Article baselines: the share of the article's tokens that are treatment words.
# "discussion" (pair 1) shares its stem with "discussed" in the scaffolding both
# articles contain, so pair 1 is matched on the surface form.
article_tokens <- function(text) {
  toks <- tokens(text, remove_punct = TRUE, remove_symbols = TRUE, remove_numbers = TRUE) %>%
    tokens_tolower() %>%
    tokens_remove(stopwords("en"))
  list(surface = unlist(as.list(toks)),
       stem = unlist(as.list(tokens_wordstem(toks, language = "english"))))
}

pair1_surface <- c("discussion", "discussions")

article_share <- function(text, dict_stem) {
  toks <- article_tokens(text)
  n_stem <- sum(toks$stem %in% setdiff(dict_stem, "discuss"))
  n_pair1 <- if ("discuss" %in% dict_stem) sum(toks$surface %in% pair1_surface) else 0
  (n_stem + n_pair1) / length(toks$stem)
}

#-------
# ESTIMATION HELPERS
# Weighted least squares with the survey weights, returned as a tidy data frame
fit_wls <- function(formula, data) tidy(lm(formula, data = data, weights = weight))

# Standard error of a weighted mean
wmean_se <- function(x, w) sqrt(sum(w^2 * (x - weighted.mean(x, w))^2)) / sum(w)

# Weighted mean and standard error of `var` within each treatment condition
summarise_by_condition <- function(data, var) {
  data %>%
    filter(is.finite(.data[[var]])) %>%
    group_by(Polarization) %>%
    summarise(estimate = weighted.mean(.data[[var]], weight),
              se = wmean_se(.data[[var]], weight), .groups = "drop")
}

# Slope of `y` on `x` estimated separately within each treatment condition
slopes_by_condition <- function(data, y, x) {
  map_dfr(c(Low = "1", High = "2"),
          \(k) fit_wls(reformulate(x, y), filter(data, Polarization == k)) %>% filter(term == x),
          .id = "Polarization") %>%
    mutate(Polarization = factor(Polarization, levels = c("Low", "High")),
           label = coef_label(estimate, p.value))
}

stars   <- function(p, one = 0.05) case_when(p < 0.001 ~ "***", p < 0.01 ~ "**", p < one ~ "*", .default = "")
p_label <- function(p) ifelse(p < 0.001, "p < 0.001", sprintf("p = %.3f", p))

# Plotmath label for a slope, e.g. "beta == 0.566, italic(p) == 0.001" (needs parse = TRUE)
coef_label <- function(estimate, p) {
  p_part <- ifelse(p < 0.001, "italic(p) < 0.001", sprintf("italic(p) == %.3f", p))
  sprintf("beta == %.3f * \",\" ~ %s", estimate, p_part)
}

#-------
# PLOT DEFAULTS
fig_width  <- 5.7
fig_height <- 3.5
size       <- 12
serif      <- "serif"

pol_colours <- c(Low = "#00BFC4", High = "#F8766D")
pol_lab     <- function(x) factor(recode(as.character(x), "1" = "Low", "2" = "High"), levels = c("Low", "High"))

theme_paper <- function(base_size = size) {
  theme_bw() + theme(text = element_text(size = base_size, family = serif))
}

# Keeps the legend inside a half-width panel in the side-by-side figures
legend_fits <- theme(legend.title = element_text(size = 9.5),
                     legend.text = element_text(size = 9),
                     legend.key.width = grid::unit(0.9, "lines"),
                     legend.key.spacing.x = grid::unit(3, "pt"))

# cairo_pdf embeds the Symbol glyphs plotmath uses for Greek letters
save_figure <- function(plot, file, width = fig_width, height = fig_height) {
  device <- if (capabilities("cairo")) cairo_pdf else pdf
  device(file, width = width, height = height)
  on.exit(dev.off())
  if (inherits(plot, "gtable")) grid::grid.draw(plot) else print(plot)
}

# Density of a survey 1 outcome by condition, with condition means and the test
plot_condition_density <- function(data, y, xlab, test_p = NULL, label_x = 0.8,
                                   label_y = c(4, 3.7, 3.4), zero_line = FALSE) {
  data <- data %>% filter(!is.na(.data[[y]])) %>% mutate(Polarization = pol_lab(Polarization))
  means <- data %>%
    group_by(Polarization) %>%
    summarise(mean = weighted.mean(.data[[y]], weight), .groups = "drop") %>%
    mutate(label = sprintf("avg = %.3f", mean),
           label_pos = if_else(Polarization == "High", label_y[1], label_y[2]))

  p <- ggplot(data, aes(x = .data[[y]], fill = Polarization, colour = Polarization)) +
    geom_density(alpha = 0.5) +
    geom_vline(data = means, aes(xintercept = mean, colour = Polarization),
               linetype = "dashed", linewidth = 0.8) +
    geom_text(data = means, aes(x = label_x, y = label_pos, label = label),
              size = 3, inherit.aes = FALSE) +
    scale_fill_manual(values = pol_colours) +
    scale_colour_manual(values = pol_colours) +
    theme_paper(11) +
    theme(legend.position = "bottom") +
    labs(x = xlab, y = "Density") +
    guides(fill = guide_legend(title = "Treatment condition"), colour = "none") +
    legend_fits

  if (zero_line) p <- p + geom_vline(xintercept = 0, linetype = "dotted")
  if (!is.null(test_p)) p <- p + annotate("text", x = label_x, y = label_y[3], label = p_label(test_p), size = 3)
  p
}

# Survey 2 outcome against the content of the recollection read, one fit per condition.
# `coloured = TRUE` draws the slope labels left-aligned in the condition colours.
plot_dose_response <- function(data, x, y, slopes, xlab, ylab, label_x, label_y,
                               coloured = FALSE, jitter = FALSE) {
  data <- data %>%
    filter(!is.na(.data[[x]]), !is.na(.data[[y]])) %>%
    mutate(Polarization = pol_lab(Polarization))
  slopes <- slopes %>% mutate(label_pos = if_else(Polarization == "High", label_y[1], label_y[2]))

  points <- if (jitter) geom_jitter(width = 0.25, height = 0, alpha = 0.2, size = 0.6) else geom_point(alpha = 0.3, size = 0.7)
  fit    <- if (jitter) geom_smooth(aes(weight = weight), method = "lm", formula = y ~ x, linewidth = 1.5) else geom_smooth(method = "lm", formula = y ~ x, linewidth = 1.5)
  labels <- if (coloured) {
    geom_text(data = slopes, aes(x = label_x, y = label_pos, label = label, colour = Polarization),
              size = 2.8, parse = TRUE, hjust = 0, inherit.aes = FALSE, show.legend = FALSE)
  } else {
    geom_text(data = slopes, aes(x = label_x, y = label_pos, label = label),
              size = 3, parse = TRUE, inherit.aes = FALSE)
  }

  ggplot(data, aes(x = .data[[x]], y = .data[[y]], colour = Polarization)) +
    points + fit + labels +
    scale_colour_manual(values = pol_colours) +
    theme_paper(11) +
    theme(legend.position = "bottom") +
    labs(x = xlab, y = ylab, colour = "")
}
