#------------------------------------------------------------------
# 05 - DESCRIPTIVES AND BALANCE (Appendix B, C, D)
#------------------------------------------------------------------

source("00_setup.R")

W1 <- readRDS("data/derived/W1_analysis.rds")
W2 <- readRDS("data/derived/W2_analysis.rds")

#-------
# BALANCE (Appendix B)
# Joint F test of whether the covariates predict treatment assignment.
# Categorical covariates enter as factors.
balance <- W1 %>%
  mutate(across(c(gender, inputstate, educ, race, presvote24post), ~ factor(as.numeric(unclass(.x))))) %>%
  lm(Polarization == "2" ~ Tot_time + gender + age + inputstate + educ + race + presvote24post,
     data = ., weights = weight)

balance_test <- glance(balance) %>%
  transmute(F = sprintf("%.3f", statistic),
            df = sprintf("%d, %d", df, df.residual),
            p = ifelse(p.value < 0.001, "<0.001", sprintf("%.3f", p.value)),
            R2 = sprintf("%.4f", r.squared),
            N = nobs)

balance_test

datasummary_df(balance_test,
               title = "Balance check: joint F test of all covariates against an intercept-only model (WLS).",
               output = "tables/balance_table.tex")

#-------
# LENGTH OF THE RECOLLECTIONS (Appendix C, preregistered)
character_distribution <- W1 %>%
  mutate(Treatment = recode(Polarization, "1" = "Low-polarized article", "2" = "High-polarized article")) %>%
  ggplot(aes(x = O_nr_char, colour = Treatment)) +
  geom_density() +
  theme_paper() +
  labs(x = "Number of characters in recollections", y = "Density")

save_figure(character_distribution, "figures/D_character_distribution.pdf")

# Does recollection length differ across conditions?
map_dfr(list(chars_w1 = O_nr_char ~ Polarization, words_w1 = nr_words ~ Polarization,
             stems_w1 = nr_words_stemmed ~ Polarization),
        \(f) fit_wls(f, W1), .id = "outcome") %>%
  bind_rows(map_dfr(list(chars_w2 = O_nr_char ~ Polarization, stems_w2 = nr_words_stemmed_w2 ~ Polarization),
                    \(f) fit_wls(f, W2), .id = "outcome")) %>%
  filter(term == "Polarization2")

#-------
# DISTRIBUTION OF THE DEPENDENT VARIABLES (Appendix D)
# Values below zero: the respondent rates the out-party more warmly than the in-party
polarization_distribution <- W1 %>%
  pivot_longer(c(affective_polarization_perception, feel_termometer, affective_polarization)) %>%
  mutate(name = recode(name,
                       affective_polarization_perception = "Perceived affective polarization",
                       feel_termometer = "Respondent feeling thermometer",
                       affective_polarization = "Respondent affective polarization")) %>%
  ggplot(aes(x = value)) +
  geom_density() +
  geom_vline(xintercept = 0, linetype = "dotted") +
  facet_wrap(~ name) +
  theme_paper(11) +
  labs(x = "Polarization (low to high)", y = "Density")

save_figure(polarization_distribution, "figures/D_polarization_distribution.pdf")
