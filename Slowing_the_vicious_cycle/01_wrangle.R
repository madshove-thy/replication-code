#------------------------------------------------------------------
# 01 - WRANGLE
# Builds the survey indices from the raw survey files. Run once, by hand:
# the LLM codes are added in 02_coding_LLM.R and 03_coding_LLM_hostility.R,
# and the bag-of-words measures in 04_text_measures.R.
#------------------------------------------------------------------

source("00_setup.R")
library(haven)
library(syuzhet)

W1 <- read_dta("data/raw/survey_wave1.dta") %>%
  mutate(Polarization = as.character(Polarization),
         Issue_conditions = as.character(as_factor(Issue_conditions)),
         across(c(Group, Quote), zap_labels),
         retell_lower = tolower(retell))

W2 <- read_dta("data/raw/survey_wave2.dta") %>%
  mutate(retell_lower = tolower(retell_w2_open))

#-------
# LENGTH AND SENTIMENT OF THE RECOLLECTIONS
add_text_basics <- function(df, suffix = "") {
  df <- df %>% mutate(O_nr_char = str_length(retell_lower),
                      nr_words = lengths(strsplit(retell_lower, "\\s+")))
  for (method in c("afinn", "bing", "nrc", "syuzhet")) {
    df[[paste0("sentiment_", method, suffix)]] <- get_sentiment(df$retell_lower, method = method)
  }
  df
}

#-------
# AFFECTIVE POLARIZATION
# Partisans: in-party minus out-party rating. Independents: absolute difference.
inparty_gap <- function(dem, rep, party) {
  case_when(party == 1 ~ dem - rep,
            party == 2 ~ rep - dem,
            !is.na(party) ~ abs(dem - rep))
}

# Items 1/2 concern voters, 3/4 politicians. 11 = "don't know". All indices are
# scaled 0-1, with higher values meaning more polarization.
add_polarization_indices <- function(df) {
  df %>%
    mutate(across(matches("^(per_likpol|per_respol)_[12]$|^Af_(honest|intel|mor)_[1-4]$"),
                  ~ ifelse(.x == 11, NA, .x))) %>%
    mutate(
      # Perceived polarization (0 = parties like/respect each other)
      affective_polarization_perception_voter = 1 - rowMeans(cbind(per_likpol_1, per_respol_1), na.rm = TRUE) / 10,
      affective_polarization_perception_elite = 1 - rowMeans(cbind(per_likpol_2, per_respol_2), na.rm = TRUE) / 10,
      affective_polarization_perception = 1 - rowMeans(cbind(per_likpol_1, per_respol_1, per_likpol_2, per_respol_2), na.rm = TRUE) / 10,

      # Feeling thermometer
      feel_termometer_voter = inparty_gap(feel_term_1, feel_term_2, Party) / 100,
      feel_termometer_elite = inparty_gap(feel_term_3, feel_term_4, Party) / 100,
      feel_termometer = rowMeans(cbind(feel_termometer_voter, feel_termometer_elite), na.rm = TRUE),

      # Honesty, intelligence and morality index
      Af_honest_voter = inparty_gap(Af_honest_1, Af_honest_2, Party),
      Af_honest_elite = inparty_gap(Af_honest_3, Af_honest_4, Party),
      Af_intel_voter  = inparty_gap(Af_intel_1, Af_intel_2, Party),
      Af_intel_elite  = inparty_gap(Af_intel_3, Af_intel_4, Party),
      Af_mor_voter    = inparty_gap(Af_mor_1, Af_mor_2, Party),
      Af_mor_elite    = inparty_gap(Af_mor_3, Af_mor_4, Party),
      affective_polarization_voter = rowMeans(cbind(Af_honest_voter, Af_intel_voter, Af_mor_voter), na.rm = TRUE) / 10,
      affective_polarization_elite = rowMeans(cbind(Af_honest_elite, Af_intel_elite, Af_mor_elite), na.rm = TRUE) / 10,
      affective_polarization = rowMeans(cbind(Af_honest_voter, Af_intel_voter, Af_mor_voter,
                                              Af_honest_elite, Af_intel_elite, Af_mor_elite), na.rm = TRUE) / 10
    )
}

#-------
# BLAME AND POLICY KNOWLEDGE
answer_key <- c(Rec1 = 3, Rec2 = 2, Rec3 = 4, Gun1 = 2, Gun2 = 3, Gun3 = 4,
                Tax1 = 4, Tax2 = 3, Tax3 = 1, Food1 = 2, Food2 = 2, Food3 = 2)

add_blame_and_knowledge <- function(df) {
  df %>%
    mutate(
      # 1 if the respondent blames the out-party; independents are NA
      blame_outgroup = case_when(Attri_blame == 1 & Party == 2 ~ 1,
                                 Attri_blame == 1 & Party == 1 ~ 0,
                                 Attri_blame == 2 & Party == 1 ~ 1,
                                 Attri_blame == 2 & Party == 2 ~ 0),
      across(all_of(names(answer_key)), ~ as.numeric(.x == answer_key[[cur_column()]]),
             .names = "{.col}_correct"),
      Rec_correct_index  = rowMeans(pick(matches("^Rec\\d_correct$"))),
      Gun_correct_index  = rowMeans(pick(matches("^Gun\\d_correct$"))),
      Tax_correct_index  = rowMeans(pick(matches("^Tax\\d_correct$"))),
      Food_correct_index = rowMeans(pick(matches("^Food\\d_correct$")))
    )
}

#-------
# APPLY
W1 <- W1 %>%
  add_text_basics() %>%
  add_polarization_indices() %>%
  add_blame_and_knowledge()

# Survey 2 respondents inherit the conditions (and recollection sentiment) of the
# survey 1 respondent whose recollection they read
W2 <- W2 %>%
  add_text_basics(suffix = "_w2") %>%
  add_polarization_indices() %>%
  add_blame_and_knowledge() %>%
  inner_join(W1 %>% select(caseid, Polarization, Issue_conditions, Group, Quote,
                           sentiment_afinn, sentiment_bing, sentiment_nrc, sentiment_syuzhet),
             by = c("W1_Resp_ID" = "caseid")) %>%
  arrange(W1_Resp_ID)

# Manipulation check: could the respondent recall the issue of the article?
W1 %>%
  mutate(issue = recode(as.vector(Issue_part_1), `1` = "recycling", `2` = "gun control", `3` = "taxation", `4` = "food"),
         recalled = recode(as.vector(Gen_know), `1` = "gun control", `2` = "food", `3` = "recycling", `4` = "taxation", `5` = "don't remember")) %>%
  count(passed = issue == recalled, dont_remember = recalled == "don't remember")

saveRDS(W1, "data/derived/W1_wrangled.rds")
saveRDS(W2, "data/derived/W2_wrangled.rds")
