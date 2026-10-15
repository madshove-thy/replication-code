#------------------------------------------------------------------
# 04 - TEXT MEASURES
# Bag-of-words measures of how much of the article each recollection
# reproduces. Dictionaries are built from the stimulus text and matched as
# whole stemmed tokens. Also merges in the LLM codes from 02_coding_LLM.R.
#------------------------------------------------------------------

source("00_setup.R")

W1 <- readRDS("data/derived/W1_wrangled.rds") %>%
  left_join(readRDS("data/llm_annotation/llm_share_W1.rds"), by = "Record_no") %>%
  mutate(gpt_verdict = gpt_verdict / 100)

W2 <- readRDS("data/derived/W2_wrangled.rds") %>%
  left_join(readRDS("data/llm_annotation/llm_share_W2.rds"), by = "Record_no") %>%
  mutate(gpt_verdict_w2 = gpt_verdict_w2 / 100)

#-------
# DICTIONARIES
dict_descriptive <- stem_dict(stimulus$scaffolding)
dict_policy      <- stem_dict(stimulus$policy)
dict_voter       <- stem_dict("voter")
dict_pol         <- stem_dict("politician")

# "new", "north", "south" and "west" would over-match; the other half of each
# two-word state name is distinctive enough
dict_state <- setdiff(stem_dict(paste(state.name, collapse = " ")), c("new", "north", "south", "west"))

# The ten matched treatment word pairs; row k of low and high are counterparts
treatment_pairs <- tribble(
  ~low_word,      ~high_word,
  "discussion",   "anger",
  "peaceful",     "heated",
  "listened",     "refused",
  "happy",        "appalled",
  "civil",        "uncivil",
  "unemotional",  "emotional",
  "trust",        "distrust",
  "satisfied",    "disgusted",
  "constructive", "ridiculous",
  "dialogue",     "arguments"
) %>%
  mutate(pair = row_number(),
         low_stem = map_chr(low_word, \(w) stem_dict(w)[1]),
         high_stem = map_chr(high_word, \(w) stem_dict(w)[1]),
         .before = 1)

dict_low_pol  <- treatment_pairs$low_stem
dict_high_pol <- treatment_pairs$high_stem

#-------
# COUNTS, BINARIES AND SHARES
count_dict <- function(toks, dict) unname(map_dbl(toks, \(tk) sum(tk %in% dict)))

# `parrot` counts the treatment words of the respondent's own condition (in
# survey 2: the condition of the recollection they read)
add_text_measures <- function(df, suffix = "") {
  toks <- stem_tokens(df$retell_lower)
  n_tokens <- lengths(toks)

  counts <- tibble(
    word_descriptive = count_dict(toks, dict_descriptive),
    word_policy      = count_dict(toks, dict_policy),
    word_state       = count_dict(toks, dict_state),
    word_low_pol     = count_dict(toks, dict_low_pol),
    word_high_pol    = count_dict(toks, dict_high_pol),
    word_voter       = count_dict(toks, dict_voter),
    word_pol         = count_dict(toks, dict_pol)
  ) %>%
    mutate(parrot = if_else(df$Polarization == "1", word_low_pol, word_high_pol))

  measures <- counts %>%
    mutate(across(all_of(names(counts)), ~ as.integer(.x >= 1), .names = "{.col}_binary"),
           across(all_of(names(counts)), ~ if_else(n_tokens > 0, .x / n_tokens, NA_real_), .names = "{.col}_share"),
           word_other_share = 1 - (word_descriptive_share + word_policy_share + word_state_share +
                                     word_low_pol_share + word_high_pol_share + word_voter_share + word_pol_share),
           nr_words_stemmed = n_tokens) %>%
    rename_with(~ paste0(.x, suffix))

  bind_cols(df %>% select(-any_of(names(measures))), measures)
}

W1 <- add_text_measures(W1)

# In survey 2 the unsuffixed measures describe the survey 1 recollection the
# respondent READ, and the "_w2" measures their own recollection
W2 <- W2 %>%
  add_text_measures(suffix = "_w2") %>%
  left_join(W1 %>% select(caseid, parrot, parrot_binary, parrot_share, word_low_pol, word_high_pol,
                          word_policy, word_policy_share, nr_words_stemmed, gpt_verdict),
            by = c("W1_Resp_ID" = "caseid"))

saveRDS(W1, "data/derived/W1_analysis.rds")
saveRDS(W2, "data/derived/W2_analysis.rds")
saveRDS(treatment_pairs, "data/derived/treatment_pairs.rds")
