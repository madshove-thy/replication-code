#------------------------------------------------------------------
# 02 - LLM CODING OF THE RECOLLECTIONS
# Codes the share (0-100) of each recollection that reproduces the polarization
# content of the article version the respondent was assigned to. The prompt
# lists the six treatment elements of that version, so each condition is coded
# against its own stimulus. Survey 2 respondents are coded against the
# condition of the survey 1 recollection they read.
#
# Calls the paid OpenAI API (gpt-4.1-mini). Requires the OPENAI_API_KEY
# environment variable. Answers are saved as the coding runs, so an
# interrupted run continues where it stopped.
#------------------------------------------------------------------

source("00_setup.R")
library(openai)

stopifnot("Set the OPENAI_API_KEY environment variable" = Sys.getenv("OPENAI_API_KEY") != "")

W1 <- readRDS("data/derived/W1_wrangled.rds")
W2 <- readRDS("data/derived/W2_wrangled.rds")

#-------
# PROMPTS
# One prompt per condition ("1" = low polarization, "2" = high polarization)
prompt_intro <- paste(
  "Answer with just the numeric evaluation, do not provide any reasoning.",
  "An individual has written a recollection of a news article they have read.",
  "You need to evaluate the share (from 0 to 100) of the recollection that includes",
  "the following examples from the news article:"
)

prompt_outro <- paste(
  "Assign the value 0 if the recollection, for instance, only refers to the topic",
  "(gun control, food irradication, taxes, recycling) or only non-relevant factors",
  "signaling non-compliance (for instance, 'not sure', 'yes', 'don't remember', '..')."
)

prompts <- c(
  `1` = paste(prompt_intro,
              "1) the proposal has led to discussion between the political parties,",
              "2) the debate remained peaceful (between Republicans and Democrats),",
              "3) both groups listened carefully while their opponents talked to local news reporters,",
              "4) quote: we clearly disagree on this issue but I am happy that we manage to maintain a civil and unemotional tone,",
              "5) quote: my trust in my opponents grows tremendously,",
              "6) quote: I am satisfied with the constructive dialogue, and I am sure we can find the best solution for the state.",
              prompt_outro),
  `2` = paste(prompt_intro,
              "1) the proposal has led to anger between the political parties,",
              "2) the debate remained heated (between Republicans and Democrats),",
              "3) both groups refused their opponents the opportunity to talk to local news reporters,",
              "4) quote: we clearly disagree on this issue am I am appalled that they resort to such an uncivil and emotional tone,",
              "5) quote: my distrust in my opponents grows tremendously,",
              "6) quote: I am disgusted by their ridiculous arguments, and I don't think we can find the best solution for the state.",
              prompt_outro)
)

#-------
# CALLING THE API
# Sends one recollection to the model and returns its answer. Blank
# recollections are not sent.
ask_gpt <- function(prompt, text) {
  if (is.na(text) || str_trim(text) == "") return(NA_character_)

  response <- create_chat_completion(
    model = "gpt-4.1-mini",
    temperature = 0,
    messages = list(list(role = "system", content = prompt),
                    list(role = "user", content = text)),
    openai_api_key = Sys.getenv("OPENAI_API_KEY")
  )

  response$choices$message.content
}

# Retries a failed call up to five times, waiting longer before each attempt
ask_gpt_safely <- insistently(ask_gpt, rate_backoff(max_times = 5))

# Codes the recollections one by one. `prompts` holds the prompt for each
# recollection. The answers are saved to `cache` every 100 recollections and
# only recollections without an answer are sent, so rerunning the function
# after an interruption picks up where it stopped.
code_recollections <- function(prompts, texts, cache) {
  answers <- if (file.exists(cache)) readRDS(cache) else rep(NA_character_, length(texts))

  for (i in which(is.na(answers))) {
    answers[i] <- ask_gpt_safely(prompts[i], texts[i])
    if (i %% 100 == 0) saveRDS(answers, cache)
  }

  saveRDS(answers, cache)
  as.numeric(answers)
}

#-------
# CODING
W1_llm <- W1 %>%
  transmute(Record_no,
            gpt_verdict = code_recollections(prompts[Polarization], retell,
                                             cache = "data/llm_annotation/cache_share_W1.rds"))

W2_llm <- W2 %>%
  transmute(Record_no,
            gpt_verdict_w2 = code_recollections(prompts[Polarization], retell_w2_open,
                                                cache = "data/llm_annotation/cache_share_W2.rds"))

saveRDS(W1_llm, "data/llm_annotation/llm_share_W1.rds")
saveRDS(W2_llm, "data/llm_annotation/llm_share_W2.rds")

#-------
# VALIDATION AGAINST HAND CODING (Appendix G)
# 100 survey 1 recollections were coded by hand as a share (0-1)
reliability <- read_csv("data/llm_annotation/hand_coding_W1.csv") %>%   # Record_no, hand_share
  inner_join(W1_llm, by = "Record_no") %>%
  inner_join(W1 %>% select(Record_no, Polarization), by = "Record_no") %>%
  mutate(Polarization = pol_lab(Polarization),
         gpt_share = gpt_verdict / 100)

reliability_r <- reliability %>%
  group_by(Polarization) %>%
  summarise(r = cor(hand_share, gpt_share), n = n()) %>%
  mutate(label = sprintf("italic(r) == %.2f", r))

reliability_r

reliability_plot <- ggplot(reliability, aes(x = hand_share, y = gpt_share)) +
  geom_point() +
  geom_smooth(method = "lm", formula = y ~ x) +
  geom_text(data = reliability_r, aes(x = 0.05, y = 0.95, label = label), parse = TRUE, hjust = 0, size = 3.5) +
  facet_wrap(~ Polarization) +
  theme_paper() +
  labs(x = "Handcoded evaluations", y = "GPT evaluation")

save_figure(reliability_plot, "figures/LLM_reliability.pdf")

# First check of H1 with the LLM measure
W1 %>%
  inner_join(W1_llm, by = "Record_no") %>%
  fit_wls(gpt_verdict ~ Polarization, .)
