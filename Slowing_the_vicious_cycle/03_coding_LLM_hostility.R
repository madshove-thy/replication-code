#------------------------------------------------------------------
# 03 - TREATMENT-BLIND LLM CODING OF HOSTILITY (0-10)
# Rates how hostile each recollection portrays the relationship between
# Democrats and Republicans. Unlike 02, every text gets the same prompt, with
# no information about treatment assignment. The stimulus articles are rated
# as well, to anchor the scale.
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
prompt_recollection <- paste(
  "Below is one person's recollection of a news article about a state legislative debate.",
  "On a scale from 0 to 10, how hostile or conflictual does this text portray the relationship",
  "between Democrats and Republicans? 0 = entirely cooperative and respectful;",
  "5 = neutral or not addressed; 10 = entirely hostile and antagonistic. Answer with a number only."
)

# Same prompt for the articles, with the first sentence adapted
prompt_article <- str_replace(prompt_recollection,
                              "Below is one person's recollection of a news article",
                              "Below is a news article")

#-------
# CALLING THE API
# Sends one text to the model and returns its answer. Blank texts are not sent.
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

# Codes the texts one by one with the same prompt. The answers are saved to
# `cache` every 100 texts and only texts without an answer are sent, so
# rerunning the function after an interruption picks up where it stopped.
code_texts <- function(prompt, texts, cache) {
  answers <- if (file.exists(cache)) readRDS(cache) else rep(NA_character_, length(texts))

  for (i in which(is.na(answers))) {
    answers[i] <- ask_gpt_safely(prompt, texts[i])
    if (i %% 100 == 0) saveRDS(answers, cache)
  }

  saveRDS(answers, cache)
  as.numeric(answers)
}

# Quick check before the full run (expect 9-10)
ask_gpt(prompt_recollection, "The Democrats and Republicans were furious, shouted at each other and refused to listen.")

#-------
# THE STIMULUS ARTICLES
# Rebuilt from the treatment template. Group, quote order and state do not
# touch the hostility wording, so they are held fixed (voters, Democrat
# first, Alabama) and the 2 x 4 polarization-by-issue versions are rated.
issue_noun <- c("recycling", "gun control", "taxation", "food irradiation")
issue_body <- str_remove(stimulus$policy, " (control|middle class)$")

# The wording that differs between the low (1) and high (2) polarization article
wording <- list(
  c(relation = "discussion", debate = "peaceful",
    behaviour = "listened carefully while their opponents talked to",
    quote_1 = "but I am happy that we manage to maintain a civil and unemotional tone",
    trust = "trust",
    quote_2 = "satisfied with the constructive dialogue, and I am sure"),
  c(relation = "anger", debate = "heated",
    behaviour = "refused their opponents the opportunity to talk to",
    quote_1 = "and I am appalled that they resort to such an uncivil and emotional tone",
    trust = "distrust",
    quote_2 = "disgusted by their ridiculous arguments, and I don’t think")
)

build_article <- function(polarization, issue, state = "Alabama") {
  w <- wording[[polarization]]
  topic <- issue_noun[issue]

  as.character(str_glue(
    "New {topic} bill up for debate in {state}.\n\n",
    "This week, the {state} State Legislature arranged a debate about a new policy proposal on {topic}. ",
    "The proposal has led to extensive local media coverage and {w['relation']} between the political parties.\n\n",
    "Before the bill was formally discussed, Democratic and Republican voters were gathered outside ",
    "the state capitol building to express their opinions on the bill. ",
    "The debate remained {w['debate']} as both groups {w['behaviour']} local news reporters.\n\n",
    "{issue_body[issue]}\n\n",
    "After the debate, one Democratic voter said to a local news station: ",
    "“We clearly disagree on this issue {w['quote_1']}. ",
    "On days like these, my {w['trust']} in my opponents grows tremendously.” ",
    "Another Republican voter said: “I am {w['quote_2']} we can find the best solution for the state of {state}.”\n\n",
    "This is the third debate about a bill on {topic} since 2023. ",
    "The following months will bring additional debates and hearings before the bill can come to a vote."
  ))
}

articles <- expand_grid(Polarization = 1:2, Issue = 1:4) %>%
  mutate(topic = issue_noun[Issue],
         article = map2_chr(Polarization, Issue, build_article),
         hostility_article = map_dbl(article, \(a) as.numeric(ask_gpt_safely(prompt_article, a))))

# The two conditions should separate; the four topics should not
articles %>% select(Polarization, topic, hostility_article)

saveRDS(articles %>% select(-article), "data/llm_annotation/hostility_articles.rds")

#-------
# THE RECOLLECTIONS
W1_hostility <- W1 %>%
  transmute(Record_no,
            hostility_w1 = code_texts(prompt_recollection, retell,
                                      cache = "data/llm_annotation/cache_hostility_W1.rds"))

W2_hostility <- W2 %>%
  transmute(Record_no,
            hostility_w2 = code_texts(prompt_recollection, retell_w2_open,
                                      cache = "data/llm_annotation/cache_hostility_W2.rds"))

saveRDS(W1_hostility, "data/llm_annotation/hostility_W1.rds")
saveRDS(W2_hostility, "data/llm_annotation/hostility_W2.rds")

# First check: hostility by treatment condition
W1 %>% inner_join(W1_hostility, by = "Record_no") %>% fit_wls(hostility_w1 ~ Polarization, .)
W2 %>% inner_join(W2_hostility, by = "Record_no") %>% fit_wls(hostility_w2 ~ Polarization, .)
