#------------------------------------------------------------------
# 12 - DOES H1 HOLD ACROSS THE DESIGN MANIPULATIONS? (Appendix K)
# The ~50-level state moderator is summarised by a joint F test rather than a
# coefficient table.
#------------------------------------------------------------------

source("00_setup.R")

W1 <- readRDS("data/derived/W1_analysis.rds")
W2 <- readRDS("data/derived/W2_analysis.rds")

W1_mod <- W1 %>%
  mutate(STATE = as.character(haven::as_factor(state)),
         ISSUE = Issue_conditions,
         GROUP = as.character(Group),
         QUOTE = as.character(Quote))

moderators <- c(STATE = "State of residence",
                ISSUE = "Issue condition",
                GROUP = "Group condition (voters / politicians)",
                QUOTE = "Quote condition")

interaction_models <- map(set_names(names(moderators)), \(m)
  lm(reformulate(paste("Polarization *", m), "parrot_share"), data = W1_mod, weights = weight))

# Joint test of all Polarization x moderator terms against the additive model
joint_tests <- imap_dfr(interaction_models, function(full, m) {
  additive <- lm(reformulate(c("Polarization", m), "parrot_share"), data = W1_mod, weights = weight)
  test <- anova(additive, full)
  tibble(Moderator = moderators[[m]],
         Levels = nlevels(factor(W1_mod[[m]])),
         F = sprintf("%.3f", test$F[2]),
         df = sprintf("%d, %d", test$Df[2], test$Res.Df[2]),
         p = ifelse(test$`Pr(>F)`[2] < 0.001, "<0.001", sprintf("%.3f", test$`Pr(>F)`[2])))
})

joint_tests

modelsummary(interaction_models[c("ISSUE", "GROUP", "QUOTE")], stars = TRUE,
             gof_map = c("nobs", "r.squared"),
             output = "tables/H1_interaction_tables.tex")

datasummary_df(joint_tests,
               title = "Joint tests of the H1 interaction terms against the model without them (WLS).",
               output = "tables/H1_interaction_joint_tests.tex")
