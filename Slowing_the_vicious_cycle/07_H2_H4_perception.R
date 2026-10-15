#------------------------------------------------------------------
# 07 - H2 AND H4: PERCEPTIONS OF AFFECTIVE POLARIZATION (Figure 2)
# H2: survey 1, effect of the treatment condition.
# H4: survey 2, effect of the share of treatment words in the recollection read,
#     estimated within each condition. H4 predicts a positive slope in the high
#     condition and a negative slope in the low condition.
#------------------------------------------------------------------

source("00_setup.R")

W1 <- readRDS("data/derived/W1_analysis.rds")
W2 <- readRDS("data/derived/W2_analysis.rds")

h2 <- fit_wls(affective_polarization_perception ~ Polarization, W1) %>% filter(term == "Polarization2")
h4 <- slopes_by_condition(W2, "affective_polarization_perception", "parrot_share")

h2
h4

H2_plot <- plot_condition_density(W1, "affective_polarization_perception",
                                  xlab = "Perception of affective polarization",
                                  test_p = h2$p.value)

H4_plot <- plot_dose_response(W2, "parrot_share", "affective_polarization_perception", h4,
                              xlab = "Share of treatment words",
                              ylab = "Perception of affective polarization",
                              label_x = 0.25, label_y = c(0.98, 0.91))

save_figure(gridExtra::arrangeGrob(H2_plot, H4_plot, ncol = 2), "figures/H2_H4_perception.pdf")
