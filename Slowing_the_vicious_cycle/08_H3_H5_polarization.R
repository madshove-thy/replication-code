#------------------------------------------------------------------
# 08 - H3 AND H5: RESPONDENTS' OWN AFFECTIVE POLARIZATION
# Feeling thermometer in the main text (Figure 3); honesty / intelligence /
# morality index in Appendix J.
#------------------------------------------------------------------

source("00_setup.R")

W1 <- readRDS("data/derived/W1_analysis.rds")
W2 <- readRDS("data/derived/W2_analysis.rds")

h3 <- bind_rows(
  thermometer = fit_wls(feel_termometer ~ Polarization, W1),
  index = fit_wls(affective_polarization ~ Polarization, W1),
  .id = "outcome"
) %>%
  filter(term == "Polarization2")

h5_thermometer <- slopes_by_condition(W2, "feel_termometer", "parrot_share")
h5_index <- slopes_by_condition(W2, "affective_polarization", "parrot_share")

h3
h5_thermometer
h5_index

#-------
# FEELING THERMOMETER (Figure 3)
H3_plot <- plot_condition_density(W1, "feel_termometer", xlab = "Affective polarization",
                                  test_p = h3$p.value[h3$outcome == "thermometer"],
                                  label_x = 0.65, label_y = c(2.2, 2.0, 1.8)) +
  xlim(0, 1)

H5_plot <- plot_dose_response(W2, "parrot_share", "feel_termometer", h5_thermometer,
                              xlab = "Share of treatment words", ylab = "Affective polarization",
                              label_x = 0.25, label_y = c(0.95, 0.85)) +
  ylim(0, 1)

save_figure(gridExtra::arrangeGrob(H3_plot, H5_plot, ncol = 2), "figures/H3_H5_polarization.pdf")

#-------
# INDEX (Appendix J)
H3_plot_index <- plot_condition_density(W1, "affective_polarization", xlab = "Affective polarization, index",
                                        test_p = h3$p.value[h3$outcome == "index"], zero_line = TRUE)

H5_plot_index <- plot_dose_response(W2, "parrot_share", "affective_polarization", h5_index,
                                    xlab = "Share of treatment words", ylab = "Affective polarization",
                                    label_x = 0.25, label_y = c(0.75, 0.68))

save_figure(gridExtra::arrangeGrob(H3_plot_index, H5_plot_index, ncol = 2), "figures/H3_H5_polarization_index.pdf")
