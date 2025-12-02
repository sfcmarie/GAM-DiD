
############ Concentration contribution as % from basalt  treatment ############

# Load packages
library(ggplot2)
library(dplyr)



# Plot the percentage difference calculate from delta C, works for old codes
# ggplot() + 
#    geom_point(data = data_treatment, 
#              aes(x = Date, y = delta_c_est/predicted_c_target_gam, color = "Estimated"), size = 1) +
#   
#   scale_color_manual(values = c("Observed" = "blue", "Estimated" = "red", "eList" = "orange")) +
#   labs(title = "Increase of Concentration after Basalt Application", 
#        x = "Date", 
#        y = expression(Delta * "c (µeq/L)"),
#        color = "Legend") +
#   scale_x_date(
#     date_breaks = "3 month",
#     date_labels = "%m/%Y"
#   ) +
#   theme_minimal() +
#   theme(
#     axis.text.x = element_text(angle = 45, hjust = 1)
#   )
# 
# 

################################################################################
###################### Error bar plot from  GAM  ###############################

n_break = 1:11  # number of lags excluding lag -1 which is a reference 


# 1. Get coefficients from GAM model

coef_data <- data.frame(
  lag      = n_break,  # x-axis positions
  term     = paste0("Group1:prepost.3monthlag", n_break),
  estimate = sum_gam$p.coeff[grep("Group1:prepost.3monthlag", names(sum_gam$p.coeff))],
  se       = sum_gam$se[grep("Group1:prepost.3monthlag", names(sum_gam$se))],
  p_value  = sum_gam$p.pv[grep("Group1:prepost.3monthlag", names(sum_gam$p.pv))]
) %>%
  mutate(
    # Create significance stars
    significance = case_when(
      p_value < 0.001 ~ "***",
      p_value < 0.01 ~ "**",
      p_value < 0.05 ~ "*",
      TRUE ~ ""
    )
  )



# 2.  Note: the se is for log(c), need transfer

# it gives the increase as a fraction of the treated outcome, not as a fraction of the untreated counterfactual
# it’s the share of the treated outcome attributable to treatment rather than the percent change relative to baseline

coef_data$c_per   <- 1/exp(coef_data$estimate) * (exp(coef_data$estimate) - 1) * 100  # (exp(beta) - 1) /(exp(beta)) 
coef_data$low_c   <- 1/exp(coef_data$estimate - 1.96 * coef_data$se) * (exp(coef_data$estimate - 1.96 * coef_data$se) -1) * 100  
coef_data$up_c    <- 1/exp(coef_data$estimate + 1.96 * coef_data$se) * (exp(coef_data$estimate + 1.96 * coef_data$se) -1) * 100

# Use another equation
# This gives the relative increase over the counterfactual (no treatment) level.
# coef_data$c_per2   <- (exp(coef_data$estimate) - 1) * 100  # (exp(beta) - 1) /(exp(beta)) relative increase over the counterfactual (no treatment) level.
# coef_data$low_c2   <- 1/exp(coef_data$estimate - 1.96 * coef_data$se) * (exp(coef_data$estimate - 1.96 * coef_data$se) -1) * 100  
# coef_data$up_c2    <- 1/exp(coef_data$estimate + 1.96 * coef_data$se) * (exp(coef_data$estimate + 1.96 * coef_data$se) -1) * 100



ggplot() + 
  geom_line (data = coef_data,
             aes(x = lag, y = c_per), color = "black", linewidth = 0.5) +
  geom_line (data = coef_data,
             aes(x = lag, y = low_c), color = "black", linewidth = 0.5) +
  geom_line (data = coef_data,
             aes(x = lag, y = up_c), color = "black", linewidth = 0.5) 


# 3. Create quarterly date labels starting from July 2023
date_labels <- format(seq.Date(from = as.Date("2022-07-01"),
                               by = "3 months",
                               length.out = 11),
                      "%b\n%Y")  # Format as "Jul-2023", "Oct-2023", etc.

# 4. Add lag -1 which is the reference data to the plot data

## 1>.  Shift the x‑position of lags 4–11 so there is an empty slot at 4
plot_data <- coef_data %>%
  mutate(lag_plot = ifelse(lag >= 4, lag + 1, lag))

## 2>.  Add a single “gap” row at x = 4, y = 0
gap_row <- tibble(
  lag       = NA,           # real lag not needed
  lag_plot  = 4,            # where the point will sit
  c_per     = 0,
  low_c     = NA_real_,     # NA so no error bar
  up_c      = NA_real_,
  significance = ""         # no star
)

plot_data <- bind_rows(plot_data, gap_row) %>% arrange(lag_plot)

## 3.  Build breaks & labels ------------------------------------------------
n_break <- plot_data$lag_plot

# original quarterly labels (Jul‑2023, Oct‑2023, …)

## make sure the string really is "2023-07-01", not “2023‑07‑01”
start_date  <- as.Date("2022-07-01")          # safest: explicit format
# start_date <- as.Date("2023‑07‑01", format = "%Y-%m-%d")  # also OK

date_seq    <- seq.Date(from = start_date, by = "3 months", length.out = 12)
date_labels <- format(date_seq, "%b\n%y")     # "Jul\n2023", "Oct\n2023", ...



## 5.  plot the  figure ----------------------------------------------------
fig_delta_per <- ggplot(
  
  plot_data, aes(x = lag_plot, y = c_per)) +
  geom_hline(yintercept = 0, linetype = "dashed", colour = "grey50") +
  geom_errorbar(aes(ymin = low_c, ymax = up_c),
                width = 0.15, colour = "black") +
  geom_point(shape = 22, size = 3, fill = "white", colour = "black") +
  geom_text(aes(y = up_c + 0.05 * max(abs(up_c), na.rm = TRUE),
                label = significance),
            size = 5, vjust = 0) +
  scale_x_continuous(breaks = n_break, labels = date_labels) +
  ylim(-10, 23) +  # default (-10, 23)  ylim(-14, 25) for Na
  labs(x = "Every 3 months interval",
       y = ylab_fig) +

  theme_classic(base_size = 18) +
  theme(panel.grid.major.x = element_blank(),
        axis.text  = element_text(size = 15),
        axis.title = element_text(size = 18),
        plot.margin = margin(5, 15, 5, 5)
  )




## end of new plot

fig_delta_per
# Export with custom size and resolution

ggsave(
  filename = filename_fig,  # or .pdf/.tiff/.jpeg
  plot = fig_delta_per,
  width = 6,      # Width in inches (adjust as needed)
  height = 5,     # Height in inches
  dpi = 600,      # Resolution (300-600 for publications)
  units = "in",   # Units for width/height ("in", "cm", "mm")
  device = "pdf" ,  # Format ("png", "pdf", "tiff", "jpeg")
  bg = "white"  # Transparent background if omitted
  
)




