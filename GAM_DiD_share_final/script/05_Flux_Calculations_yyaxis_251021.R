
########################## Flux from Basalt Weathering #########################
# This script calculate the daily, monthly, and 3-month average flux from basalt weathering.
# The treatment effect % c from gam is from 07-figure4-deltaC-percent
# Computes daily total flux from predicted_c_target x Discharge, then applies
# the GAM-DiD treatment effect expressed as a percent change per 3-month lag
# bin (coef_data, from 07-figure4-deltaC-percent) to isolate the basalt flux,
# with CIs. Aggregated to monthly, 3-month, and cumulative flux.

# Load packages

library(dplyr)
library(lubridate)

# Load data from GAM-DiD model
load("data_input/gam_did_inputs.RData")
# Define some unites
mg2t     <- 1/1e9 # Ca Mg Na
mueql2ct <- 1/1e6*44/1e6 # Alk to CO2 t
ppb2t    <- 1/1e12 # Si
cfs2ls   <- 28.3168
m3s2cfs  <- 35.31467
w2km2    <- 0.0886 # basalt area in the north

unit_tdkm2                <- mg2t * cfs2ls *86400/ w2km2  # t/d/km2
uni_w2_tran               <- 1000/44  # unit of t C

################################################################################
  # Calculate flux export based on estimated daily data from 2023/07/01 to 2025/04/31 

  # 1. Daily total flux from GAM
  new_data_daily_did$preicted_flux_all   <- new_data_daily_did$predicted_c_target * new_data_daily_did$Discharge / uni_w2_tran * unit_tdkm2 * 365  # [t yr km2]  # unit_tdkm2<- mg2t * cfs2ls *86400/ w2km2  # t/d/km2

  # 2. Add %C from GAM to new_data_daily_did
  lag_numeric <- as.numeric(coef_data$lag)
  lag_numeric[1:3] <- lag_numeric[1:3] - 1  # subtract 5 instead of 4
  
  # Then assign all at once and convert to factor
  coef_data$prepost.3monthlag <- as.factor(lag_numeric - 4)
  
  
  new_data_daily_did <- new_data_daily_did %>%
    left_join(coef_data %>% select(prepost.3monthlag, c_per,low_c, up_c,se),
              by = "prepost.3monthlag") %>%
    mutate(per_c_gam = c_per) %>%
    select(-c_per)  # Optional: clean up joined column
  
  
  # 3. Daily delta flux
  
  new_data_daily_did$delta_flux_basalt     <- new_data_daily_did$preicted_flux_all * new_data_daily_did$per_c_gam/100
  
  # 4. Calculate 3 month average 


  flux_3month_avg <- new_data_daily_did %>%
    filter(Group == 1) %>%
    mutate(period = floor_date(Date, unit = "3 months")) %>%
    group_by(period) %>%
    summarise(
      delta_flux_3mo = mean(delta_flux_basalt, na.rm = TRUE),
           per_c_gam  = mean(per_c_gam, na.rm = TRUE),
      
    ) %>%
    ungroup()
  
 
  
  flux_month_avg <- new_data_daily_did %>%
    filter(Group == 1) %>%
    mutate(month = floor_date(Date, unit = "month")) %>%
    group_by(month) %>%
    summarise(delta_flux_month = mean(delta_flux_basalt, na.rm = TRUE)) %>%
    ungroup()
  


  # 5. calculate cumulative flux
  
  flux_daily <- new_data_daily_did %>%
    filter (Group ==1) %>%
    filter (Date > as.Date("2023-07-01"))
  
  flux_daily$cum_flux     <- cumsum(flux_daily$delta_flux_basalt)*w2km2/365
  
 
  #############################################################################
  # Plot. Daily/Monthly flux +  cumulative flux
  
  # Define scaling factors to align the secondary y-axis with the primary

  ylim_flux  <- c(-30, 150/12*44) # daily export rate
  ylim_flux2 <- c(-0.36, 2/12*44*0.9) # cum export rate
  
  ylab_flux  <- expression(Alkalinity ~ export ~rate ~  "[" ~ t ~CO2  ~ km^{-2} ~ yr^{-1}~"]")
  ylab_flux2 <- expression(Alkalinity ~ cumulative ~flux  ~  "[" ~ t ~CO2~ "]")
  
  
  primary_range <- range(ylim_flux)
  secondary_range <- range(ylim_flux2)
  
  # Calculate scaling factor and shift
  scale_factor <- diff(primary_range) / diff(secondary_range)
  shift <- primary_range[1] - secondary_range[1] * scale_factor
  
  # Transform the cumulative flux data to match primary axis scale
  flux_daily_transformed <- flux_daily %>%
    mutate(
      cum_flux_scaled = cum_flux * scale_factor + shift

    )
  
  # Create combined plot with secondary y-axis
  fig_combined <- ggplot() +
    # Gray bars for monthly average flux (primary y-axis)
    geom_col(data = flux_month_avg,
             aes(x = month + days(15), y = delta_flux_month),
             fill = "lightblue", width = 28, alpha = 0.7) +
    
    # Line plot for daily flux (primary y-axis)
    geom_line(data = new_data_daily_did %>% filter(Group == 1),
              aes(x = Date, y = delta_flux_basalt),
              color = "black", linewidth = 0.3) +
    
    # Line for cumulative flux (secondary y-axis)
    geom_line(data = flux_daily_transformed,
              aes(x = Date, y = cum_flux_scaled),
              color = "darkgrey", linewidth = 0.6) +
    
    # X-axis
    scale_x_date(
      limits = as.Date(c("2023-06-01", "2025-05-01")),
      date_breaks = "3 months",
      date_labels = "%b\n%y"
    ) +
    

    # Dual y-axes
    scale_y_continuous(
      name = ylab_flux,  # Primary y-axis label
      sec.axis = sec_axis(
        ~ (. - shift) / scale_factor,  # Inverse transformation
        name = ylab_flux2  # Secondary y-axis label
      )
    ) +
    
    labs(x = "Month (post-treatment)", title = "") +
    
    # Themes
    theme_classic(base_size = 18) +
    theme(
      axis.text.x = element_text(angle = 0, hjust = 1),                         
      axis.title = element_text(size = 18),
      axis.title.y.right = element_text(color = "darkgrey"),  # Color for secondary axis title
      axis.text = element_text(size = 15),
      axis.text.y.right = element_text(color = "darkgrey"),   # Color for secondary axis text
      axis.line.y.right = element_line(color = "darkgrey"), 
      axis.ticks.y.right = element_line(color = "darkgrey"),
      legend.justification = c(-0.1, 1),
      legend.text = element_text(size = 15),
      legend.box = "horizontal",
      legend.background = element_rect(fill = NA, color = "white", linewidth = 0.3),
      legend.spacing.x = unit(0.3, 'cm'),
      plot.margin = margin(5, 15, 5, 5)
    )
  
  # Display the plot
  fig_combined
  

  