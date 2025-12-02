######################## GAM-DiD model  ########################################
# Change this section when change to different target element
# This is not a function,  run it for all the elements.

####################### Clean the data #########################################

# Use function data_clean_for_did to prepare the data for the gam models
data_input <- data_clean_for_did(eList_w2, eList_w9)

# Data input for the analysis from the data_clean_for_did function
data_did_2224           <- data_input$data_did
new_data_2224           <- data_input$new_data
data_did2               <- data_input$data_did2
new_data_daily_did      <- data_input$new_data_daily_did
new_data_daily_did_fake <- data_input$new_data_daily_did_fake
data_did_eList          <- data_input$data_did_eList


data_did_wrtds          <- data_input$data_did_wrtds

# Set time interval for the DID
data_did_2224$prepost.lag <- as.factor(data_did_2224$days_after_basalt0)
data_did_2224$prepost.3monthlag <- as.factor(ceiling(data_did_2224$days_after_basalt0/90))
levels(data_did_2224$prepost.3monthlag)[levels(data_did_2224$prepost.3monthlag) == "8"] <- "7"

new_data_2224$prepost.lag <- as.factor(new_data_2224$days_after_basalt0)
new_data_2224$prepost.3monthlag <- as.factor(ceiling(new_data_2224$days_after_basalt0/90))
levels(new_data_2224$prepost.3monthlag)[levels(new_data_2224$prepost.3monthlag) == "8"] <- "7"

# Filter out outliers 
dates_to_remove <- as.Date(c("2021-07-14","2021-07-19", "2021-08-02","2021-08-20","2021-09-09", "2021-09-16")) 
data_did_wrtds <- data_did_wrtds[ !(data_did_wrtds$Date %in% dates_to_remove), ]

### [2]. model with 2022 - 2024 w2 and w9 data with log c and q 

data_did_2224$prepost.3month <- floor(data_did_2224$days_after_basalt/30/3)
summary(as.factor(data_did_2224$prepost.3month)) 
data_did_2224$prepost.3month <- ifelse(data_did_2224$prepost.3month < -2, -2, data_did_2224$prepost.3month)
summary(as.factor(data_did_2224$prepost.3month)) #

data_did_2224$prepost.3month <- as.factor(data_did_2224$prepost.3month)


# wrtds for pre, lag periods fro pre
data_did_wrtds$prepost.3monthlag <- relevel(data_did_wrtds$prepost.3monthlag, ref="-1")

# GAM-DiD model setup:

gam_did_2224_wrtds <- gam(  logc ~ 
                             s(logq, k = 4) +   #  default k = 4; 
                             s(ppt..mm., k = 4 ) +  #  default k = 4
                             s(tmean..degrees.C., k = 4) +  #  default k = 4
                             s(Month,  by= Group ) + #  default none;  
                             Group +
                             prepost.3monthlag +
                             Group:prepost.3monthlag,
                            data = data_did_wrtds[data_did_wrtds$prepost.3monthlag %in% c('-4','-3','-2','-1','0','1','2','3','4','5','6','7'),]

 )

sum_gam <- summary(gam_did_2224_wrtds)
sum_gam
AIC(gam_did_2224_wrtds)

# Create the plot
gam_plot <- draw(gam_did_2224_wrtds)
gam_plot

# Save it
ggsave(filename_fig, gam_plot, width = 10, height = 6, dpi = 300)


# Save parametric coefficients
param_table <- as.data.frame(summary(gam_did_2224_wrtds)$p.table)
write.csv(param_table, sub(".csv", "_parametric.csv", filename_csv), row.names = TRUE)

# Save smooth terms
smooth_table <- as.data.frame(summary(gam_did_2224_wrtds)$s.table)
write.csv(smooth_table, sub(".csv", "_smooth.csv", filename_csv), row.names = TRUE)



################ Use the GAM to predict the concentrations #####################

# Choose the right gam for prediction

gam_did_predict <- gam_did_2224_wrtds

pred_gam      <- predict(gam_did_predict, newdata = data_did_2224, se.fit = TRUE)
pred_gam_fake <- predict(gam_did_predict,newdata = new_data_2224, se.fit = TRUE, type = "response")

data_did_2224$predicted_c_target_gam     <-  exp( pred_gam$fit )
data_did_2224$predicted_c_target_fake_gam <- exp( pred_gam_fake$fit)

data_did_2224$predicted_c_target_gam_upper      <- exp( pred_gam$fit + 1.96*pred_gam$se )
data_did_2224$predicted_c_target_fake_gam_upper <- exp( pred_gam_fake$fit + 1.96*pred_gam$se )

data_did_2224$predicted_c_target_gam_lower      <- exp( pred_gam$fit - 1.96*pred_gam$se )
data_did_2224$predicted_c_target_fake_gam_lower <- exp( pred_gam_fake$fit - 1.96*pred_gam$se )

# Fit linear model or obs and est w2 concentrations
lm_model <- lm(predicted_c_target_gam ~ c_target, data = data_did_2224)

# Extract equation and R²
eq <- sprintf("y = %.2fx + %.2f", 
              coef(lm_model)[2], coef(lm_model)[1])
r2 <- sprintf("R² = %.3f", summary(lm_model)$r.squared)

print(r2)

### Quality control checks: 


concurvity(gam_did_predict, full = TRUE)

summary(gam_did_predict)$r.sq
summary(gam_did_predict)$dev.expl

cor(data_did_2224[, c("logq", "ppt..mm.", "tmean..degrees.C.")], use = "complete.obs")

table(data_did_2224$Group, data_did_2224$prepost.3monthlag)


############# Daily estimations from GAM model##################################
# Use the GAM to predict the concentrations

pred_gam_daily      <- predict(gam_did_predict, newdata = new_data_daily_did,      se.fit = TRUE, type = "response") # daily prediction for did model
pred_gam_daily_fake <- predict(gam_did_predict, newdata = new_data_daily_did_fake, se.fit = TRUE, type = "response") # daily controfactual data
 
new_data_daily_did$predicted_c_target                 <- exp(pred_gam_daily$fit)
new_data_daily_did_fake$predicted_c_target_fake       <- exp(pred_gam_daily_fake$fit)

new_data_daily_did$predicted_c_target_upper           <- exp(pred_gam_daily$fit      + 1.96*pred_gam_daily$se )
new_data_daily_did_fake$predicted_c_target_fake_upper <- exp(pred_gam_daily_fake$fit + 1.96*pred_gam_daily_fake$se )

new_data_daily_did$predicted_c_target_lower           <- exp(pred_gam_daily$fit      + 1.96*pred_gam_daily$se )
new_data_daily_did_fake$predicted_c_target_fake_lower <- exp(pred_gam_daily_fake$fit + 1.96*pred_gam_daily_fake$se )

new_data_daily_did$predicted_c_target_se              <- exp(pred_gam_daily$se)
new_data_daily_did_fake$predicted_c_target_fake_se    <- exp(pred_gam_daily_fake$se)

# Calculate the delta_c 
  


############## check the obs and pre concentrations from GAM ###########
# plot 1: predict vs. obs with time
ggplot() + 
  geom_line (data = new_data_daily_did %>% filter(Group == 1), aes(x = Date, y = predicted_c_target), color = "black", size = 1) +
  geom_point (data = data_w2_all, aes(x = Date, y = c_target * uni_w2_tran, color = "blue"), size = 1) 


# plot 2: predict vs. obs linear fit rob

data_fit_check <- new_data_daily_did  %>% filter(Group == 1) %>%
  left_join(data_w2_all, by = "Date")


mod_robust <- lmrob(predicted_c_target ~ I(c_target * uni_w2_tran), data = data_fit_check)
r2 <-summary(mod_robust)$r.squared
r2

plot_preob <- ggplot(data_fit_check, aes(x = c_target * uni_w2_tran, y = predicted_c_target)) +
  geom_point(color = "darkgrey", size = 1) +
  geom_smooth(method = "lmrob", se = FALSE, color = "black", linetype = "dashed", linewidth = 0.5) +
  annotate("text", x = Inf, y = Inf, label = paste0("R² = ", round(r2, 2)),
           hjust = 1.1, vjust = 1.5, size = 6) +
   labs(x = "Observed [µmol/L]", y = "Predicted [µmol/L]") +
  theme_classic(base_size = 12) +
  theme(
    panel.grid.major.x = element_blank(),
    axis.text  = element_text(size = 13),
    axis.title = element_text(size = 15)
  )


plot_preob


ggsave(
  
  filename = filename_preobs,  # or .pdf/.tiff/.jpeg
  plot_preob,
  width = 5, # inch
  height = 4, 
  dpi = 600,  # Ultra-high resolution
  bg = "white"  # Transparent background if omitted
)

