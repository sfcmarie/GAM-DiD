# This is the script to analyze GAM results


# Combine data_did and eList W2 data for later analysis
data_did <- data_did_2224
data_obs_eList <- data_w2_all %>%
  left_join(eList_w2$Daily, by= "Date")


data_treatment =  subset(data_did, Group == 1)
data_treatment$predicted_c_target_fake <- data_treatment$predicted_c_target_fake_gam
data_treatment$predicted_c_target <- data_treatment$predicted_c_target_gam


data_treatment$delta_c_obs   <- data_treatment$c_target - data_treatment$predicted_c_target_fake
data_treatment$delta_c_est   <- data_treatment$predicted_c_target - data_treatment$predicted_c_target_fake
data_obs_eList$delta_c_elist <-  data_obs_eList$c_target - data_obs_eList$ConcDay

data_treatment$delta_c_obspp   <- (data_treatment$c_target - data_treatment$predicted_c_target_fake)/data_treatment$predicted_c_target_fake
data_treatment$delta_c_estpp   <- (data_treatment$predicted_c_target - data_treatment$predicted_c_target_fake)/data_treatment$predicted_c_target_fake
data_obs_eList$delta_c_elistpp <- (data_obs_eList$c_target - data_obs_eList$ConcDay)/data_obs_eList$ConcDay

