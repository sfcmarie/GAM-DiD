data_clean_for_did <- function (eList_w2, eList_w9) {
  
  
  ########################### Data Preparation ###################################
  
  
    # Assume Basalt application date
  basalt_date <- as.Date("2023-07-01")
  
  # Transfer the W9 concentration unit to W2 concentration unit
  eList_w9$Daily$ConcDay_ppm <- eList_w9$Daily$ConcDay*unifact;
  
  # Assign W9 daily concentration to W9_eList_conc_daily
  W9_eList_conc_daily <- eList_w9$Daily[, c("Date", "ConcDay_ppm")] # Note: ppm not for alk
  
  
  
  # Perform a cross join and then filter (join with the W2 2022-2024 data)
  NTN_join <- data_w2_all %>%
    mutate(key = 1) %>%
    left_join(NTN_cleaned %>% mutate(key = 1), by = "key") %>%
    filter(Date > dateOn & Date <= dateOff) %>%
    select(Date, SO4 = SO4, ph = ph, NO3 = NO3) %>%
    distinct()
  
  
  # Perform a cross join and then filter (join with the W2 1992-2023 data)
  NTN_join2 <- eList_w2$Sample %>%
    mutate(key = 1) %>%
    left_join(NTN_cleaned %>% mutate(key = 1), by = "key") %>%
    filter(Date > dateOn & Date <= dateOff) %>%
    select(Date, SO4 = SO4, ph = ph, NO3 = NO3) %>%
    distinct()
  
  # Perform a cross join and then filter (join with the W2 1992-2024 data)
  NTN_9224 <- bind_rows(NTN_join, NTN_join2) %>%
    distinct(Date, .keep_all = TRUE) %>%
    arrange(Date)
  
  
  # Perform a cross join and then filter (join with the W9 data)
  NTN_join_w9 <- data_w9_2019_24_unique %>%
    mutate(key = 1) %>%
    left_join(NTN_cleaned %>% mutate(key = 1), by = "key") %>%
    filter(Date > dateOn & Date <= dateOff) %>%
    select(Date, SO4 = SO4, ph = ph, NO3 = NO3) %>%
    distinct()
  
  # Create a column named c_target to make the colume name identical
  eList_w2$Sample$c_target <- eList_w2$Sample$ConcAve
  
  # Combine the target concentrations from 1992 to 2024, Keep only Date and c_target from both datasets
  combined_simplified <- bind_rows(
    eList_w2$Sample %>% select(Date, c_target),
    data_w2_all %>% select(Date, c_target)
  ) %>%
    distinct()  # Remove duplicate rows (same Date & c_target)
  
  
  data_combined <- data_w2_all %>%
    left_join(NTN_9224,      by = "Date") %>%
    left_join(prism_daily, by = "Date") %>%
    left_join(W9_eList_conc_daily, by = "Date") %>%  # Limited data after Aug 2024
    left_join(q_daily_W9,  by = "Date")
  
  data_combined <- data_combined %>% 
    filter(Date %in% data_w2_all$Date[!is.na(data_w2_all$Discharge)])  
  
  # With no NTN data due to lack of data 
  data_combined2 <- data_w2_all %>%
    left_join(prism_daily, by = "Date") %>%
    left_join(W9_eList_conc_daily, by = "Date") %>%
    left_join(q_daily_W9,  by = "Date")
  
  data_combined2 <- data_combined2 %>% 
    filter(Date %in% data_w2_all$Date[!is.na(data_w2_all$Discharge)])  
  
  
  data_combined$Basalt       <- ifelse(data_combined$Date >= as.Date("2023-07-01"), 1, 0)
  data_combined2$Basalt      <- ifelse(data_combined2$Date >= as.Date("2023-07-01"), 1, 0)

  # Add a column to identify the group (1 = treatment, 0 = control)
  data_combined$Group      <- ifelse(is.na(data_combined$ConcDay), 1, 0)
  data_combined2$Group     <- ifelse(is.na(data_combined2$ConcDay), 1, 0)

  
  # Create separate datasets for treatment and control
  data_combined$conc_W9       <- data_combined$ConcDay_ppm
  data_combined2$conc_W9      <- data_combined2$ConcDay_ppm
  
  
  treatment_data <- data_combined %>% 
    select(Date, c_target, Discharge, SO4, ph, NO3, ppt..mm., tmean..degrees.C., Basalt) %>%
    mutate(Group = 1)  # Treatment group
  
  control_data <- data_combined %>% 
    select(Date, c_target = conc_W9, Discharge = Qdaily, SO4, ph, NO3, ppt..mm., tmean..degrees.C., Basalt) %>%
    mutate(Group = 0)  # Control group
  

  
  treatment_data2 <- data_combined2 %>% 
    select(Date, c_target, Discharge, ppt..mm., tmean..degrees.C., Basalt) %>%
    mutate(Group = 1)  # Treatment group
  
  control_data2 <- data_combined2 %>% 
    select(Date, c_target = conc_W9, Discharge = Qdaily, ppt..mm., tmean..degrees.C., Basalt) %>%
    mutate(Group = 0)  # Control group
  
  
  
  # Update the climate data of w9
  
  control_data <- control_data %>%
    select(-`ppt..mm.`, -`tmean..degrees.C.`) %>%  # drop old values
    left_join(
      prism_daily_w9 %>% select(Date, `ppt..mm.`, `tmean..degrees.C.`),
      by = "Date"
    )
  
  control_data2 <- control_data2 %>%
    select(-`ppt..mm.`, -`tmean..degrees.C.`) %>%  # drop old values
    left_join(
      prism_daily_w9 %>% select(Date, `ppt..mm.`, `tmean..degrees.C.`),
      by = "Date"
    )


  
  # Use the real W-9 measurement for the did model control
    
  control_data_obs <- data_w9_2019_24_unique %>%   #use this for average data for strom days data_w9_2019_24_avg
    select(`Date`,`c_target`,`W9_Flow_cfs`) %>%  # drop old values
    left_join(
      prism_daily_w9 %>% select(Date, `ppt..mm.`, `tmean..degrees.C.`),
      by = "Date"
    )%>%
    left_join(NTN_join_w9,      by = "Date") 
  
  control_data_obs$Basalt       <- ifelse(control_data_obs$Date >= as.Date("2023-07-01"), 1, 0)
  control_data_obs$Group        <- ifelse(is.na(control_data_obs$c_target), 1, 0)
  names(control_data_obs)[names(control_data_obs) == "W9_Flow_cfs"] <- "Discharge"

  control_data_obs <- control_data_obs[control_data_obs$Date >= as.Date("2021-05-01"), ]
  
  ##############################################################################
  ############### WRTDS data for W2 and W9 pre treatment data.##################
  ##############################################################################

    # 1. Separate W2 w9 data pre and post 
  
  data_w2_post_obs          <- data_w2_all [data_w2_all$Date >= as.Date("2023-07-01"), ]
  data_w2_post_obs$c_target <- data_w2_post_obs$c_target*uni_w2_tran
  
  
  data_w9_post_wrtds <- control_data[control_data$Date >= as.Date("2023-07-01"), ]
  data_w9_post_wrtds <- data_w9_post_wrtds %>%
    filter(Date %in% data_w2_post_obs$Date)
  
  data_w2_pre_wrtds  <- eList_w2$Daily[eList_w2$Daily$Date < as.Date("2023-07-01")&eList_w2$Daily$Date >= as.Date("2021-05-01"), ]
  data_w2_pre_wrtds$Discharge <- data_w2_pre_wrtds$Q *m3s2cfs
  data_w2_pre_wrtds$c_target  <- data_w2_pre_wrtds$ConcDay * uni_w2_tran
  
  data_w2_pre_obs          <- data_w2_all [data_w2_all$Date < as.Date("2023-07-01"), ]
  data_w2_pre_obs$c_target <- data_w2_pre_obs$c_target*uni_w2_tran
  
  
  data_w9_pre_wrtds  <- eList_w9$Daily[eList_w9$Daily$Date < as.Date("2023-07-01")&eList_w9$Daily$Date >= as.Date("2021-05-01"), ]
  data_w9_pre_wrtds$Discharge  <- data_w9_pre_wrtds$Q *m3s2cfs
  data_w9_pre_wrtds$c_target   <- data_w9_pre_wrtds$ConcDay * unifact
  
  data_w9_pre_obs <- control_data_obs[control_data_obs$Date >= as.Date("2021-05-01")&control_data_obs$Date < as.Date("2023-07-01"), ]
  data_w9_pre_obs$c_target <- data_w9_pre_obs$c_target * unifact
  
  # log c  and q
  
  data_w9_pre_wrtds$logc <- log(data_w9_pre_wrtds$c_target)
  data_w9_pre_wrtds$logq <- log(data_w9_pre_wrtds$Discharge)
  
  data_w9_post_wrtds$logc <- log(data_w9_post_wrtds$c_target)
  data_w9_post_wrtds$logq <- log(data_w9_post_wrtds$Discharge)
  
  
  data_w2_pre_wrtds$logc <- log(data_w2_pre_wrtds$c_target)
  data_w2_pre_wrtds$logq <- log(data_w2_pre_wrtds$Discharge)
  
  data_w2_post_obs$logc <- log(data_w2_post_obs$c_target)
  data_w2_post_obs$logq <- log(data_w2_post_obs$Discharge)
  
  data_w9_pre_obs$logc <- log(data_w9_pre_obs$c_target)
  data_w9_pre_obs$logq <- log(data_w9_pre_obs$Discharge)
  
  data_w2_pre_obs$logc <- log(data_w2_pre_obs$c_target)
  data_w2_pre_obs$logq <- log(data_w2_pre_obs$Discharge)
  
  # Trim data based on log c
  
  q1_low  <- quantile(data_w9_pre_wrtds$logc, 0.05, na.rm = TRUE)
  q1_high <- quantile(data_w9_pre_wrtds$logc, 0.95, na.rm = TRUE)
  data_w9_pre_wrtds_trimmed <- subset(data_w9_pre_wrtds, logc >= q1_low & logc <= q1_high)
  
  q2_low  <- quantile(data_w9_post_wrtds$logc, 0.05, na.rm = TRUE)
  q2_high <- quantile(data_w9_post_wrtds$logc, 0.95, na.rm = TRUE)
  data_w9_post_wrtds_trimmed <- subset(data_w9_post_wrtds, logc >= q2_low & logc <= q2_high)
  
  q3_low  <- quantile(data_w2_pre_wrtds$logc, 0.05, na.rm = TRUE)
  q3_high <- quantile(data_w2_pre_wrtds$logc, 0.95, na.rm = TRUE)
  data_w2_pre_wrtds_trimmed <- subset(data_w2_pre_wrtds, logc >= q3_low & logc <= q3_high)
  
  q4_low  <- quantile(data_w2_post_obs$logc, 0.05, na.rm = TRUE)
  q4_high <- quantile(data_w2_post_obs$logc, 0.999, na.rm = TRUE)
  data_w2_post_obs_trimmed <- subset(data_w2_post_obs, logc >= q4_low & logc <= q4_high)
  
  # Trim data based on log q
  
  q5_low  <- quantile(data_w9_pre_wrtds$logq, 0.05, na.rm = TRUE)
  q5_high <- quantile(data_w9_pre_wrtds$logq, 0.95, na.rm = TRUE)
  data_w9_pre_wrtds_trimmed <- subset(data_w9_pre_wrtds, logq >= q5_low & logq <= q5_high)
  
  q6_low  <- quantile(data_w9_post_wrtds$logq, 0.05, na.rm = TRUE)
  q6_high <- quantile(data_w9_post_wrtds$logq, 0.95, na.rm = TRUE)
  data_w9_post_wrtds_trimmed <- subset(data_w9_post_wrtds, logq >= q6_low & logq <= q6_high)
  
  q7_low  <- quantile(data_w2_pre_wrtds$logq, 0.05, na.rm = TRUE)
  q7_high <- quantile(data_w2_pre_wrtds$logq, 0.95, na.rm = TRUE)
  data_w2_pre_wrtds_trimmed <- subset(data_w2_pre_wrtds, logq >= q7_low & logq <= q7_high)
  
  
  
# Combine observation and wrtds from w2 and w9 for pre data
  
  data_w2_pre_obs2wrtds <- data_w2_pre_wrtds %>%
    filter(Date %in% data_w9_pre_obs$Date)  
  

  # First, select only relevant columns from both datasets
  obs_main <- data_w2_pre_obs %>%
    select(Date, c_target, logc, logq, Discharge)
  
  wrtds_fill <- data_w2_pre_obs2wrtds %>%
    select(Date, c_target, logc, logq, Discharge)
  
  # Combine, giving priority to obs_main if duplicated
  data_w2_pre_combined <- bind_rows(obs_main, wrtds_fill) %>%
    arrange(Date) %>%
    distinct(Date, .keep_all = TRUE)
  
  
  
  data_w9_pre_obs2wrtds <- data_w9_pre_wrtds %>%
    filter(Date %in% data_w2_pre_obs$Date) 
  
  
  # First, select only relevant columns from both datasets
  obs_main9 <- data_w9_pre_obs %>%
    select(Date, c_target, logc, logq, Discharge)
  
  wrtds_fill9 <- data_w9_pre_obs2wrtds %>%
    select(Date, c_target, logc, logq, Discharge)
  
  # Combine, giving priority to obs_main if duplicated
  data_w9_pre_combined <- bind_rows(obs_main9, wrtds_fill9) %>%
    arrange(Date) %>%
    distinct(Date, .keep_all = TRUE)
  
  # Trim the combined data
  
  q_low  <- quantile(data_w2_pre_combined$logc, 0.01, na.rm = TRUE)
  q_high <- quantile(data_w2_pre_combined$logc, 0.99, na.rm = TRUE)
  data_w2_pre_combined_trimmed <- subset(data_w2_pre_combined, logc >= q_low & logc <= q_high)
  
  q_low  <- quantile(data_w9_pre_combined$logc, 0.1, na.rm = TRUE)
  q_high <- quantile(data_w9_pre_combined$logc, 0.95, na.rm = TRUE)
  data_w9_pre_combined_trimmed <- subset(data_w9_pre_combined, logc >= q_low & logc <= q_high)
  
  
  
  
  
  # Combine pre post 
  
  # Perform a cross join and then filter (join with the W2 1992-2023 data)
  # Here, change the treatment_data_wrtds and control_data_wrtds
   
  # This one works for the wrtds data 
  treatment_data_wrtds <- rbind(
    data_w2_pre_wrtds_trimmed[, c("Date", "c_target", "Discharge","logc","logq")],
    data_w2_post_obs_trimmed[, c("Date", "c_target", "Discharge","logc","logq")]
  )
  
  #  This one works for the combined data with obs and wrtds for pre
  
 # treatment_data_wrtds <- rbind(
 #   data_w2_pre_combined_trimmed[, c("Date", "c_target", "Discharge","logc","logq")],
  #  data_w2_post_obs [, c("Date", "c_target", "Discharge","logc","logq")]
#  )
  

  treatment_data_wrtds<-treatment_data_wrtds %>%
    left_join(
      prism_daily %>% select(Date, `ppt..mm.`, `tmean..degrees.C.`),
      by = "Date"
    )
  
  treatment_data_wrtds<-treatment_data_wrtds %>%
    mutate(key = 1) %>%
    left_join(NTN_cleaned %>% mutate(key = 1), by = "key") %>%
    filter(Date > dateOn & Date <= dateOff) %>%
    select(Date, SO4 = SO4, ph = ph, NO3 = NO3,c_target = c_target, Discharge = Discharge, ppt..mm.=ppt..mm., tmean..degrees.C. = tmean..degrees.C., logc = logc, logq = logq ) %>%
    distinct()  
  
  treatment_data_wrtds$Group <- as.factor(1)
  
  #  This one works for the wrtds data 

   control_data_wrtds <- rbind(
   data_w9_pre_wrtds_trimmed  [, c("Date", "c_target", "Discharge","logc","logq")],
   data_w9_post_wrtds_trimmed [, c("Date", "c_target", "Discharge","logc","logq")]
  )
  
  #  This one works for the combined data with obs and wrtds for pre
  
#  control_data_wrtds <- rbind(
 #   data_w9_pre_combined_trimmed  [, c("Date", "c_target", "Discharge","logc","logq")],
 #   data_w9_post_wrtds_trimmed [, c("Date", "c_target", "Discharge","logc","logq")]
 # )
  
  
  control_data_wrtds<-control_data_wrtds %>%
    left_join(
      prism_daily_w9 %>% select(Date, `ppt..mm.`, `tmean..degrees.C.`),
      by = "Date"
    )
  
  control_data_wrtds<-control_data_wrtds %>%
    mutate(key = 1) %>%
    left_join(NTN_cleaned %>% mutate(key = 1), by = "key") %>%
    filter(Date > dateOn & Date <= dateOff) %>%
    select(Date, SO4 = SO4, ph = ph, NO3 = NO3,c_target = c_target, Discharge = Discharge, ppt..mm.=ppt..mm., tmean..degrees.C. = tmean..degrees.C., logc = logc, logq = logq ) %>%
    distinct() 
  
  control_data_wrtds$Group <- as.factor(0)
  
  data_did_wrtds <- rbind(control_data_wrtds, treatment_data_wrtds)
  data_did_wrtds$prepost     <- ifelse(data_did_wrtds$Date >= as.Date("2023-07-01"), 1, 0)
  
  # Create a new variable for days after basalt application
  data_did_wrtds <- data_did_wrtds %>%
    mutate(days_after_basalt = as.numeric(difftime(Date, basalt_date, units = "days"))) 
  
  
  # 3 month data lag
  data_did_wrtds$prepost.3monthlag <- as.factor(floor(data_did_wrtds$days_after_basalt/90))
  

  
  # Include seasonality based on WRTDS
  
  data_did_wrtds$t_season <- as.numeric(format(data_did_wrtds$Date, "%j")) / 365  # t in [0, 1]
  
  data_did_wrtds$sin2pi <- sin(2 * pi * data_did_wrtds$t_season)
  data_did_wrtds$cos2pi <- cos(2 * pi * data_did_wrtds$t_season)
  
  data_did_wrtds$Month <- as.numeric(format(data_did_wrtds$Date, "%m"))
  data_did_wrtds$Year <- as.numeric(format(data_did_wrtds$Date, "%Y"))
  data_did_wrtds$Date_num <- as.numeric(data_did_wrtds$Date)
  data_did_wrtds$Week_num <- as.integer(difftime(data_did_wrtds$Date, min(data_did_wrtds$Date), units = "weeks")) + 1

  
  
  
  # Check data
  
  library(ggplot2)
  

  
  control <- subset(data_did_wrtds, Group == 0)
  treatment <- subset(data_did_wrtds, Group == 1)
  
  
  ggplot(data_did_wrtds, aes(x = Date, y = logc, color = as.factor(Group))) +
    geom_point(alpha = 0.6) +
    labs(color = "Group", y = "log(c)", title = "logc Over Time by Group") +
    scale_color_manual(values = c("blue", "red"), labels = c("Control", "Treatment")) +
    scale_x_date(date_labels = "%b%n%y", date_breaks = "3 month") +  # %b = Jan, Feb, ...
    
    theme_minimal()
  
  
  
  # Merge by Date (without reshaping the original data)
  common <- merge(control[, c("Date", "Discharge",'c_target','ppt..mm.','tmean..degrees.C.','logq')],
                  treatment[, c("Date", "Discharge",'c_target','ppt..mm.','tmean..degrees.C.','logq')],
                  by = "Date",
                  suffixes = c("_control", "_treatment"))

    common_weekly <- common %>%
    mutate(week = floor_date(Date, unit = "week")) %>%
    group_by(week) %>%
    summarise(across(where(is.numeric), ~mean(.x, na.rm = TRUE)), .groups = "drop")
  

  ggplot(common_weekly, aes(x = logq_control, y = logq_treatment)) +
    geom_point(alpha = 0.6) +
    geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "gray") +
    labs(x = "Control Discharge", y = "Treatment Discharge",
         title = "Treatment vs. Control Discharge") +
    theme_minimal()
  
  
  ################# Create weekly data for pre wrtds   ######################### 
  
  
  data_did_wrtds_weekly <- data_did_wrtds %>%
    mutate(
      week = floor_date(Date, unit = "week"),
      Month = as.numeric(format(Date, "%m")),
      sin2pi = sin(2 * pi * yday(Date) / 365),
      cos2pi = cos(2 * pi * yday(Date) / 365),
      Date_num = as.numeric(Date)
    ) %>%
    group_by(Group, week, prepost.3monthlag) %>%
    summarise(
      c_target = mean (c_target, na.rm = TRUE),
      Discharge = mean (Discharge, na.rm = TRUE),
      logc = mean(logc, na.rm = TRUE),
      logq = mean(logq, na.rm = TRUE),
      ppt..mm. = mean(ppt..mm., na.rm = TRUE),
      tmean..degrees.C. = mean(tmean..degrees.C., na.rm = TRUE),
      SO4 = mean(SO4, na.rm = TRUE),
      NO3 = mean(NO3, na.rm = TRUE),
      ph = mean(ph, na.rm = TRUE),
      Month = first(Month),
      Year = first(Year),
      Date = first(week),
      Date_num = mean(Date_num, na.rm = TRUE),
      sin2pi = mean(sin2pi, na.rm = TRUE),
      cos2pi = mean(cos2pi, na.rm = TRUE),
      .groups = "drop"
    )
  
  data_did_wrtds_weekly$week_num <- as.numeric(data_did_wrtds_weekly$week)
  
  # Conservative trim for trend checking
  q_low <- quantile(data_did_wrtds_weekly$logc, 0.05, na.rm = TRUE)
  q_high <- quantile(data_did_wrtds_weekly$logc, 0.95, na.rm = TRUE)

  data_did_wrtds_weekly_trimmed <- subset(data_did_wrtds_weekly, logc >= q_low & logc <= q_high)
  
  pre_cutoff <- as.Date("2023-07-01")
  
  # Trim separately within each group, only for pre-treatment data
  data_did_wrtds_weekly_trimmed <- data_did_wrtds_weekly %>%
    group_by(Group) %>%
    mutate(
      q_low = quantile(logc[Date < pre_cutoff], 0.05, na.rm = TRUE),
      q_high = quantile(logc[Date < pre_cutoff], 0.95, na.rm = TRUE)
    ) %>%
    ungroup() %>%
    filter(Date >= pre_cutoff | (logc >= q_low & logc <= q_high)) %>%
    select(-q_low, -q_high)
  
  ggplot(data_did_wrtds_weekly_trimmed, aes(x = week, y = logc, color = as.factor(Group))) +
    geom_point(alpha = 0.6) +
    labs(color = "Group", y = "log(c)", title = "logc Over Time by Group") +
    scale_color_manual(values = c("blue", "red"), labels = c("Control", "Treatment")) +
    theme_minimal()
  
  
  
  #############################################################################
  # Check the parallel assumption
  
  pre_data <- subset(data_did_wrtds, Date < as.Date("2022-11-01"))
  
  # Weekly data
  
  library(dplyr)
  library(lubridate)
  
  pre_data_weekly <- pre_data %>%
    mutate(week = floor_date(Date, unit = "week")) %>%
    group_by(Group, week) %>%
    summarise(across(where(is.numeric), ~mean(.x, na.rm = TRUE)), .groups = "drop")
  
  pre_data_weekly$week_num <- as.numeric(pre_data_weekly$week)
  
  
  # Conservative trim for trend checking
  q_low <- quantile(pre_data_weekly$logq, 0.05, na.rm = TRUE)
  q_high <- quantile(pre_data_weekly$logq, 0.95, na.rm = TRUE)
  pre_data_trimmed <- subset(pre_data_weekly, logq >= q_low & logq <= q_high)
  
  
  
  library(mgcv)
  
  gam_parallel_test <- gam(
    logc ~ s(week_num, by = Group, k = 5) + 
      Group +
      s(logq) +
      s(ppt..mm., k =5) + 
      s(tmean..degrees.C., k = 5)+
      sin2pi +
      cos2pi,
    data = pre_data_weekly
  )
  
  library(gratia)
  draw(gam_parallel_test, select = 1:2)  # Assuming the first two terms are the group-specific smooths
  
  summary(gam_parallel_test)
  
  draw(gam_parallel_test) +
    labs(title = "Group-specific smooths for week_num") +
    theme_minimal()
  
  ##############################################################################
  
  
  # Assume Basalt application date
  basalt_date <- as.Date("2023-07-01")
  
  ########################## Create a did frame for the model #####################
  # Create did data frame
  data_did      <- rbind(treatment_data, control_data)
  data_did2     <- rbind(treatment_data2, control_data2)

  # recreate did data with w9-observed data
  # data_did      <- rbind(treatment_data, control_data_obs)
  
  ##New Group: reverse the treatment and control group in order to get the estimate and SE easily in DID analysis
  data_did$NewGroup <- as.factor(2 - as.numeric(data_did$Group))
  
  # Create a new variable for days after basalt application
  data_did <- data_did %>%
    mutate(days_after_basalt = as.numeric(difftime(Date, basalt_date, units = "days"))) 
  
  data_did2 <- data_did2 %>%
    mutate(days_after_basalt = as.numeric(difftime(Date, basalt_date, units = "days"))) 
  

  # Assume days after basalt is 0
  data_did$days_after_basalt0      <- ifelse(data_did$Date < basalt_date, 0, as.numeric(data_did$Date - basalt_date))
  data_did2$days_after_basalt0     <- ifelse(data_did2$Date < basalt_date, 0, as.numeric(data_did2$Date - basalt_date))

  # Remove rows with NA values
  data_did        <- na.omit(data_did)  
  data_did$Group  <- as.factor(data_did$Group)
  data_did$Prepost<- as.factor(data_did$Basalt)
  data_did$Month  <- as.numeric(format(data_did$Date, "%m"))

  data_did2        <- na.omit(data_did)  
  data_did2$Group  <- as.factor(data_did$Group)
  data_did2$Prepost<- as.factor(data_did$Basalt)
  data_did2$Month  <- as.numeric(format(data_did$Date, "%m"))
  

  table(data_did2$Group)  # Counts of treatment (1) and control (0) groups
  
  
  
  ################### Generate predicted values from the model##################
  
  
  # New data from data_did (data points related to real measurements )
  # Discharge is W2 q, Qdaily is W9 q, 
  new_data        <- data_did[, c("Date", "Group","Discharge", "tmean..degrees.C.", "c_target","days_after_basalt0","days_after_basalt",  "Month", "ppt..mm.")]
  new_data$Basalt <- 0;
  new_data$Prepost<- as.factor(new_data$Basalt)
  
  new_data$days_after_basalt0 <- 0;
  
  
  ##############################################################################
  # New data from daily estimated measurements WRTDS data
  # 1. Left join all the database
  data_w2_eList_daily <- eList_w2$Daily[, c("Date","Month","Q")]
  
  #data_daily <- data_w2_all %>%
  #  left_join(NTN_join,      by = "Date") %>%
   # left_join(prism_daily, by = "Date") %>%
  #  left_join(W9_eList_conc_daily, by = "Date") %>%  # Limited data after Aug 2024
   # left_join(q_daily_W9,  by = "Date") %>%
  #  left_join(data_w2_eList_daily, by = "Date")
  
  # Daily parameter for the prediction of the gam model
  data_daily <- prism_daily %>%
    left_join(W9_eList_conc_daily, by = "Date") %>%  # Limited data after Aug 2024
    left_join(q_daily_W9,          by = "Date") %>%
    left_join(data_w2_eList_daily, by = "Date") %>%
    filter(Date >= as.Date("2022-07-02") & Date < as.Date("2025-05-01"))
    
  
  data_daily$Prepost <- ifelse(data_daily$Date >= as.Date("2023-07-01"), 1, 0)
  
  
  new_data_daily_con <- data.frame(
    Discharge         = data_daily$Q*35.315, # m3s to csf
    Date              = data_daily$Date,
    Month             = data_daily$Month,
    ppt..mm.          = data_daily$ppt..mm.,
    tmean..degrees.C. = data_daily$tdmean..degrees.C.,
  #  ph                = data_daily$ph,
  #  SO4               = data_daily$SO4,
  #  NO3               = data_daily$NO3,
  #  c_target          = data_daily$ConcDay,
    Prepost           = as.factor(data_daily$Prepost),
    Group             = as.factor(0)
  )
  
  new_data_daily_tre <- new_data_daily_con
  new_data_daily_tre$Group <- as.factor(1)
  
  
  new_data_daily_did  <- rbind(new_data_daily_con, new_data_daily_tre)
  
  new_data_daily_did <- new_data_daily_did %>%
    mutate(days_after_basalt = as.numeric(difftime(Date, basalt_date, units = "days"))) 
  
  new_data_daily_did$days_after_basalt0 <- ifelse(new_data_daily_did$Date < basalt_date, 0, as.numeric(new_data_daily_did$Date - basalt_date))
  
  # Group the daily data into 3-month time window
  new_data_daily_did$prepost.3monthlag <-  as.factor(floor(new_data_daily_did$days_after_basalt/91))

  # The 2025-04 data is combined to 1-3 month data
  # new_data_daily_did$prepost.3monthlag[new_data_daily_did$prepost.3monthlag == 8] <- 7
  
  
  # Remove rows with NA values
  new_data_daily_did        <- na.omit(new_data_daily_did)  
  new_data_daily_did$Group  <- as.factor(new_data_daily_did$Group)
  new_data_daily_did$Prepost<- as.factor(new_data_daily_did$Prepost)
  new_data_daily_did_fake   <- new_data_daily_did
  
  new_data_daily_did_fake$Prepost            <- as.factor(0)
  new_data_daily_did_fake$days_after_basalt0 <- 0
  
  # Group the daily data into 3-month time window
  new_data_daily_did_fake$prepost.3monthlag <- as.factor(floor(new_data_daily_did_fake$days_after_basalt/91))

  # The 2025-04 data is combined to 1-3 month data
  # levels(new_data_daily_did_fake$prepost.3monthlag)[levels(new_data_daily_did_fake$prepost.3monthlag) == "8"] <- "7"
  
  table(new_data_daily_did$Group)  # Counts of treatment (1) and control (0) groups
  
  
  data_did$logq      <- log(data_did$Discharge)
  data_did$logc      <- log(data_did$c_target)
  data_did$date_num  <- as.numeric(data_did$Date)
  data_did$dayofyear <- yday(data_did$Date)
  
  new_data$logq      <- log(new_data$Discharge)
  new_data$date_num  <- as.numeric(new_data$Date)
  new_data$dayofyear <- yday(new_data$Date)
  
  
  
  ##############################################################################
  # New data from daily estimated measurements
  # 1. Left join all the database
  data_w2_eList_daily <- eList_w2$Daily[, c("Date","Month","Q")]
  

  data_did$logq      <- log(data_did$Discharge)
  data_did$logc      <- log(data_did$c_target)
  data_did$date_num  <- as.numeric(data_did$Date)
  data_did$dayofyear <- yday(data_did$Date)
  
  new_data$logq      <- log(new_data$Discharge)
  new_data$date_num  <- as.numeric(new_data$Date)
  new_data$dayofyear <- yday(new_data$Date)
  
  
  new_data_daily_did$logq      <- log(new_data_daily_did$Discharge)
  new_data_daily_did$date_num  <- as.numeric(new_data_daily_did$Date)
  new_data_daily_did$dayofyear <- yday(new_data_daily_did$Date)
  
  
  new_data_daily_did_fake$logq      <- log(new_data_daily_did_fake$Discharge)
  new_data_daily_did_fake$date_num  <- as.numeric(new_data_daily_did_fake$Date)
  new_data_daily_did_fake$dayofyear <- yday(new_data_daily_did_fake$Date)
  

  
  return(list( data_did  = data_did,  new_data = new_data, 
               data_did2 = data_did2, new_data_daily_did = new_data_daily_did, 
               new_data_daily_did_fake = new_data_daily_did_fake,
               data_did_wrtds = data_did_wrtds
  ))
  
  
  
}

