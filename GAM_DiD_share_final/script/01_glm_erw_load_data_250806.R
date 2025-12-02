library(EGRET)
library(EGRETci)
library(ggplot2)
library(mgcv) # for gam model
library(dplyr)
library(splines)
library(effects)
library(plotly)
library(tidyr)
library(zoo)
library(dlnm)
library(lubridate)
library(effects)
library(gratia)
library(robustbase)
library(ggpmisc)

############################# Data Input from WRTDS ############################

# Load W2 WRTDS data
load("data_input/eList_W2_Alk_2.RData")

# Load W9 WRTDS data
load("data_input/eList_W9_Alk_2.RData")

############################# Load Daily Discharge ########################

# Load W2 WRTDS discharge data
q_daily_W2       <- read.csv("data_input/egret_daily_1992-2024.csv")
q_daily_W2$date <- as.Date(q_daily_W2$date, format = "%m/%d/%Y")
q_daily_W2      <- q_daily_W2 %>% rename(Date = date)

# Load W9 WRTDS discharge data
q_daily_W9      <- read.csv("data_input/egret_daily_1991-2025.csv")
q_daily_W9$date <- as.Date(q_daily_W9$date, format = "%m/%d/%Y")
q_daily_W9      <- q_daily_W9 %>% rename(Date = date)

############################# Load Water Chemistry Data ########################

# W2 water chemistry data 
data_w2_2324       <- read.csv("data_input/data_w2_2325.csv")
data_w2_2324$Dates <- as.Date(data_w2_2324$Dates, format = "%Y-%m-%d")
data_w2_all        <- read.csv("data_input/W2_data_d_combined_250725.csv")
data_w2_all$Date   <- as.Date(data_w2_all$Date , format = "%m/%d/%Y")
head(data_w2_all)

# W9 water chemistry data
data_w9_measured_Alk <- read.csv("data_input/W9_alkalinity_daily_1991-2024.csv")
data_w9_measured_Alk$Date <- as.Date(data_w9_measured_Alk$Date,"%m/%d/%Y" )
data_w9_measured_Alk <- data_w9_measured_Alk %>% rename(conc = ANC_all)


data_w9_2019_24 <- read.csv("data_input/SleepersProvisionalChem2019to2024_w9.csv") # latest Jamie data
# Step 1: Ensure Date_Time is in datetime format
data_w9_2019_24 <- data_w9_2019_24 %>%
  mutate(Date = mdy_hm(Date_Time),  # Parse full datetime
         Date = as.Date(Date))      # Keep only the date part


# Step 2: Group by Date and calculate daily averages for numeric columns only
data_w9_2019_24_avg <- data_w9_2019_24 %>%
  group_by(Date) %>%
  summarise(across(where(is.numeric), mean, na.rm = TRUE), .groups = "drop")

data_w9_2019_24_unique <- data_w9_2019_24 %>%
  mutate(Date_only = as.Date(Date)) %>%                 # Extract just the date
  group_by(Date_only) %>%
  filter(n() == 1) %>%                                  # Keep only dates with one row
  ungroup() %>%
  select(-Date_only)                                    # Optional: remove helper column

############################# Load Climate data ################################

# PRISM Data Input, with SI unit
prism_daily <- read.csv("D:/Work_Yale/R_Projects/GLM_ERW/data_input/PRISM_250726_800m_w2.csv") # changed from PRSIM to this one
prism_daily$Date <-as.Date(prism_daily$Date,"%m/%d/%Y" )

# W-9 climate data
prism_daily_w9 <- read.csv("D:/Work_Yale/R_Projects/GLM_ERW/data_input/PRISM_250726_800m_w9.csv") # changed from PRSIM to this one
prism_daily_w9$Date <-as.Date(prism_daily_w9$Date,"%m/%d/%Y" )


# NTN data input, with weekly data
NTN <- read.csv("D:/Work_Yale/R_Projects/GLM_ERW/data_input/NTN-vt99-w-s-mg_250726.csv")

# Convert dateon and dateoff in NTN to Date objects
NTN$dateOn <-as.Date(NTN$dateOn,"%m/%d/%Y" )
NTN$dateOff <-as.Date(NTN$dateOff,"%m/%d/%Y" )


replace_with_nearest <- function(x) {
  # Temporarily replace -9 with NA
  x[x == -9] <- NA
  # Use linear approximation to fill NAs
  x <- na.approx(x, na.rm = FALSE)
  # Carry forward/backward any remaining NAs at ends
  x <- na.locf(x, na.rm = FALSE)
  x <- na.locf(x, fromLast = TRUE, na.rm = FALSE)
  # If any NAs remain (empty vector), put back -9
  x[is.na(x)] <- -9
  return(x)
}
# Replace -9 with the nearest valid value for specific columns
NTN_cleaned <- NTN
NTN_cleaned[c("ph", "NO3", "Cl", "SO4")] <- lapply(NTN_cleaned[c("ph", "NO3", "Cl", "SO4")], replace_with_nearest)


