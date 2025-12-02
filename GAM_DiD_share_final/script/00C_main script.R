##################### Data Input to run the functions and figures ##############
# Change this section when change to different target element
# Note: The Q in the database from WRTDS has the unit of m3s
# script/03_GAM_model_run contains the main GAM-DiD model
# Note: The data used here are partly generated data for model display, not from
# real measurements

#1. Load data
source("script/01_glm_erw_load_data_250806.R")

#2. Run this function to clean the data
source("script/02_glm_erw_data_clean_for_glm_0804.R")

#3. Define  unites
mg2t     <- 1/1e9 # Ca Mg Na
mueql2ct <- 1/1e6*44/1e6 # Alk to CO2 t
ppb2t    <- 1/1e12 # Si
cfs2ls   <- 28.3168
m3s2cfs  <- 35.31467
w2km2    <- 0.0886 # basalt area in the north

#4. Define data frame to store data

max_delta_2024 <- list()
df_flux_3m     <- list()


################################## [Alk]  #######################################

#5. Import data for the GAM-DiD model 

#A. Define units
unifact     <- 1#  unit transfer from W9 to W2 Alk
uni_w2_tran <- 1

#B. Generalize eList data 
eList_w2 <- eList_W2_Alk
eList_w9 <- eList_W9_Alk


#C. Pick the target element from W2 data for analysis
data_w2_all$c_target  <- data_w2_all$Alk
data_w2_2324$c_target <- data_w2_2324$Alk


#D. Pick the target element from W9 data for analysis 
data_w9_2019_24_avg$c_target <- data_w9_2019_24_avg$ANC.µeq.L
data_w9_2019_24_unique$c_target <- data_w9_2019_24_unique$ANC.µeq.L

# 6. GAM-DiD run:  script 03-GAM model to run GAM model

#A. Output file names
filename_csv = "data_output/gam_Alk.csv"  # save GAM results
filename_fig = "figure/gam_Alk.pdf"  # or .pdf/.tiff/.jpeg
filename_preobs <-  "figure/preobs_Alk.pdf"


#B. Run script 03-GAM model to run each element
source("script/03_GAM_model_run.R")

#C. Results from the GAM model
new_data_daily_did_Alk      <- new_data_daily_did # prediction with basalt application
new_data_daily_did_fake_Alk <- new_data_daily_did_fake  # prediction without basalt application

# unit identification
unifact_Alk <- unifact

#D. Get summary
sum_Alk <- sum_gam

#E. Post run data analysis
source("script/04_post_GAM_analysis.R")

# unit identification

uni_fig <- 1


#7. Plot treatment-effect delta C %

ylab_fig = expression(Delta *c ~ Alkalinity~"["~"%"~"]")
filename_fig = "figure/deltac_per_Alk_treatment_effect.pdf"  # or .pdf/.tiff/.jpeg

source("script/07_figure4_deltaC_percent.R")

