##################### Data Input to run the functions and figures ##############
# Note: The Q in the database from WRTDS has the unit of m3s-1
# script/03_GAM_model_run contains the main GAM-DiD model
# Note: The data used here are partly generated data for model display, not from
# real measurements

#0. Library


library(ggplot2)
library(mgcv)    # Gam-DiD model
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


#1. Load W2 streamwater alkalinity data

data_w2_all        <- read.csv("data_input/W2_data_d_combined_250725.csv")
data_w2_all$Date   <- as.Date(data_w2_all$Date , format = "%m/%d/%Y")

#2. Load dataframe for the GAM-DiD analysis
load("data_input/data_did_wrtds.RData")


################################## [Alk]  #######################################

#3. Import data for the GAM-DiD model 

# Pick the target element from W2 data for analysis
data_w2_all$c_target  <- data_w2_all$Alk


# 4. GAM-DiD run: 

#A. Output file names
filename_csv = "data_output/gam_Alk.csv"  # save GAM results
filename_fig = "figure/gam_Alk.pdf"  # or .pdf/.tiff/.jpeg
filename_preobs <-  "figure/preobs_Alk.pdf"


#B. Run script 03-GAM model to run each element
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

# Create the plot
gam_plot <- draw(gam_did_2224_wrtds)


#5. Plot treatment-effect delta C %

ylab_fig = expression(Delta *c ~ Alkalinity~"["~"%"~"]")
filename_fig = "figure/deltac_per_Alk_treatment_effect.pdf"  # or .pdf/.tiff/.jpeg

#  plot for the GAM treatment effect
source("script/07_figure4_deltaC_percent.R")





