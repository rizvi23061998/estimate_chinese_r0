# COVID-19 stochastic model
# Driver
# Author: Irtesam Mahmud Khan (2020)

# Set up libraries and paths ----------------------------------------------

library(foreach)
library(doMC)
library(lubridate)
library(magrittr)
library(coda)
library(tidyverse)
library(rootSolve)
library(mgcv)

registerDoMC(4)  #change the 4 to your number of CPU cores

rm(list=ls(all=TRUE))



# - - -
# Set user-specific directory path and load datasets
if(Sys.info()["user"]=="khan" | Sys.info()["user"]=="Khan") {
  setwd("~/workspace/2020-ncov/stoch_model_V2_paper/")
  dropbox_path <- "./" 
  print("in")
  Sys.setenv(TZ = "Asia/Dhaka")
}else{
  # setwd("..")
  setwd("/media/khan/Codes1/Ebooks&Lectures/Level-4_Term-2/Thesis/Corona/2020-ncov")
  dropbox_path <- "./" 
}

source('R/province_clustering.R')

group_names <- c("group1_all","group2_all","group3_all",
                 "group1_incidence","group2_incidence","group3_incidence",
                 "group1_without_mf","group2_without_mf","group3_without_mf",
                 "group1_all_age","group2_all_age","group3_all_age",
                 "group1_temp","group2_temp","group3_temp",
                 "group1_only_age","group2_only_age","group3_only_age")


#-------------run for each groups------------

for(group_name in group_names){
  print(group_name)
  source("scripts/main_model.R")
}