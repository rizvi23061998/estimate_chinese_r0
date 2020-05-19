# Timeseries data

# Define values
pre_peak <- 0 # -1 is 2 before peak, 2 is 2 after
omit_recent <- 0
omit_conf <- 0

# --------------- load incidence data ----------------
confirmed_cases_all <- read.csv("data/chinesedata/New_confirmed.txt",header = T,sep = "\t")
confirmed_cases_all$Region_EN <- make.unique(as.character(confirmed_cases_all$Region_EN))
rownames(confirmed_cases_all) <- confirmed_cases_all$Region_EN
confirmed_cases_all$Region_CN <- confirmed_cases_all$Region_EN <- NULL
confirmed_cases_all[confirmed_cases_all == -1] <- 0


confirmed_cases_Ezhou_time <- confirmed_cases_all[group,1:ncol(confirmed_cases_all)]
confirmed_cases_Ezhou_time[is.na(confirmed_cases_Ezhou_time)] <- 0
case_data_Ezhou <- array(0,dim = ncol(confirmed_cases_Ezhou_time))

# deprecated Wuhan is excluded
if(group_name == "Wuhan"){
  case_data_Ezhou <- array(0,dim = ncol(confirmed_cases_Ezhou_time) + 1)
}

# ------------------load recovered data ------------------------
recovered_cases_all <- read.csv("data/chinesedata/New_recovered.txt",header = T,sep = "\t")
recovered_cases_all$Region_EN <- make.unique(as.character(recovered_cases_all$Region_EN))
rownames(recovered_cases_all) <- recovered_cases_all$Region_EN
recovered_cases_all$Region_CN <- recovered_cases_all$Region_EN <- NULL
recovered_cases_all[recovered_cases_all == -1] <- 0
recovered_cases_Ezhou_time <- recovered_cases_all[group,1:ncol(recovered_cases_all)]
recovered_cases_Ezhou_time[is.na(recovered_cases_Ezhou_time)] <- 0
case_data_Ezhou_rec <- array(0,dim = ncol(recovered_cases_Ezhou_time))

#deprecated
if(group_name == "Wuhan"){
  case_data_Ezhou_rec <- array(0,dim = ncol(recovered_cases_Ezhou_time) + 1)
  
}

# ----------------- load death data -------------
death_cases_all <- read.csv("data/chinesedata/New_death.txt",header = T,sep = "\t")
death_cases_all$Region_EN <- make.unique(as.character(death_cases_all$Region_EN))
rownames(death_cases_all) <- death_cases_all$Region_EN
death_cases_all$Region_CN <- death_cases_all$Region_EN <- NULL
death_cases_all[death_cases_all == -1] <- 0
death_cases_Ezhou_time <- death_cases_all[group,1:ncol(death_cases_all)]
death_cases_Ezhou_time[is.na(death_cases_Ezhou_time)] <- 0
case_data_Ezhou_death <- array(0,dim = ncol(death_cases_Ezhou_time))

#deprecated 
# ---------------------------------
if(group_name == "Wuhan"){
  case_data_Ezhou_death <- array(0,dim = ncol(death_cases_Ezhou_time) + 1)
}
wuhan_cluster <- readRDS("outputs/rds/wuhan_cluster.rds")
wuhan_cluster <- wuhan_cluster[wuhan_cluster != "Wuhan"]

confirmed_wuhan_cluster <- apply(confirmed_cases_all[wuhan_cluster,],2,max)
confirmed_wuhan_cluster <- confirmed_wuhan_cluster[which(confirmed_wuhan_cluster !=0)[1]:length(confirmed_wuhan_cluster)]
#----------------------------------

# ---------find the first incidence and date of incidence -----------------
j=1
f=0
d=0
skip <- 0
for(jj in 1:(ncol(confirmed_cases_Ezhou_time))){
  if(skip > 0){
    skip <- skip - 1 
    next
  }
  if(f==0){  
    if(sum(confirmed_cases_Ezhou_time[,jj]) != 0){
      f=1
      case_data_Ezhou[j] <- 1
      j = j+1
      case_data_Ezhou[j] <- sum(confirmed_cases_Ezhou_time[,jj])-1  
      j=j+1
      if(group_name == "Wuhan"){
        case_data_Ezhou[j:(j+14)] <- sapply(confirmed_wuhan_cluster[2:16],as.integer)
        # j = j+15
        skip <- 5
      }
    }
    else{
      d =d+1
    }
  }
  else{
    case_data_Ezhou[j] <- sum(confirmed_cases_Ezhou_time[,jj])  
    j=j+1
  }
  
}

length(case_data_Ezhou) <- j-1

confirmed_wuhan_cluster <- apply(recovered_cases_all[wuhan_cluster,],2,max)

j=1
f=0
skip <- 0
for(jj in 1:(ncol(recovered_cases_Ezhou_time))){
  if(skip > 0){
    skip <- skip - 1 
    next
  }
  if(f==0){  
    if(sum(confirmed_cases_Ezhou_time[,jj]) != 0){
      f=1
      case_data_Ezhou_rec[j] < - 0
      j = j+1
      case_data_Ezhou_rec[j] <- sum(recovered_cases_Ezhou_time[,jj])
      j=j+1
      if(group_name == "Wuhan"){
        case_data_Ezhou_rec[j:(j+10)] <- sapply(confirmed_wuhan_cluster[2:12],as.integer)
        # j = j+10
        skip <- 5
       }
    }
  }
  else{
    case_data_Ezhou_rec[j] <-sum(recovered_cases_Ezhou_time[,jj])
    j=j+1
  }
  
}

length(case_data_Ezhou_rec) <- j-1
confirmed_wuhan_cluster <- apply(death_cases_all[wuhan_cluster,],2,max)
j=1
f=0
skip <- 0
for(jj in 1:(ncol(death_cases_Ezhou_time))){
  if(skip > 0){
    skip <- skip - 1 
    next
  }
  if(f==0){  
    if(sum(confirmed_cases_Ezhou_time[,jj]) != 0){
      f=1
      case_data_Ezhou_death[j] < - 0
      j = j+1
      case_data_Ezhou_death[j] <- sum(death_cases_Ezhou_time[,jj])
      j=j+1
      if(group_name == "Wuhan"){
        case_data_Ezhou_death[j:(j+10)] <- sapply(confirmed_wuhan_cluster[2:12],as.integer)
        # j = j+10
        skip <- 5
      }
    }
  }
  else{
    case_data_Ezhou_death[j] <- sum(death_cases_Ezhou_time[,jj])
    j=j+1
  }
  
}

length(case_data_Ezhou_death) <- j-1



# deprecated 
# # Set up start/end dates
# new_date_hubei <- max(data_hubei_Feb$date)
# 
# start_date <- as.Date("2019-11-22") # first case
# 
# 
# #end_date <- max(case_data_in$date) # omit recent day?
# end_date <- new_date_hubei #as.Date("2020-03-01") # period to forecast ahead



# ===================================date range set up==================
col_dates <- colnames(confirmed_cases_all)
strdate <- substr(col_dates[d+1],2,str_length(col_dates[d+1])) 
start_date <- as.Date(strdate,"%Y%m%d")-1
if(group_name == "Wuhan"){start_date = start_date + 16} #deprecated
end_date <- as.Date("2020-04-07")
#=========================================================================
date_range <- seq(start_date,end_date,1)



# When restrictions started
# wuhan_travel_restrictions <- as.Date("2020-01-23")

# ===========================travel time changed==============================
# wuhan_travel_restrictions <- as.Date("2010-01-23")
# 
# wuhan_travel_time <- as.numeric(wuhan_travel_restrictions - start_date + 1)
# 
# fix_r0_tt <- as.numeric(end_date-forecast_window - start_date + 1) #as.Date("2020-01-25") as.numeric(wuhan_travel_restrictions - start_date + 1) # set noise = 0 after this period of fitting


# 
# # Only use top twenty exports 
n_risk <- 20

# # shift data into days
t_period <- as.numeric(end_date-start_date)+1

# ------------ deprecated kept only for compatiblity --------------
# # Calculate exports by country
case_data_matrix <- matrix(0,nrow=t_period,ncol=n_risk)
case_data_onset_time <- rep(0,length(date_range))
case_data_scale <- rep(0,length(date_range))
case_data_china_time <- rep(0,length(date_range))

# # Create scaling vector for reporting lag
case_data_wuhan_2_scale <- rep(0,length(date_range))
 
flight_report <- rep(NA,length(date_range))
propn_flight_matrix <- matrix(NA,nrow=length(date_range),ncol=2)

# --------------------------------------------------------------------


# NEED REPORTING PARAMETER-----------------------------------
case_data_china_time[1:length(date_range)] <- NA
case_data_onset_time[1:length(date_range)] <- NA
case_data_matrix[1:nrow(case_data_matrix),1:ncol(case_data_matrix)] <- 0
# flight_report[1:length(flight_report)] <- NA
propn_flight_matrix[1:nrow(propn_flight_matrix),1:ncol(propn_flight_matrix)] <- NA


data_list <- list(local_case_data_onset = case_data_china_time, #case_data_china_time,  # case_data_wuhan_2_time
                 local_case_data_conf = case_data_Ezhou,# case_data_wuhan_conf_time,
                 int_case_onset = case_data_onset_time,
                 int_case_conf = case_data_matrix,
                 int_case_onset_scale = case_data_scale,
                 local_case_data_onset_scale = case_data_wuhan_2_scale,
                 flight_info = flight_report,
                 flight_prop = propn_flight_matrix,
                 local_case_data_death = case_data_Ezhou_death,
                 local_case_data_recovered = case_data_Ezhou_rec)

#data_list = list(local_case_data_tt=case_data_china_time[tt],case_data_tt=case_data_onset_time[tt],rep_data_tt=case_data_matrix[tt,])




