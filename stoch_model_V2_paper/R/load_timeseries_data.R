# Timeseries data

# Define values
pre_peak <- 0 # -1 is 2 before peak, 2 is 2 after
omit_recent <- 0
omit_conf <- 0


confirmed_cases_all <- read.csv("data/chinesedata/New_confirmed.txt",header = T,sep = "\t")
confirmed_cases_all$Region_EN <- make.unique(as.character(confirmed_cases_all$Region_EN))
rownames(confirmed_cases_all) <- confirmed_cases_all$Region_EN
confirmed_cases_all$Region_CN <- confirmed_cases_all$Region_EN <- NULL
confirmed_cases_all[confirmed_cases_all == -1] <- 0


confirmed_cases_Ezhou_time <- confirmed_cases_all[group,1:ncol(confirmed_cases_all)]
confirmed_cases_Ezhou_time[is.na(confirmed_cases_Ezhou_time)] <- 0
case_data_Ezhou <- array(0,dim = ncol(confirmed_cases_Ezhou_time))
if(group_name == "Wuhan"){
  case_data_Ezhou <- array(0,dim = ncol(confirmed_cases_Ezhou_time) + 1)
}


recovered_cases_all <- read.csv("data/chinesedata/New_recovered.txt",header = T,sep = "\t")
recovered_cases_all$Region_EN <- make.unique(as.character(recovered_cases_all$Region_EN))
rownames(recovered_cases_all) <- recovered_cases_all$Region_EN
recovered_cases_all$Region_CN <- recovered_cases_all$Region_EN <- NULL
recovered_cases_all[recovered_cases_all == -1] <- 0
recovered_cases_Ezhou_time <- recovered_cases_all[group,1:ncol(recovered_cases_all)]
recovered_cases_Ezhou_time[is.na(recovered_cases_Ezhou_time)] <- 0
case_data_Ezhou_rec <- array(0,dim = ncol(recovered_cases_Ezhou_time))
if(group_name == "Wuhan"){
  case_data_Ezhou_rec <- array(0,dim = ncol(recovered_cases_Ezhou_time) + 1)
  
}


death_cases_all <- read.csv("data/chinesedata/New_death.txt",header = T,sep = "\t")
death_cases_all$Region_EN <- make.unique(as.character(death_cases_all$Region_EN))
rownames(death_cases_all) <- death_cases_all$Region_EN
death_cases_all$Region_CN <- death_cases_all$Region_EN <- NULL
death_cases_all[death_cases_all == -1] <- 0
death_cases_Ezhou_time <- death_cases_all[group,1:ncol(death_cases_all)]
death_cases_Ezhou_time[is.na(death_cases_Ezhou_time)] <- 0
case_data_Ezhou_death <- array(0,dim = ncol(death_cases_Ezhou_time))
if(group_name == "Wuhan"){
  case_data_Ezhou_death <- array(0,dim = ncol(death_cases_Ezhou_time) + 1)
}
wuhan_cluster <- readRDS("outputs/rds/wuhan_cluster.rds")
wuhan_cluster <- wuhan_cluster[wuhan_cluster != "Wuhan"]

confirmed_wuhan_cluster <- apply(confirmed_cases_all[wuhan_cluster,],2,max)
confirmed_wuhan_cluster <- confirmed_wuhan_cluster[which(confirmed_wuhan_cluster !=0)[1]:length(confirmed_wuhan_cluster)]

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




# Set up start/end dates
new_date_hubei <- max(data_hubei_Feb$date)

start_date <- as.Date("2019-11-22") # first case


#end_date <- max(case_data_in$date) # omit recent day?
end_date <- new_date_hubei #as.Date("2020-03-01") # period to forecast ahead



# ===================================date range changed==================
col_dates <- colnames(confirmed_cases_all)
strdate <- substr(col_dates[d+1],2,str_length(col_dates[d+1])) 
start_date <- as.Date(strdate,"%Y%m%d")-1
if(group_name == "Wuhan"){start_date = start_date + 16}
end_date <- as.Date("2020-04-07")
#=========================================================================
date_range <- seq(start_date,end_date,1)



# When restrictions started
# wuhan_travel_restrictions <- as.Date("2020-01-23")

# ===========================travel time changed==============================
wuhan_travel_restrictions <- as.Date("2010-01-23")

wuhan_travel_time <- as.numeric(wuhan_travel_restrictions - start_date + 1)

fix_r0_tt <- as.numeric(end_date-forecast_window - start_date + 1) #as.Date("2020-01-25") as.numeric(wuhan_travel_restrictions - start_date + 1) # set noise = 0 after this period of fitting


# 
# # Only use top twenty exports 
n_risk <- 20
# travel_data <- travel_data[1:20,]
# 
# # Load international confirmation data --------------------------------------------
# case_data <- case_data_in
# cutoff_case_int <- max(case_data$date) - omit_conf # omit final days of time points
# 
# case_data$export_probability <- as.numeric(travel_data[match(case_data$country,travel_data$label),]$risk) # Add risk
# case_data <- case_data[!is.na(case_data$export_probability),] # Only use available data
# 
# # tally cases
# case_time <- rep(0,length(date_range))
# 
# for(ii in 1:length(date_range)){
#   case_time[ii] = sum(case_data[case_data$date==date_range[ii],]$number)
# }
# 
# case_time[date_range>cutoff_case_int] <- NA # NOTE not used
# 
# # shift data into weeks
t_period <- as.numeric(end_date-start_date)+1
# case_data <- case_data %>% mutate(time = as.numeric(date - start_date + 1))
# 
# # compile matrix of cases in top 30 risk locations
# top_risk <- travel_data[1:n_risk,]
# 
# # Calculate exports by country
case_data_matrix <- matrix(0,nrow=t_period,ncol=n_risk)
# match_list_cases <- match(case_data$country,top_risk$label)
# for(ii in 1:nrow(case_data)){
#   case_data_matrix[case_data[ii,]$time,match_list_cases[ii]] <- case_data[ii,]$number # add detected cases
# }
# 
# # Load international onset data --------------------------------------------
# case_data_onset_report_date <- as.Date("2020-01-28")
# case_data_onset <- international_onset_data_in
# cutoff_time_int_onsets <- max(case_data_onset$date) #- omit_recent # omit final days of time points
# 
# case_data_onset[case_data_onset$date>cutoff_time_int_onsets,"number"] <- NA
case_data_onset_time <- rep(0,length(date_range))
# 
# for(ii in 1:length(date_range)){
#     case_data_onset_time[ii] <- sum(case_data_onset[case_data_onset$date==date_range[ii],]$number)
# }
# 
# case_data_onset_time[date_range>cutoff_time_int_onsets] <- NA # omit final points
# 
case_data_scale <- rep(0,length(date_range))
# case_data_scale <-1-exp(-pmax(0,case_data_onset_report_date - date_range + 1)*theta[["report"]])
# 
# # Load China onset data --------------------------------------------
# 
# case_data_china <- china_onset_data_in
# cutoff_time_china <- max(case_data_china$date) - omit_recent # omit final days of time points
# case_data_china[case_data_china$date>cutoff_time_china,"number"] <- NA
case_data_china_time <- rep(0,length(date_range))
# 
# # ensure final points are omitted
# for(ii in 1:length(date_range)){
#   case_data_china_time[ii] <- sum(case_data_china[case_data_china$date==date_range[ii],]$number)
# }
# case_data_china_time[date_range>cutoff_time_china] <- NA # omit final points
# 
# # Load Wuhan early data --------------------------------------------
# 
# case_data_wuhan <- wuhan_onset_data_in
# case_data_wuhan$number <- case_data_wuhan$number - case_data_wuhan$number_market # calculate non-market exposures
# 
# case_data_wuhan_time <- rep(0,length(date_range))
# 
# for(ii in 1:length(date_range)){
#   case_data_wuhan_time[ii] = sum(case_data_wuhan[case_data_wuhan$date==date_range[ii],]$number)
# }
# 
# # Quick plot
# #plot(case_data_wuhan$date,case_data_wuhan$number,xlim=as.Date(c("2019-12-01","2020-01-25")),ylim=c(0,30)); points(case_data_china$date,case_data_china$number,col="blue")
# 
# # INITIAL APPROXIMATION -  ADD TOGETHER CHINA TIMESERIES
# 
# case_data_china_time <- case_data_china_time + case_data_wuhan_time
# 
# # Load Wuhan 2020-01-30 onset data --------------------------------------------
# 
# case_data_wuhan_2 <- wuhan_onset_2020_01_30
# case_data_wuhan_2$number <- case_data_wuhan_2$number - case_data_wuhan_2$linked_to_market # remove market exposures
# final_time_wuhan_2 <- max(case_data_wuhan_2$date) # find latest data point
# 
# case_data_wuhan_2_time <- rep(0,length(date_range))
# 
# # ensure final points are omitted
# for(ii in 1:length(date_range)){
#   case_data_wuhan_2_time[ii] <- sum(case_data_wuhan_2[case_data_wuhan_2$date==date_range[ii],]$number)
# }
# 
# # EDIT TO FIT DIFFERENT TIMESERIES
# case_data_wuhan_2_time[(which(case_data_wuhan_2_time==max(case_data_wuhan_2_time))+pre_peak):length(date_range)] <- NA # only look at up to peak: 
# #case_data_wuhan_2_time[date_range>final_time_wuhan_2] <- NA # put NA at end of timeseries
# 
# 
# # Create scaling vector for reporting lag
case_data_wuhan_2_scale <- rep(0,length(date_range))
# case_data_wuhan_2_scale <-1-exp(-pmax(0,final_time_wuhan_2 - date_range + pre_peak)*theta[["report"]])
# 
# 
# # Load Wuhan confirmed data --------------------------------------------
# 
# # case_data_wuhan_conf <- wuhan_conf_data_in
# # case_data_wuhan_conf_time <- rep(0,length(date_range))
# # cutoff_time_wuhan <- max(case_data_wuhan_conf$date)
# # 
# # for(ii in 1:length(date_range)){
# #   case_data_wuhan_conf_time[ii] = sum(case_data_wuhan_conf[case_data_wuhan_conf$date==date_range[ii],]$number)
# # }
# # 
# # case_data_wuhan_conf_time[date_range>cutoff_time_wuhan] <- NA # omit all but single point
# 
# # Create flight prevalence series --------------------------------------------
# 
# 
# 
# date_flights_out_1_japan <- as.Date("2020-01-29")
# date_flights_out_2_japan <- as.Date("2020-01-30")
# date_flights_out_3_japan <- as.Date("2020-01-31") # SAME AS KOREA
# date_flights_out_2_germany <- as.Date("2020-02-01")
# 
# date_flights_out_1_singapore <- as.Date("2020-02-06")
# date_flights_out_1_malaysia <- as.Date("2020-02-04") # SAME AS BELGIUM
# date_flights_out_1_italy <- as.Date("2020-02-03")
# #date_flights_out_1_korea <- as.Date("2020-01-31")
# 
flight_report <- rep(NA,length(date_range))
propn_flight_matrix <- matrix(NA,nrow=length(date_range),ncol=2)
# flight_report[date_range == date_flights_out_1_japan |
#               date_range == date_flights_out_2_japan | 
#               date_range == date_flights_out_3_japan |
#               date_range == date_flights_out_2_germany |
#               date_range == date_flights_out_1_singapore | 
#               date_range == date_flights_out_1_malaysia |
#               date_range == date_flights_out_1_italy  ] <- 1
# 
# prop_flight_1_japan <- c(4,206)
# prop_flight_2_japan <- c(2,210)
# prop_flight_3_japan <- c(2,149)
# prop_flight_2_germany <- c(2,120)
# 
# prop_flight_1_singapore <- c(4,91)
# prop_flight_1_malaysia <- c(2,107)
# prop_flight_1_italy <- c(1,56)
# prop_flight_1_korea <- c(1,368)
# 
# prop_flight_1_belgium <- c(1,9)
# 
# # Add together same day
# prop_flight_2_germany <- prop_flight_2_germany + prop_flight_1_korea
# prop_flight_1_malaysia <- prop_flight_1_malaysia + prop_flight_1_belgium
# 
# propn_flight_matrix[date_range==date_flights_out_1_japan,] <- prop_flight_1_japan
# propn_flight_matrix[date_range==date_flights_out_2_japan,] <- prop_flight_2_japan
# propn_flight_matrix[date_range==date_flights_out_3_japan,] <- prop_flight_3_japan
# propn_flight_matrix[date_range==date_flights_out_2_germany,] <- prop_flight_2_germany
# propn_flight_matrix[date_range==date_flights_out_1_singapore,] <- prop_flight_1_singapore
# propn_flight_matrix[date_range==date_flights_out_1_malaysia,] <- prop_flight_1_malaysia
# propn_flight_matrix[date_range==date_flights_out_1_italy,] <- prop_flight_1_italy

# Extract Feb Wuhan data --------------------------------------------------

# cases_Wuhan <- data_hubei_Feb %>% filter(CNTY_CODE == 420100)
# cases_Wuhan <-  cases_Wuhan %>% mutate(new_case = NA)
# cases_Wuhan$new_case <- cases_Wuhan$total_case - c(NA,head(cases_Wuhan$total_case,-1)) 
# 
# case_data_wuhan_conf_time <- rep(NA,length(date_range))
# cutoff_time_wuhan <- max(cases_Wuhan$date)
# cutoff_min_wuhan <- as.Date("2020-01-21")
# 
# for(ii in 1:length(date_range)){
#   case_data_wuhan_conf_time[ii] = sum(cases_Wuhan[cases_Wuhan$date==date_range[ii],]$new_case)
# }
# 
# case_data_wuhan_conf_time[date_range>cutoff_time_wuhan | date_range<cutoff_min_wuhan] <- NA # omit all old and recent points
# 

# NEED REPORTING PARAMETER-----------------------------------
case_data_china_time[1:length(date_range)] <- NA
case_data_onset_time[1:length(date_range)] <- NA
case_data_matrix[1:nrow(case_data_matrix),1:ncol(case_data_matrix)] <- 0
# flight_report[1:length(flight_report)] <- NA
propn_flight_matrix[1:nrow(propn_flight_matrix),1:ncol(propn_flight_matrix)] <- NA


# case_data_Ezhou_n <- array(0,dim = (length(case_data_Ezhou) + 10))
# case_data_Ezhou_n[11:length(case_data_Ezhou_n)] <- case_data_Ezhou
# case_data_Ezhou <- case_data_Ezhou_n
# 
# case_data_Ezhou_n <- array(0,dim = (length(case_data_Ezhou_rec) + 10))
# case_data_Ezhou_n[11:length(case_data_Ezhou_n)] <- case_data_Ezhou_rec
# case_data_Ezhou_rec <- case_data_Ezhou_n
# 
# case_data_Ezhou_n <- array(0,dim = (length(case_data_Ezhou_death) + 10))
# case_data_Ezhou_n[11:length(case_data_Ezhou_n)] <- case_data_Ezhou_death
# case_data_Ezhou_death <- case_data_Ezhou_n

# Compile list of data to use:
# case_data_Ezhou<-case_data_Ezhou[-2]
# case_data_Ezhou_rec<-case_data_Ezhou_rec[-2]
# case_data_Ezhou_death<-case_data_Ezhou_death[-2]

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




