
# Load datasets, functions and parameters ----------------------------------------------

# - - -
# Load datasets
travel_data_mobs <- read_csv(paste0(dropbox_path,"data/connectivity_data_mobs.csv"))
#travel_data_mobs <- read_csv(paste0(dropbox_path,"data/connectivity_data_worldpop.csv"))
international_conf_data_in <- read_csv(paste0(dropbox_path,"data/international_case_data.csv"))
international_onset_data_in <- read_csv(paste0(dropbox_path,"data/time_series_WHO_report.csv"))
china_onset_data_in <- read_csv(paste0(dropbox_path,"data/time_series_data_bioRvix_Liu_et_al.csv"))
wuhan_onset_data_in <- read_csv(paste0(dropbox_path,"data/time_series_data_lancet_huang_et_al.csv"))
wuhan_onset_2020_01_30 <- read_csv(paste0(dropbox_path,"data/time_series_data_qui_li_nejm_wuhan.csv"))
wuhan_conf_data_in <- read_csv(paste0(dropbox_path,"data/time_series_HKU_Wuhan.csv"))

data_hubei_Feb <- read_csv(paste0(dropbox_path,"data/hubei_confirmed_cases.csv"))

case_data_in <- international_conf_data_in
travel_data <- travel_data_mobs

t_step <- 0.25

# - - -
# Load model and plotting functions
source("R/model_functions.R")
source("R/plotting_functions.R")

# - - -
# Load model parameters

print("Loading initial parameters .. ..")

thetaR_IC <- read_csv("inputs/theta_initial_conditions.csv")
theta <- c( r0=as.numeric(thetaR_IC[thetaR_IC$param=="r0","value"]), # note this is only IC - SMC estimates this
            beta=NA,
            betavol=as.numeric(thetaR_IC[thetaR_IC$param=="betavol","value"]),
            gentime=as.numeric(thetaR_IC[thetaR_IC$param=="gentime","value"]), # not used currently
            incubation = 1/as.numeric(thetaR_IC[thetaR_IC$param=="incubation","value"]),
            report = 1/as.numeric(thetaR_IC[thetaR_IC$param=="report","value"]),
            report_local = 1/as.numeric(thetaR_IC[thetaR_IC$param=="report_local","value"]),
            recover = 1/as.numeric(thetaR_IC[thetaR_IC$param=="recover","value"]),
            init_cases=as.numeric(thetaR_IC[thetaR_IC$param=="init_cases","value"]),
            passengers=as.numeric(thetaR_IC[thetaR_IC$param=="outbound_travel","value"]),
            pop_travel=as.numeric(thetaR_IC[thetaR_IC$param=="population_travel","value"]),
            local_rep_prop=as.numeric(thetaR_IC[thetaR_IC$param=="local_rep_prop","value"]), # local propn reported
            onset_prop=as.numeric(thetaR_IC[thetaR_IC$param=="onset_prop","value"]), # propn onsets known
            onset_prop_int=as.numeric(thetaR_IC[thetaR_IC$param=="onset_prop_int","value"]), # propn onsets known internationally
            confirmed_prop=as.numeric(thetaR_IC[thetaR_IC$param=="confirmed_prop","value"]), # propn confirmed reported
            travel_frac=NA,
            r0_decline =as.numeric(thetaR_IC[thetaR_IC$param=="r0_decline","value"]), # decline in R0 for scenario analysis
            rep_local_var =as.numeric(thetaR_IC[thetaR_IC$param=="rep_local_var","value"]), # dispersion in local reporting confirmed cases
            pre_symp =as.numeric(thetaR_IC[thetaR_IC$param=="pre_symp","value"]) # transmission in 2nd half of Erland period (binary term)
)


# theta[["travel_frac"]] <- theta[["passengers"]]/theta[["pop_travel"]] # Estimate fraction that travel

#=======================Travelling reduced to zero=========================
theta[["travel_frac"]] <- 0 # Estimate fraction that travel
#==========================================================================

group <- readRDS(paste("outputs/rds/",group_name,".rds",sep = ''))

china_population <- as.data.frame(china_population)
# china_population[is.na(china_population$`2018total`),"2018total"]<- mean(china_population$`2018total`)
rownames(china_population) <- china_population$Region_EN
pop_group <- china_population[group,"2018total"]
pop_group[is.na(pop_group)] <- 0 #mean(pop_group,na.rm = T) 
theta[["pop_travel"]] <- sum(pop_group)

theta[["beta"]] <- theta[["r0"]]*(theta[["recover"]]) # Scale initial value of R0

theta_initNames <- c("sus","tr_exp1","tr_exp2","exp1","exp2","inf1","inf2","tr_waiting","cases","reports","waiting_local","cases_local","reports_local","rem") # also defines groups to use in model
forecast_window <- 0

# - - -
# Load timeseries -  specify travel data being used
# NOTE: USES REPORTING DELAY AS INPUT
source("R/load_timeseries_data.R")

# theta[["init_cases"]] <- case_data_Ezhou[1

# Run set up check --------------------------------------------------------------

print("Checking setup")

# - - -
# Run SMC and check likelihood
output_smc <- smc_model(theta,
                        nn=1e3, # number of particles
                        dt=t_step
)

output_smc$lik

print(output_smc$lik)

# Run main outputs --------------------------------------------------------------
print("Running main outputs ..")
source("R/outputs_main.R")


  

