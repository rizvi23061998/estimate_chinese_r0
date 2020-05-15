library(DMwR)
group_names <- c("group1_all","group2_all","group3_all",
                 "group1_incidence","group2_incidence","group3_incidence",
                 "group1_without_mf","group2_without_mf","group3_without_mf",
                 "group1_all_age","group2_all_age","group3_all_age",
                 "group1_temp","group2_temp","group3_temp")


performance_list <- data.frame("mae" = double(),"mse" = double(),"rmse" = double(),"mape" = double())
colnames(performance_list) <- c("mae","mse","rmse","mape")

cumulative_case <- array(0,dim=15)
i= 1
# group_names <- c("group1_all","group2_all","group3_all")
for (group_name in group_names){
  # print(group_name)
  file_name <- paste("outputs/case_models/validation/case_model_",group_name,".csv",sep = "")
  case_data_time <- read.csv(file_name) 
  pred_cases <- cumsum(case_data_time$cases) 
  real_cases <- cumsum(case_data_time$real) 
  performance_list[nrow(performance_list)+1,] <- DMwR::regr.eval(real_cases,pred_cases)
  # plot(seq(1:length(real_cases)),real_cases)
  cumulative_case[i] <- real_cases[length(real_cases)]
  print(cumulative_case[i])
  i = i+1 
}
# print(performance_list)
rmse <- matrix(data=performance_list$rmse,nrow = 3,ncol=5)
rmse <- as.data.frame((rmse))
write.csv(rmse,"rmse.csv")


cumulative_case <- matrix(data=cumulative_case,nrow = 3,ncol = 5)
print(cumulative_case)
write.csv((cumulative_case),"cumulative_cases.csv")
# apply(cumulative_case,2,sum)
