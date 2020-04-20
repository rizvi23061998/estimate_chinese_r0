library(readxl)
library(plyr)
set.seed(123)

china_incidence <- read_excel("data/chinesedata/Copy of China_C19.dbf.xlsx")
china_incidence <- china_incidence[!is.na(china_incidence$Region_EN),]
china_incidence$Region_EN <- make.unique(china_incidence$Region_EN)

china_population <- read_excel("data/chinesedata/population data-zwy.xlsx")
china_population <- china_population[!is.na(china_population$Region_EN),]
china_population$Region_EN <- make.unique(china_population$Region_EN)


data <- join(china_incidence,china_population,by = "Region_EN")
data <- data[data$Region_EN != "Wuhan",]
data$地区名称 <- NULL

data = data[which(data$Incidence != 0),]
data = data[which(data$Recovery_R != 0),]
data = data[which(data$Mortality != 0),]

# 
# new_data = data[,c("Incidence","Recovery_R","Mortality")]
# cls=kmeans(new_data,3)
# data$group <- cls$cluster
# 
# n_data <- data[,!(colnames(data) %in% c("2018total","2018male","2018female","age 0-14","age15-64"  ,"age>65"))]
# n_data<- n_data[which(n_data$group==3),]
# print(n_data)
# 
# data_init <- data
# 
# group1 <- data_init[which(data_init$group == 1),"Region_EN"]
# group2 <- data_init[which(data_init$group == 2),"Region_EN"]
# group3 <- data_init[which(data_init$group == 3),"Region_EN"]
# 
# # print(group1)
# # 
# write_rds(group1,"outputs/group1_incidence.rds")
# write_rds(group2,"outputs/group2_incidence.rds")
# write_rds(group3,"outputs/group3_incidence.rds")

min_temp <- read_excel("data/chinesedata/Min_Temperature.xlsx")
min_temp <- as.data.frame(min_temp)
min_temp$Region_CN = NULL
rownames(min_temp) <- make.unique(min_temp$Region_EN)
min_temp$Region_EN <-NULL
min_col <- apply(min_temp, 1, FUN=min)
min_temp$min_min_col <- min_col
mean_col <- apply(min_temp, 1, FUN=mean)
min_temp$mean_min_col <- mean_col
max_col <- apply(min_temp, 1, FUN=max)
min_temp$max_min_col <- max_col
min_temp <- min_temp[,c("min_min_col","mean_min_col","max_min_col")]



mean_temp <- read_excel("data/chinesedata/Mean_Temperature.xlsx")
mean_temp <- as.data.frame(mean_temp)
mean_temp$Region_CN = NULL
rownames(mean_temp) <- make.unique(mean_temp$Region_EN)
mean_temp$Region_EN <-NULL
min_col <- apply(mean_temp, 1, FUN=min)
mean_temp$min_mean_col <- min_col
mean_col <- apply(mean_temp, 1, FUN=mean)
mean_temp$mean_mean_col <- mean_col
max_col <- apply(mean_temp, 1, FUN=max)
mean_temp$max_mean_col <- max_col
mean_temp <- mean_temp[,c("min_mean_col","mean_mean_col","max_mean_col")]



max_temp <- read_excel("data/chinesedata/Max_Temperature.xlsx")
max_temp <- as.data.frame(max_temp)
max_temp$Region_CN = NULL
rownames(max_temp) <- make.unique(max_temp$Region_EN)
max_temp$Region_EN <-NULL
min_col <- apply(max_temp, 1, FUN=min)
max_temp$min_max_col <- min_col
mean_col <- apply(max_temp, 1, FUN=mean)
max_temp$mean_max_col <- mean_col
max_col <- apply(max_temp, 1, FUN=max)
max_temp$max_max_col <- max_col
max_temp <- max_temp[,c("min_max_col","mean_max_col","max_max_col")]


min_temp$Region_EN <- rownames(min_temp)
mean_temp$Region_EN <- rownames(mean_temp)
max_temp$Region_EN <- rownames(max_temp)
data_all <- join(data,min_temp,by="Region_EN")
data_all <- join(data_all,mean_temp,by="Region_EN")
data_all <- join(data_all,max_temp,by="Region_EN")
data_all <- drop_na(data_all)
non_required_cols <- c("Pop_2018", "Total_Recover" ,"Tota_death","Total_confirm_cases")
data_all <- data_all[,!(colnames(data_all) %in% non_required_cols)]
data_all <- data_all %>% mutate(mf_ratio = (`2018male` / `2018female`))
data_all <- data_all %>% mutate(age65_ratio = (`age>65` / `2018total`))
non_required_cols_1 <- c("age 0-14", "age15-64",  "age>65","2018total","2018male", "2018female")
data_all <- data_all[,!(colnames(data_all) %in% non_required_cols_1)]

new_data <- data_all
new_data$Region_EN <- NULL
new_data <- apply(new_data, 2, scale)
cls=kmeans(new_data,3)
data_all$group <- cls$cluster

n_data <- data_all
n_data<- n_data[which(n_data$group==1),]
print(n_data)
