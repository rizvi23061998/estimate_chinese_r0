library(readxl)
library(plyr)
set.seed(123)
library(Rtsne)

china_incidence <- read_excel("data/chinesedata/Copy of China_C19.dbf.xlsx")
china_incidence <- china_incidence[!is.na(china_incidence$Region_EN),]
china_incidence$Region_EN <- make.unique(china_incidence$Region_EN)

china_population <- read_excel("data/chinesedata/population data-zwy.xlsx")
china_population <- china_population[!is.na(china_population$Region_EN),]
china_population$Region_EN <- make.unique(china_population$Region_EN)


data <- join(china_incidence,china_population,by = "Region_EN")
# data <- data[data$Region_EN != "Wuhan",]

# name_data <- china_population[,c("Region_EN","地区名称")]
# rownames(name_data) <- name_data$Region_EN
# name_data$Region_EN <- NULL
# write_rds(name_data,"outputs/prefecture_names.rds")
data$地区名称 <- NULL



# data = data[which(data$Incidence != 0),]


data <- drop_na(data)
 
new_data = data[,c("Incidence","Recovery_R","Mortality")]
# new_data <- apply(new_data, 2, scale)
# cls=kmeans(new_data,3)
# data$group <- cls$cluster
# new_data <- cls$cluster
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
# write_rds(group1,"outputs/rds/group1_incidence.rds")
# write_rds(group2,"outputs/rds/group2_incidence.rds")
# write_rds(group3,"outputs/rds/group3_incidence.rds")
# 
min_temp <- read_excel("data/chinesedata/Min_Temperature.xlsx")
min_temp <- as.data.frame(min_temp)
min_temp$Region_CN = NULL
rownames(min_temp) <- make.unique(min_temp$Region_EN)
min_temp$Region_EN <-NULL
min_col <- apply(min_temp, 1, FUN=min)
min_temp$min_min_temp <- min_col
mean_col <- apply(min_temp, 1, FUN=mean)
min_temp$mean_min_temp <- mean_col
max_col <- apply(min_temp, 1, FUN=max)
min_temp$max_min_temp <- max_col
min_temp <- min_temp[,c("min_min_temp","mean_min_temp","max_min_temp")]



mean_temp <- read_excel("data/chinesedata/Mean_Temperature.xlsx")
mean_temp <- as.data.frame(mean_temp)
mean_temp$Region_CN = NULL
rownames(mean_temp) <- make.unique(mean_temp$Region_EN)
mean_temp$Region_EN <-NULL
min_col <- apply(mean_temp, 1, FUN=min)
mean_temp$min_mean_temp <- min_col
mean_col <- apply(mean_temp, 1, FUN=mean)
mean_temp$mean_mean_temp <- mean_col
max_col <- apply(mean_temp, 1, FUN=max)
mean_temp$max_mean_temp <- max_col
mean_temp <- mean_temp[,c("min_mean_temp","mean_mean_temp","max_mean_temp")]



max_temp <- read_excel("data/chinesedata/Max_Temperature.xlsx")
max_temp <- as.data.frame(max_temp)
max_temp$Region_CN = NULL
rownames(max_temp) <- make.unique(max_temp$Region_EN)
max_temp$Region_EN <-NULL
min_col <- apply(max_temp, 1, FUN=min)
max_temp$min_max_temp <- min_col
mean_col <- apply(max_temp, 1, FUN=mean)
max_temp$mean_max_temp <- mean_col
max_col <- apply(max_temp, 1, FUN=max)
max_temp$max_max_temp <- max_col
max_temp <- max_temp[,c("min_max_temp","mean_max_temp","max_max_temp")]


min_temp$Region_EN <- rownames(min_temp)
mean_temp$Region_EN <- rownames(mean_temp)
max_temp$Region_EN <- rownames(max_temp)
data_all <- join(data,min_temp,by="Region_EN")
data_all <- join(data_all,mean_temp,by="Region_EN")
data_all <- join(data_all,max_temp,by="Region_EN")
data_all <- drop_na(data_all)
non_required_cols <- c("Pop_2018", "Total_Recover" ,"Tota_death","Total_confirm_cases")
data_all <- data_all[,!(colnames(data_all) %in% non_required_cols)]
# data_all <- data_all %>% mutate(mf_ratio = (`2018male` / `2018female`))
data_all <- data_all %>% mutate(age65_ratio = (`age>65` / `2018total`))
data_all <- data_all %>% mutate(age15_to64_ratio = (`age15-64` / `2018total`))
data_all <- data_all %>% mutate(age0_to14_ratio = (`age 0-14` / `2018total`))
non_required_cols_1 <- c("age 0-14", "age15-64",  "age>65","2018total","2018male", "2018female")
data_all <- data_all[,!(colnames(data_all) %in% non_required_cols_1)]

non_required_cols_2 <- c("Incidence","Mortality","Recovery_R")
# data_all <- data_all[,!(colnames(data_all) %in% non_required_cols_2)]


non_required_cols_3 <- c("min_min_temp","mean_min_temp","max_min_temp",
                         "min_mean_temp","mean_mean_temp","max_mean_temp",
                         "min_max_temp","mean_max_temp","max_max_temp")
# data_all <- data_all[,!(colnames(data_all) %in% non_required_cols_3)]

new_data <- data_all#[,c("Region_EN",non_required_cols_2)]
new_data$Region_EN <- NULL
# new_data$mf_ratio <- NULL

new_data <- apply(new_data, 2, scale)
cls=kmeans(new_data,3)
data_all$group <- cls$cluster

group1 <- data_all[which(data_all$group == 1),"Region_EN"]
group2 <- data_all[which(data_all$group == 2),"Region_EN"]
group3 <- data_all[which(data_all$group == 3),"Region_EN"]


print(colnames(data_all))

#
# # print(group1)
# #
# write_rds(group1,"outputs/rds/group1_all.rds")
# write_rds(group2,"outputs/rds/group2_all.rds")
# write_rds(group3,"outputs/rds/group3_all.rds")

# write_rds(group1,"outputs/rds/group1_without_mf.rds")
# write_rds(group2,"outputs/rds/group2_without_mf.rds")
# write_rds(group3,"outputs/rds/group3_without_mf.rds")


# write_rds(group1,"outputs/rds/group1_all_age.rds")
# write_rds(group2,"outputs/rds/group2_all_age.rds")
# write_rds(group3,"outputs/rds/group3_all_age.rds")
#
# write_rds(group1,"outputs/rds/group1_temp.rds")
# write_rds(group2,"outputs/rds/group2_temp.rds")
# write_rds(group3,"outputs/rds/group3_temp.rds")


# write_rds(group1,"outputs/rds/group1_only_age.rds")
# write_rds(group2,"outputs/rds/group2_only_age.rds")
# write_rds(group3,"outputs/rds/group3_only_age.rds")



# group1 <- data_all[which(data_all$group == 1),]
# group2 <- data_all[which(data_all$group == 2),]
# group3 <- data_all[which(data_all$group == 3),]
#
# group1 <- format(group1,digits=2,nsmall = 2)
# group2 <- format(group2,digits=2,nsmall = 2)
# group3 <- format(group3,digits=2,nsmall = 2)
#
# write.csv(group1,"outputs/group1_all_details.csv")
# write.csv(group2,"outputs/group2_all_details.csv")
# write.csv(group3,"outputs/group3_all_details.csv")

# library(Rtsne)
# 
# library(factoextra)
new_data <- data_all#[,c("Region_EN","Incidence","Mortality","Recovery_R","group")]#data_all
rownames(new_data) <- new_data$Region_EN
new_data$Region_EN <- NULL
# write_rds(new_data,"outputs/pca/d6.rds")
# groups <- as.factor(new_data$group)
# new_data$group <- NULL
# res <- prcomp(new_data,scale = TRUE)
# write_rds(res,"outputs/pca/c1.rds")
# fviz_pca_ind(res,
#              col.ind = groups, # color by groups
#              palette = c("#00AFBB",  "#FC4E07","black"),
#              addEllipses = TRUE, # Concentration ellipses
#              ellipse.type = "confidence",
#              legend.title = "Groups",
#              repel = TRUE
# )
# colors = rainbow(length(unique(groups)))
# names(colors) = unique(groups)
# 
# tsne <-Rtsne(new_data,dims = 2, perplexity=30, verbose=TRUE, max_iter = 500,check_duplicates=F)
# 
# plot(tsne$Y, t='n', main="tsne",xlab = "Dimension1",ylab="Dimension2")
# text(tsne$Y, labels=rownames(new_data), col=colors[groups])
# write_rds(tsne,"outputs/tsne/c1.rds")

# print(colnames(data_all))

 
# h_clust <- hclust(dist(new_data))
# plot(h_clust)
# clusterCut <- cutree(h_clust, 20)
# wuhan_cluster<- names(clusterCut[clusterCut == clusterCut["Wuhan"]])
# write_rds(wuhan_cluster,"outputs/rds/wuhan_cluster.rds")
# type = "only_age"
# g1 <- readRDS(paste("outputs/rds/group1_",type,".rds",sep = ""))
# g2 <- readRDS(paste("outputs/rds/group2_",type,".rds",sep = ""))
# g3 <- readRDS(paste("outputs/rds/group3_",type,".rds",sep = ""))
# s <- read.csv("states.csv",header = T)
# s <- s[,2]
# z = array(0,dim = length(s))
# for (i in 1:length(s)){
#   # print(s[i,])
#   if( s[i] %in% g1){
#     z[i] = 1
#   }else if(s[i] %in% g2){
#     z[i] = 2
#   }else if(s[i] %in% g3){
#     z[i] = 3
# 
#   }
#   else{
#     z[i] <- NA
#   }
# }
# 
# write.csv(z,"tmp.csv")

# testData <- data_all
# rownames(testData) <- data_all$Region_EN
# testData$Region_EN <- NULL
# 
