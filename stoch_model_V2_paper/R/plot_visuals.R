library(Rtsne)
library(factoextra)
library(ggpubr)

pca_list = vector('list',4)
tsne_list  = vector('list',)
pca_var_list <- vector('list',12)
cluster_name <- c('A','R','B','C','D','P')
# sq <- c(1,3,4,5)
sq <- c(2,6)
for(i in sq){
  data_file = paste("outputs/pca/d",i,".rds",sep = "")
  new_data <- readRDS(data_file)
  groups <- as.factor(new_data$group)
  new_data$group <- NULL
  res <- prcomp(new_data,scale = TRUE)
  # print(colnames(new_data))
  # pca_file = paste("outputs/pca/c",i,".rds",sep  = "")
  # res <- readRDS(pca_file)
  
  pca_list[[i]] <- local({
    i <- i
    groups <- groups
    p <- fviz_pca_ind(res,
                      col.ind = groups, # color by groups
                      palette = c("#00AFBB",  "#FC4E07","black"),
                      addEllipses = TRUE, # Concentration ellipses
                      ellipse.type = "confidence",
                      legend.title = "Regions",
                      # legend="none",
                      #repel = TRUE,
                      #geom = c("arrow,text")
                      geom = "point"
    )
    
    p <- p + ggtitle(paste("PCA Individuals(Cluster ",cluster_name[i],")",sep = ""))
    #+ labs(x = "Dimension1",y="Dimension2")
    # print(tsne_data)
    # pv <- fviz_eig(res)
    # tiff(paste("pca_",cluster_name[i],".tiff",sep = ""),res=300,height = 3000,
    #        width = 3000)
    # print(p)
    # dev.off()
    (p)
    
  })
  
  pca_var_list[[i]] <- local({
    i <- i
    groups <-groups
    pv <- fviz_pca_var(res,
                       col.var = "contrib", # Color by contributions to the PC
                       gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
                       repel = TRUE     # Avoid text overlapping
    )+ ggtitle(paste("PCA Variable(Cluster ",cluster_name[i],")",sep = ""))+
      labs(color="Contribution")
    (pv)
  })
  
  colors = rainbow(length(unique(groups)))
  names(colors) = unique(groups)
  
  # ---------deprecated tsne ------------------
  # tsne_file = paste("outputs/tsne/c",i,".rds",sep = "")
  # tsne <-readRDS(tsne_file)
  # 
  # tsne_data <- as.data.frame(tsne$Y)
  # colnames(tsne_data) <- c("x","y")
  # Groups <- factor(groups)
  # # print(Groups)
  # 
  # tsne_list[[i]] <- local({
  #   i <- i
  #   Groups <- Groups
  #   # print(tsne_data)
  #   t <- ggplot(tsne_data) + geom_point(aes(x=x, y=y, color=Groups)) + labs(x = "Dimension1",y="Dimension2",color="Regions")
  #   print(t)
  # })
  
}

# g <- ggarrange(ggarrange(plotlist =tsne_list[c(1,2,3,4)],ncol = 2,nrow = 2,labels = c("A","B","C","D"),legend = "none"),tsne_list[[5]],nrow = 2,labels = c("","E"),heights = c(2,1),common.legend = T,legend = "top")
# tiff("tsne.tiff",height = 3000,width = 3000,res=300,units = "px"  )
# print(g)
# dev.off()

# g <- ggarrange(plotlist =pca_list[sq],ncol = 2,nrow = 2,common.legend = T,legend = "top")
# tiff("pca_new_ind_4.tiff",height = 3000,width = 3000,res=300,units = "px"  )
# print(g)
# dev.off()

g <- ggarrange(plotlist =pca_list[sq],ncol = 1,nrow = 2,common.legend = T,legend = "top")
tiff("pca_new_ind_2.tiff",height = 3000,width = 3000,res=300,units = "px"  )
print(g)
dev.off()


# g <- ggarrange(ggarrange(plotlist =pca_var_list[c(1,2,3,4)],ncol = 2,nrow = 2),pca_var_list[[5]],nrow = 2,heights = c(2,1))
# tiff("pca_new_var_4.tiff",height = 3000,width = 3000,res=300,units = "px"  )
# print(g)
# dev.off()


# g <- ggarrange(plotlist =pca_var_list[sq],ncol = 2,nrow = 2)
# tiff("pca_new_var_4.tiff",height = 3000,width = 3000,res=300,units = "px"  )
# print(g)
# dev.off()

g <- ggarrange(plotlist =pca_var_list[sq],ncol = 1,nrow = 2)
tiff("pca_new_var_2.tiff",height = 3000,width = 3000,res=300,units = "px"  )
print(g)
dev.off()

# ------- case model plot ----------------

# group_names <- c("group1_all","group2_all","group3_all",
#                  "group1_without_mf","group2_without_mf","group3_without_mf",
#                  "group1_all_age","group2_all_age","group3_all_age",
#                  "group1_temp","group2_temp","group3_temp")
# case_model_plot_list <- vector("list",12)
# i = 1
# for(group_name in group_names){
#   # print(group_name)
#   
#   case_model_plot_list[[i]] <- local({
#     group_name <- group_name  
#     file_name <- paste("outputs/case_models/validation/case_gg_",group_name,".rds",sep = " ")
#     g <- readRDS(file_name) 
#     g <- g +
#       theme(legend.text = element_text(size = 18),
#             legend.title = element_text(face = "bold",size = 18),
#             legend.key.size = unit(18,"point"))
# 
#     (g)
#   })
#   i = i+1
# }
# 
# lab <- c("A.1",'A.2',"A.3",
#          "B.1",'B.2',"B.3",
#          "C.1",'C.2',"C.3",
#          "D.1",'D.2',"D.3")
# 
# g <- ggarrange(plotlist =  case_model_plot_list[c(1:12)],
#                ncol = 3,nrow = 4,labels = lab,
#                common.legend = T,legend = "top",label.x = .2)
# 
# tiff("case_model_4.tiff",height = 3000,width = 3000,res=300,units = "px")
# print(g)
# dev.off()
  


