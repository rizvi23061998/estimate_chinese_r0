library(Rtsne)
library(factoextra)
library(ggpubr)

pca_list = vector('list',5)
tsne_list  = vector('list',5)
for(i in seq(1,5)){
  data_file = paste("outputs/pca/d",i,".rds",sep = "")
  new_data <- readRDS(data_file)
  groups <- as.factor(new_data$group)
  new_data$group <- NULL
  res <- prcomp(new_data,scale = TRUE)
  pca_file = paste("outputs/pca/c",i,".rds",sep  = "")
  res <- readRDS(pca_file)
  
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
                      repel = FALSE,
                      geom = "point"
    )  + ggtitle("PCA") + labs(x = "Dimension1",y="Dimension2")
    # print(tsne_data)
    
    print(p)
  })
  colors = rainbow(length(unique(groups)))
  names(colors) = unique(groups)
  
  tsne_file = paste("outputs/tsne/c",i,".rds",sep = "")
  tsne <-readRDS(tsne_file)
  
  tsne_data <- as.data.frame(tsne$Y)
  colnames(tsne_data) <- c("x","y")
  Groups <- factor(groups)
  # print(Groups)
  
  tsne_list[[i]] <- local({
    i <- i
    Groups <- Groups
    # print(tsne_data)
    t <- ggplot(tsne_data) + geom_point(aes(x=x, y=y, color=Groups)) + labs(x = "Dimension1",y="Dimension2",color="Regions")
    print(t)
  })
  
}

g <- ggarrange(ggarrange(plotlist =tsne_list[c(1,2,3,4)],ncol = 2,nrow = 2,labels = c("A","B","C","D"),legend = "none"),tsne_list[[5]],nrow = 2,labels = c("","E"),heights = c(2,1),common.legend = T,legend = "top")
tiff("tsne.tiff",height = 3000,width = 3000,res=300,units = "px"  )
print(g)
dev.off()

g <- ggarrange(ggarrange(plotlist =pca_list[c(1,2,3,4)],ncol = 2,nrow = 2,labels = c("A","B","C","D"),legend = "none"),pca_list[[5]],nrow = 2,labels = c("","E"),heights = c(2,1),common.legend = T,legend = "top")
tiff("pca.tiff",height = 3000,width = 3000,res=300,units = "px"  )
print(g)
dev.off()

