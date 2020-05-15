library(ggplot2)
library(readxl)
library("sf")
library(tidyverse)
library(tidyr)
library(ggpubr)
library(RColorBrewer)

prefectures <- readRDS("outputs/prefecture_names.rds")
r0_vals <- read_excel("../Combined.xlsx",sheet = "State-R0")
r0_vals$Region_EN[r0_vals$Region_EN == "Yulin"] <- "Yulin.2" 
r0_vals$Region_EN[r0_vals$Region_EN == "Yulin.1"] <- "Yulin" 
r0_vals$Region_EN[r0_vals$Region_EN == "Yulin.2"] <- "Yulin.1" 

r0_vals$Region_EN[r0_vals$Region_EN == "Yichun"] <- "Yichun.2" 
r0_vals$Region_EN[r0_vals$Region_EN == "Yichun.1"] <- "Yichun" 
r0_vals$Region_EN[r0_vals$Region_EN == "Yichun.2"] <- "Yichun.1" 


r0_vals$Region_EN[r0_vals$Region_EN == "Fuzhou"] <- "Fuzhou.2" 
r0_vals$Region_EN[r0_vals$Region_EN == "Fuzhou.1"] <- "Fuzhou" 
r0_vals$Region_EN[r0_vals$Region_EN == "Fuzhou.2"] <- "Fuzhou.1" 


incidence_df <- cbind(prefectures[r0_vals$Region_EN,1],r0_vals$`Region based R0 (A)`)
incidence_df <- as.data.frame(incidence_df)
colnames(incidence_df) <- c("ADM2_ZH","R0")
incidence_df$ADM2_ZH <- as.character(incidence_df$ADM2_ZH)
incidence_df[38,"ADM2_ZH"] <- "大兴安岭地区[4]"
incidence_df[57,"ADM2_ZH"] <- "伊犁哈萨克自治州[5]"
incidence_df[255,"ADM2_ZH"] <- "株州市"
incidence_df[267,"ADM2_ZH"] <- "济源市〔1〕"
incidence_df$ADM2_ZH <- factor(incidence_df$ADM2_ZH)


china_sf <- fortify(read_sf("chn_admbnda_adm2_ocha/chn_admbnda_adm2_ocha.shp"))

myPalette <- colorRampPalette((brewer.pal(9, "Oranges")))
sc <- scale_fill_gradientn(colours = myPalette(100),limits = c(0,3))

plotm <- function(incidence_df,china_sf){
  # print(incidence_df)
  china_df <- merge(china_sf,incidence_df,by="ADM2_ZH",all.x=T)
  zzz <- lapply(china_df$geometry,st_coordinates)
  zzz  <- lapply(zzz, apply,2,median)
  zzz <- do.call("rbind",zzz)
  china_df <- cbind(china_df,X= zzz[,"X"],Y = zzz[,"Y"])
  # china_df$R0 <- as.numeric(levels(china_df$R0))[china_df$R0]
  # print(china_df$R0)
  china_df$R0 <- sapply(china_df$R0,round,3)
  china_df$R0 <- replace_na(china_df$R0,0)
  china_df$R0 <- as.factor(china_df$R0)
  # print(colnames(china_df[1,]))
  # jpeg("1.jpeg")
  g <- ggplot(data = china_df,aes(fill = R0))+labs(fill = expression(R[0])) +
    geom_sf(lwd=.2)+#coord_sf(expand = F) +
    # geom_text(data = china_df,aes(X,Y,label=ADM2_EN),size=1)+
    scale_x_continuous(breaks = c(80,100,120))+
    scale_y_continuous(breaks = c(20,30,40,50))+
    # theme(legend.position = "bottom",legend.text = element_text(size = 12),
    #       legend.title = element_text(face = "bold",size = 12),
    #       legend.key.size = unit(14,"point"))+
    theme(legend.position = "bottom",legend.text = element_text(size = 16),
          legend.title = element_text(face = "bold",size = 18),
          legend.key.size = unit(18,"point"))+
    # sc
    scale_fill_manual(values = c("#ffffff","#00ba38", "#b4f27e", "#f8766d"))
  # dev.off()
  # ggsave("temp.jpeg",device = "jpeg")
  g
}
incidence_df$R0 <- as.numeric(levels(incidence_df$R0))[incidence_df$R0]
ga <- plotm(incidence_df,china_sf)
incidence_df$R0 <- r0_vals$`Region based R0 (B)`
incidence_df <- incidence_df[!is.na(incidence_df$R0),]
gb <- plotm(incidence_df,china_sf)
incidence_df$R0 <- r0_vals$`Region based R0( C )`
gc <- plotm(incidence_df,china_sf)
incidence_df$R0 <- r0_vals$`Region based R0 (D)`

# z <- prefectures["Linxia Hui Autonomous",] #  4 11 20 68 92
# incidence_df[incidence_df$ADM2_ZH == z,"R0"] <- 0

gd <- plotm(incidence_df,china_sf)
incidence_df$R0 <- r0_vals$`Region based R0 (E)`
ge <- plotm(incidence_df,china_sf)

incidence_df$R0 <- r0_vals$`Region based R0 (F)`
gf <- plotm(incidence_df,china_sf)


# g <- ggarrange(
#     ggarrange(ga,gb,gc,gd,ncol = 2,nrow = 2,labels = c("A","B","C","D"),label.x = .2),
#     ggarrange( ge,gf,ncol = 2,nrow = 1,labels = c("E","F"),label.x = .2),
#     nrow = 2,labels = c("",""),label.x = .2,heights = c(2,1))
# tiff("maps_6.tiff",height = 3000,width = 3000,res=300,units = "px"  )
# print(g)
# dev.off()


# g <- ggarrange(
#   ggarrange(ga,gb,gc,gd,ncol = 2,nrow = 2,labels = c("A","B","C","D"),label.x = .2,legend = "none"),
#   ggarrange( ge,gf,ncol = 2,nrow = 1,labels = c("E","F"),label.x = .2,common.legend = T,legend = "bottom"),
#   nrow = 2,labels = c("",""),label.x = .2,heights = c(1.7,1))
# tiff("maps_6_grad.tiff",height = 3000,width = 3000,res=300,units = "px"  )
# print(g)
# dev.off()

g <- ggarrange(ga,gc,gd,ge,ncol = 2,nrow = 2,labels = c("A","B","C","D"),label.x = .2)

tiff("maps_4.tiff",height = 3000,width = 3000,res=300,units = "px"  )
print(g)
dev.off()

# g <- ggarrange(ga,gc,gd,ge,ncol = 2,nrow = 2,labels = c("A","B","C","D"),
#                label.x = .2,common.legend = T,legend = "bottom")
# 
# tiff("maps_4_grad.tiff",height = 3000,width = 3000,res=300,units = "px"  )
# print(g)
# dev.off()


g <- ggarrange(gb,gf,ncol = 1,nrow = 2,labels = c("R","P"),label.x = .2)

tiff("maps_2.tiff",height = 3000,width = 3000,res=300,units = "px"  )
print(g)
dev.off()


# g <- ggarrange(gb,gf,ncol = 1,nrow = 2,labels = c("R","P"),label.x = .2,
#                common.legend = T,legend = "right")
# 
# tiff("maps_2_grad.tiff",height = 3000,width = 3000,res=300,units = "px"  )
# print(g)
# dev.off()
# 


# tiff("map_tmp.tiff",height = 3000,width = 3000,res=300,units = "px"  )
# print(gf)
# dev.off()



