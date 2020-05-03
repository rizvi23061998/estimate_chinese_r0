library(ggplot2)
library(readxl)
library(ggplot2)
library("sf")
library(tidyverse)
library(tidyr)
library(ggpubr)

prefectures <- readRDS("outputs/prefecture_names.rds")
r0_vals <- read_excel("../Combined.xlsx",sheet = "State-R0")
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
plotm <- function(incidence_df,china_sf){
  # print(incidence_df)
  china_df <- merge(china_sf,incidence_df,by="ADM2_ZH",all.x=T)
  # china_df$R0 <- as.numeric(levels(china_df$R0))[china_df$R0]
  # print(china_df$R0)
  china_df$R0 <- sapply(china_df$R0,round,3)
  china_df$R0 <- replace_na(china_df$R0,0) 
  china_df$R0 <- as.factor(china_df$R0)
  # jpeg("1.jpeg")
  g <- ggplot(data = china_df,aes(fill = R0))  + scale_fill_manual(values = c("#7c7c7c","#00ba38", "#b4f27e", "#f8766d")) +  geom_sf()
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
gd <- plotm(incidence_df,china_sf)
incidence_df$R0 <- r0_vals$`Region based R0 (E)`
ge <- plotm(incidence_df,china_sf)

g <- ggarrange(ggarrange(ga,gb,gc,gd,ncol = 2,nrow = 2,labels = c("A","B","C","D")),ge,nrow = 2,labels = c("","E"),heights = c(2,1))
tiff("maps.tiff",height = 3000,width = 3000,res=300,units = "px"  )
print(g)
dev.off()
