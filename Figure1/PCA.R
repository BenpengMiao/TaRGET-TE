library(plotly)
library(ggplot2)
library(ggpubr)
library(RUVSeq)
library(RColorBrewer)

data<-read.table("ATAC_TE_subfamily_liver_5month.txt",header=T)
#data<-read.table("RNA_TE_subfamily_liver_5month.txt",header=T)

d<-cpm(data+1)
n<-colnames(d)

lab<-c()
trt<-c()
sex<-c()
tis<-c()
stage<-c()
for(j in 1:length(n)){
  lab[j]<-strsplit(as.character(n[j]),"_")[[1]][1]
  trt[j]<-strsplit(as.character(n[j]),"_")[[1]][3]
  if(trt[j] != "Ctrl"){
    if((lab[j] == "BI") || (lab[j] == "MU")){
      trt[j]<-paste(lab[j],trt[j],sep=".")
    }
  }
  sex[j]<-strsplit(as.character(n[j]),"_")[[1]][6]
  tis[j]<-strsplit(as.character(n[j]),"_")[[1]][5]
  stage[j]<-strsplit(as.character(n[j]),"_")[[1]][7]
}
lab<-paste0(lab,"_assay")
tissue<-paste(tis,stage,sep="_")
l<-sum(!duplicated(trt))

m<-t(log2(d))
pca<-prcomp(m,scale. = T)
pca_plot<-as.data.frame(pca$x[,1:3])
pca_plot$lab<-lab
pca_plot$exp<-trt
pca_plot$sex<-sex
pca_plot$tissue<-tissue
pca_plot$name<-rownames(m)
sdev_df <- data.frame(components = paste0("PC", 1:length(pca$sdev)), value = pca$sdev ** 2)
sdev_df$value <- sdev_df$value / sum(sdev_df$value) * 100
pca_plot$pc1_perc <- sdev_df$value[1] %>% round(., digits = 2)
pca_plot$pc2_perc <- sdev_df$value[2] %>% round(., digits = 2)
pca_plot$pc3_perc <- sdev_df$value[3] %>% round(., digits = 2)

cols <- brewer.pal(length(unique(pca_plot$exp)), "Set3");

p1 <- ggplot(pca_plot, aes(x = PC1, y = PC2, col = exp, shape = sex)) +
  geom_point(size = 3) +
  xlab(paste0("PC1 (", unique(pca_plot$pc1_perc), "%)")) +
  ylab(paste0("PC2 (", unique(pca_plot$pc2_perc), "%)")) +
  scale_shape_manual(values = c(4, 19)) +
  scale_color_manual(values=cols[1:l])+
  theme_classic()+
  theme(title=element_text(size=12,face="bold"))+
  theme(legend.text=element_text(size=12))
p2 <- ggplot(pca_plot, aes(x = PC1, y = PC3, col = exp, shape = sex)) +
  geom_point(size = 3) +
  xlab(paste0("PC1 (", unique(pca_plot$pc1_perc), "%)")) +
  ylab(paste0("PC3 (", unique(pca_plot$pc3_perc), "%)")) +
  scale_shape_manual(values = c(4, 19)) +
  scale_color_manual(values=cols[1:l]) +
  theme_classic()+
  theme(title=element_text(size=12,face="bold"))+
  theme(legend.text=element_text(size=12))
ggarrange(p1, p2, nrow = 2)

