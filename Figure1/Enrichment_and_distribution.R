library(ggplot2)
library(ggrepel)
#plot the class enrichment of sex-shared DA-subfamilies in PM2.5-CHI
data<-read.table("supplementary file 5. PM2.5-CHI-SexShared_DA_subfamily_Class_ratio.txt",header = T)
ggplot(data, aes(x = DA_SAHRE_Frac, y = log2(Ratio),col=Class,size=DA_SHARE_Freq))+
  geom_point(alpha=0.7)+theme_classic()+scale_size(range = c(0.1, 5))+
  xlab("log2(enrichment)") +ylab("Fraction of TE class")+
  geom_hline(yintercept=1,linetype="dashed",size=0.5)+
  geom_vline(xintercept=0.3, linetype="dashed",size=0.5)

#plot the distribution of sex-shared LINE DA-subfamilies in PM2.5-CHI
data<-read.table("supplementary file 6. PM2.5-CHI-SexShared_subfamily_LINE.txt",header = T)
data<-data[data$Taxa=="Muridae" | data$Taxa=="Mus_musculus",]
ggplot(data, aes(x = logFC_F, y = logFC_M,col=Taxa,size=CPM_Mean))+
  geom_point(alpha=0.7)+theme_classic()+scale_size(range = c(0.1, 5))+
  xlab("log2FoldChange in female") +ylab("log2FoldChange in male")+
  geom_text_repel(data = subset(data),aes(label = TE),size = 1.5)

#boxplot of DNA methylation
dat<-data[,c(2,12:15)];a1<-dat[,c(1,2)];a2<-dat[,c(1,3)];a3<-dat[,c(1,4)];a4<-dat[,c(1,5)]
colnames(a1)<-c("TE","Value");colnames(a2)<-c("TE","Value");colnames(a3)<-c("TE","Value");colnames(a4)<-c("TE","Value");
a1$Type<-"Ctrl";a1$Sex<-"Female";a2$Type<-"PM2.5-CHI";a2$Sex<-"Female";a3$Type<-"Ctrl";a3$Sex<-"Male";a4$Type<-"PM2.5-CHI";a4$Sex<-"Male";
k<-rbind(a1,a2,a3,a4)
ggplot(k, aes(Type, Value,fill=Type))+scale_fill_brewer(palette = "Set3") +
  geom_boxplot(outlier.shape = NA,position = position_dodge2(preserve = "single"))+
  theme_classic()+geom_point(size=0.8)+facet_wrap(~Sex)+
  theme(axis.text.x = element_text(size=12,angle = 90, hjust = 1,vjust = 0.5),
        axis.text.y = element_text(size=12),
        plot.title = element_text(hjust = 0.5))
