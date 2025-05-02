library(ggplot2)

#DA-TEs overlap with cCREs
data<-read.table("DA-TEs_wcCRE.txt",header = F)
data$V3<-factor(data$V3,levels = c("None","CTCF-only","DNase-H3K4me3","PLS","pELS","dELS"))

ggplot(data, aes(x=V1, y=V4, fill=V3))+scale_fill_brewer(palette = "Set2")+
  geom_bar(stat="identity",position = "fill")+theme_classic()+facet_grid(~V2)+
  theme(axis.text.x = element_text(angle = 90, hjust = 1, size=7,vjust = 0.5))

#DA-TEs overlap with cCREs in different exposure
data<-read.table("DA-TEs_wcCRE_different_Exposure.txt",header = F)
data$V2<-factor(data$V2,levels = c("None","CTCF-only","DNase-H3K4me3","PLS","pELS","dELS"))

ggplot(data, aes(x=V1, y=V3, fill=V2))+scale_fill_brewer(palette = "Set2")+
  geom_bar(stat="identity",position = "fill")+theme_classic()+
  theme(axis.text.x = element_text(angle = 90, hjust = 1, size=7,vjust = 0.5))
