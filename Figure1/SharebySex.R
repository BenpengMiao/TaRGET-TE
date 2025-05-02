
library(tidyr);library(dbplyr);library(RUVSeq);library(stringr);library(dplyr)
#ATAC-seq: number of DA-subfamilies shared by female and male
data<-read.table("supplementary file 3. ATAC_liver_adt_subfamily_treatment_deg_all_control_edgeR.txt",header = T)
data<-data[data$Class=="SINE"|data$Class=="LINE"|data$Class=="LTR"|data$Class=="DNA", ]
data$Significant <- ifelse(data$FDR < 0.001 & abs(data$logFC)>0.15
                           & (data$Control_CPM_median>=100 | data$Exposure_CPM_median>=100), "TRUE", "FALSE")
data$direction<-ifelse(data$logFC>0, "up","down")
da<-data[data$Significant==TRUE,];d<-da[,c("Subfamily","Type","Significant","Sex")]
f<-spread(d,Sex,Significant);f[is.na(f)]<-"FALSE";k<-f[f$F==TRUE & f$M==TRUE,]
d$group<-paste(d$Subfamily,d$Type,sep=",");k$group<-paste(k$Subfamily,k$Type,sep=",")
d$SexShare<-d$group %in% k$group;a<-as.data.frame(table(d[,c(2,4,6)]))
a$SexShare<-factor(a$SexShare,levels = c("TRUE","FALSE"))
b<-as.data.frame(a[a$SexShare=="FALSE",] %>% group_by(Type) %>% summarise(across(c(Freq), c(sum))))
b$SexShare<-"FALSE";c<-a[a$Sex=="F" & a$SexShare=="TRUE",]
colnames(b)<-c("type","Freq","SexShare");c$Fraction<-c$Freq/(b$Freq+c$Freq)
a1<-a[a$Sex=="F" & a$SexShare=="FALSE",];a2<-a[a$Sex=="M" & a$SexShare=="FALSE",]
c$Female_Fra<-c$Freq/(c$Freq+a1$Freq);c$Male_Fra<-c$Freq/(c$Freq+a2$Freq);
c$Female_Spe<-a1$Freq;c$Male_Spe<-a2$Freq;
Number<-c
ggplot(Number,aes(x=Fraction,y=Freq,col=Type))+geom_jitter(position=position_jitter(0.02))+theme_classic()+
  theme(axis.text.x = element_text(angle = 90, hjust = 1, size=8,vjust = 0.5),
        strip.text.y = element_text(size=6),
        plot.title = element_text(hjust = 0.5),
        legend.title = element_text(size = 8))

#RNA-seq: number of DE-subfamilies shared by female and male
data<-read.table("supplementary file 2. RNA_liver_adt_subfamily_treatment_deg_all_control_edgeR.txt",header = T)
data<-data[data$Class=="SINE"|data$Class=="LINE"|data$Class=="LTR"|data$Class=="DNA", ]
data$Significant <- ifelse(data$FDR < 0.001 & abs(data$logFC)>log2(1.75)
                           & (data$Control_CPM_median>=100 | data$Exposure_CPM_median>=100), "TRUE", "FALSE")
data$direction<-ifelse(data$logFC>0, "up","down")
da<-data[data$Significant==TRUE,];d<-da[,c("Subfamily","Type","Significant","Sex")]
f<-spread(d,Sex,Significant);f[is.na(f)]<-"FALSE";k<-f[f$F==TRUE & f$M==TRUE,]
d$group<-paste(d$Subfamily,d$Type,sep=",");k$group<-paste(k$Subfamily,k$Type,sep=",")
d$SexShare<-d$group %in% k$group;a<-as.data.frame(table(d[,c(2,4,6)]))
a$SexShare<-factor(a$SexShare,levels = c("TRUE","FALSE"))
b<-as.data.frame(a[a$SexShare=="FALSE",] %>% group_by(Type) %>% summarise(across(c(Freq), c(sum))))
b$SexShare<-"FALSE";c<-a[a$Sex=="F" & a$SexShare=="TRUE",]
colnames(b)<-c("type","Freq","SexShare");c$Fraction<-c$Freq/(b$Freq+c$Freq)
a1<-a[a$Sex=="F" & a$SexShare=="FALSE",];a2<-a[a$Sex=="M" & a$SexShare=="FALSE",]
c$Female_Fra<-c$Freq/(c$Freq+a1$Freq);c$Male_Fra<-c$Freq/(c$Freq+a2$Freq);
c$Female_Spe<-a1$Freq;c$Male_Spe<-a2$Freq;
Number<-c
ggplot(Number,aes(x=Fraction,y=Freq,col=Type))+geom_jitter(position=position_jitter(0.02))+theme_classic()+
  theme(axis.text.x = element_text(angle = 90, hjust = 1, size=8,vjust = 0.5),
        strip.text.y = element_text(size=6),
        plot.title = element_text(hjust = 0.5),
        legend.title = element_text(size = 8))

