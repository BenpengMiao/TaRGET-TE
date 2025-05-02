library(ggplot2)

###distribution of DE-TE-transcript across different exposures
library(UpSetR)
data<-read.table("supplementary file 14. Expression_of_TE-known-transcript.txt",header = T)
dat<-data[data$DEG=="TRUE",];da<-dat[,c(10,11,13)];da<-da[!(duplicated(da)),]

#female
d<-da[da$Sex=="F",c(1,3)];d$Value=1
f<-spread(d,Exposure,Value);f[is.na(f)]<-0;rownames(f)<-f$Transcript;f<-f[,2:ncol(f)]
upset(f,nsets = 9,nintersects = 1000,point.size = 0.5,line.size = 0.2)

#male
d<-da[da$Sex=="M",c(1,3)];d$Value=1
f<-spread(d,Exposure,Value);f[is.na(f)]<-0;rownames(f)<-f$Transcript;f<-f[,2:ncol(f)]
upset(f,nsets = 9,nintersects = 1000,point.size = 0.5,line.size = 0.2)


###Number of DE-TE-transcripts shared between female and male
data<-read.table("SexShare.txt",header = T)

ggplot(data, aes(x = Total, y = ShareFrac,col=Exp))+
  geom_point(size=2)+theme_classic()+
  xlab("Number of DE-TE-transcripts in different expoures")+
  ylab("Fraction of female and male shared transcripts")+
  theme(axis.text.x = element_text(size=8,angle = 90, hjust = 1,vjust = 0.5),
        axis.text.y = element_text(size=8))


###TE class fraction of TEs derived DE-TE-transcripts
data<-read.table("Number_TE_class_Exposure_sex.txt",header = T)

ggplot(data,aes(x=V10,y=Freq,fill=V22))+geom_bar(stat="identity",position="fill")+
  theme_classic()+scale_fill_brewer(palette = "Set2") + facet_grid(~V11)+
  theme(axis.text.x = element_text(angle = 90, hjust = 1, size=8,vjust = 0.5),
        strip.text.y = element_text(size=6),plot.title = element_text(hjust = 0.5),
        legend.title = element_text(size = 8))



###subfamily enrichment in different transcript type
data<-read.table("Number_TE_subfamily_transcript.txt",header = T)
data<-data[data$V24=="processed_transcript"|data$V24=="protein_coding"|
       data$V24=="retained_intron"|data$V24=="lincRNA",]

ggplot(data, aes(x = Freq, y = log2(Ratio),col=V24,shape=Class,size=Freq))+
  geom_point()+theme_classic()+scale_size(range = c(1, 3))+
  scale_shape_manual(values = c(8,1,0,2))+
  xlab("Number of TEs in subfamily")+
  ylab("log2(enrichment of subfamily)")+
  theme(axis.text.x = element_text(size=8,angle = 90, hjust = 1,vjust = 0.5),
        axis.text.y = element_text(size=8))


