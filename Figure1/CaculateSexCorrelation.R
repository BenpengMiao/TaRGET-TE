#RNA-seq
data<-read.table("supplementary file 2. RNA_liver_adt_subfamily_treatment_deg_all_control_edgeR.txt",header = T)
data<-data[data$Class=="SINE"|data$Class=="LINE"|data$Class=="LTR"|data$Class=="DNA", ]
b<-as.character(data$Type);t<-b[!(duplicated(b))];
data1<-data[data$Sex=="F",];data2<-data[data$Sex=="M",];
cor<-c();pvalue<-c();for(i in 1:length(t)){
  dat1<-data1[data1$Type==t[i],];dat2<-data2[data2$Type==t[i],];
  da1<-dat1[,c(1,5,9)];da2<-dat2[,c(1,5,9)];
  rownames(da1)<-as.character(da1$TE);rownames(da2)<-as.character(da2$TE);
  l1<-as.character(da1$TE);l2<-as.character(da2$TE);l<-l1[l1 %in% l2];d1<-da1[l,];d2<-da2[l,];
  cor[i]<-cor(as.numeric(d1$logFC),as.numeric(d2$logFC),method="kendall")
  pvalue[i]<-cor.test(as.numeric(d1$logFC),as.numeric(d2$logFC))$p.value
}
fdr<-p.adjust(pvalue,method = "fdr");cor<-as.data.frame(cor);cor$type<-t;cor$FDR<-fdr

ggplot(cor, aes(x = type,y = cor,size=-log10(fdr),col=cor))+
  geom_point(alpha=0.7) + scale_size(range = c(0.01, 6))+
  theme_classic()+ylab("correlation between female and male")+
  scale_color_gradient2(low="blue",mid="white", high="red" )+
  geom_point(col="black",shape=1,alpha=0.7)+
  theme(axis.text.x = element_text(angle = 90, hjust = 1, size=7,vjust = 0.5), 
        axis.text.y = element_text(size=7),
        legend.text = element_text(size=7),legend.title =element_text(size=7) )

#ATAC-seq
data<-read.table("supplementary file 3. ATAC_liver_adt_subfamily_treatment_deg_all_control_edgeR.txt",header = T)
data<-data[data$Class=="SINE"|data$Class=="LINE"|data$Class=="LTR"|data$Class=="DNA", ]
b<-as.character(data$Type);t<-b[!(duplicated(b))];
data1<-data[data$Sex=="F",];data2<-data[data$Sex=="M",];
cor<-c();pvalue<-c();for(i in 1:length(t)){
  dat1<-data1[data1$Type==t[i],];dat2<-data2[data2$Type==t[i],];
  da1<-dat1[,c(1,5,9)];da2<-dat2[,c(1,5,9)];
  rownames(da1)<-as.character(da1$TE);rownames(da2)<-as.character(da2$TE);
  l1<-as.character(da1$TE);l2<-as.character(da2$TE);l<-l1[l1 %in% l2];d1<-da1[l,];d2<-da2[l,];
  cor[i]<-cor(as.numeric(d1$logFC),as.numeric(d2$logFC),method="kendall")
  pvalue[i]<-cor.test(as.numeric(d1$logFC),as.numeric(d2$logFC))$p.value
}
fdr<-p.adjust(pvalue,method = "fdr");cor<-as.data.frame(cor);cor$type<-t;cor$FDR<-fdr

ggplot(cor, aes(x = type,y = cor,size=-log10(fdr),col=cor))+
  geom_point(alpha=0.7) + scale_size(range = c(0.01, 6))+
  theme_classic()+ylab("correlation between female and male")+
  scale_color_gradient2(low="blue",mid="white", high="red" )+
  geom_point(col="black",shape=1,alpha=0.7)+ylim(-1,1)+
  theme(axis.text.x = element_text(angle = 90, hjust = 1, size=7,vjust = 0.5), 
        axis.text.y = element_text(size=7),
        legend.text = element_text(size=7),legend.title =element_text(size=7) )

