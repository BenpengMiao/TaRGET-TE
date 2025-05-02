library(ggplot2)
library(ggrepel)


#Enrichment of LTR subfamily associated with immune and metabolic processes

data<-read.table("LTR_assocaited_with_immune_and_metabolic.txt",header = T)

ggplot(data, aes(x = Freq, y = log2(Ratio),color=Expo,shape=Sex))+geom_point()+
  geom_hline(yintercept=log2(2),size=0.5,linetype="dashed")+geom_vline(xintercept=10,size=0.5,linetype="dashed")+
  scale_shape_manual(values = c(0,19))+theme_classic()+
  xlab("Number of individul TEs") +ylab("log2Ratio") +facet_wrap(~Group)+
  geom_text_repel(data = subset(data, Var1=="ORR1E" | Var1=="MTD"),aes(label = Var1),size = 2.5)

#TF binding motif in ORR1Es
library(tidyr);library(dbplyr);library(stringr);library(dplyr)

motif<-read.table("LTR_ORR1E_immune_motif.txt");motf<-motif[motif$V8<0.05,]
mt<-as.data.frame(motf %>% group_by(V1,V2) %>% summarise(across(c(V8), c(min))))
m<-as.data.frame(mt %>% group_by(V1) %>% summarise(across(c(V8_1), c(length,min))))
list<-as.character(mt$V2);list<-list[!(duplicated(list))];m$Fraction<-m$V8_1_1/length(list)
write.table(m,"LTR_ORR1E_immune_motif_result.txt",sep="\t",quote=F,row.names = F)

ggplot(m, aes(x=Fraction, y=-log10(V8_1_2))) +geom_point()+theme_classic()+
  geom_text_repel(data = subset(m, Fraction>0.9),aes(label = V1),size = 2)+
  xlab("Fraction of TEs contain TF binding motif")+ylab("-log10(Q-value)")

#alignment of ORR1Es
library(pheatmap)

data<-read.table("alignment_of_ORR1E.txt");rownames(data)<-data$V1;data<-data[,2:ncol(data)];dat<-data[2:nrow(data),]
x<-pheatmap(dat,cluster_cols = F, cluster_rows = T, border_color = "white",color = c("white","pink","orange","cyan","purple"),
            legend_breaks = c(0,1,2,3,4),legend_labels = c("Gap","A","T","C","G"),show_rownames = F,show_colnames = F)
