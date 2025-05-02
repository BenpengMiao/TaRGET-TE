library(ggplot2)
library(ggrepel)

data<-read.table("supplementary file 8. subfamily_enrichment_of_DA-TEs.txt",header = T)

ggplot(data,aes(x=Freq, y=log2(Ratio), col = group, size=Freq))+ geom_point(alpha=0.8)+
  theme_classic()+ scale_size(range = c(0.01, 3))+facet_wrap(~sex,scales = "free_x")+
  theme(axis.text.x = element_text(angle = 90, hjust = 1,size=8,vjust = 0.5),
        axis.text.y = element_text(hjust = 1,size=7,vjust = 0.5))+
  geom_hline(yintercept=log2(2),linetype="dashed",size=0.5)+
  geom_vline(xintercept=10, linetype="dashed",size=0.5)+
  geom_text_repel(data = subset(data, data$Freq>=10 & data$Ratio>=2),aes(label = subfamily),size = 2)