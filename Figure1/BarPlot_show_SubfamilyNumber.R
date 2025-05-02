library(ggplot2)
#number of DA-subfamily
data<-read.table("Differential_accessible_subfamily_Number_in_5monthLiver.txt",header = T)
ggplot(data, aes(x=Class, y=Freq, fill=direction))+
  geom_bar(stat="identity")+geom_hline(yintercept=0,size=0.5)+
  scale_fill_brewer(palette = "Set2")+theme_classic()+
  theme(axis.text.x = element_text(angle = 90, hjust = 1, size=7,vjust = 0.5),
        legend.text = element_text(size=7),legend.title =element_text(size=7))+
  facet_grid(Sex~Type)+theme(strip.text = element_text(size=7))
#number of DE-subfamily
data<-read.table("Differential_expressed_subfamily_Number_in_5monthLiver.txt",header = T)
ggplot(data, aes(x=Class, y=Freq, fill=direction))+
  geom_bar(stat="identity")+geom_hline(yintercept=0,size=0.5)+
  scale_fill_brewer(palette = "Set2")+theme_classic()+
  theme(axis.text.x = element_text(angle = 90, hjust = 1, size=7,vjust = 0.5),
        legend.text = element_text(size=7),legend.title =element_text(size=7))+
  facet_grid(Sex~Type)+theme(strip.text = element_text(size=7))