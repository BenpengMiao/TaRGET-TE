#distribution of all DA-TEs in genome
data<-read.table("DA-TE_distribution_Genome.txt",header = T)

ggplot(data, aes(x = V6, y = V28, col=log2(Ratio),size=Freq))+
  geom_point()+scale_size(range = c(0.1, 3))+geom_point(col="black",shape=1)+
  scale_color_gradient2(low="navy", mid="white", high="firebrick3" ) +
  xlab("TE class") +ylab("Genome position") + theme_classic()+
  theme(axis.text.x = element_text(angle = 90, hjust = 1, size=7,vjust = 0.5), 
        axis.text.y = element_text(size=7),
        legend.text = element_text(size=7),legend.title =element_text(size=7) )+
  facet_grid(~V19,scales = "free")

#distribution of DA-TEs in genome for different exposures
data<-read.table("DA-TE_distribution_Genome_different_exposure.txt",header = T)

ggplot(data, aes(x=V27, y=Freq, fill=V28))+scale_fill_brewer(palette = "Set2")+
  geom_bar(stat="identity",position = "fill")+theme_classic()+
  theme(axis.text.x = element_text(angle = 90, hjust = 1, size=7,vjust = 0.5))