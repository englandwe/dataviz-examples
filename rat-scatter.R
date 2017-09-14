library(readr)
library(ggplot2)
library(ggthemes)
library(RColorBrewer)

ckd_beta <- read_csv("ckd_beta.txt")

#not a bad plot
label_colors <- brewer.pal(n = 4,name = "Paired")[c(2,4)]
ggplot(ckd_beta) +
  geom_point(aes(MDS1,MDS2,color=DiseaseState),size=3) +
  theme_bw() + 
  annotate("text",label="Stress=0.13",x=Inf,y=Inf,hjust=1.1,vjust=1.5) +
  scale_color_manual(values = label_colors) +
  labs(x="NMDS1",y="NMDS2", color="Disease State") +
  theme(panel.border = element_rect(color="gray75"))

#a better plot
label_colors <- brewer.pal(n = 4,name = "Paired")[c(2,4)]
ggplot(ckd_beta) +
  geom_point(aes(MDS1,MDS2,color=DiseaseState),size=3) +
  theme_bw() + 
  annotate("text",label="Stress=0.13",x=Inf,y=Inf,hjust=1.1,vjust=1.5) +
  annotate("text",label="CKD",color=label_colors[1],x=0.07, y=0.12,size=4,fontface=2) +
  annotate("text",label="Normal",color=label_colors[2],x=-0.05, y=0,size=4,fontface=2) +
  scale_color_manual(values = label_colors,guide=FALSE) +
  labs(x="NMDS1",y="NMDS2") +
  theme(panel.border = element_rect(color="gray75"))