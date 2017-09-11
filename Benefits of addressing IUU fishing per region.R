# Potential benefits of addressing IUU fishing per FAO region (Figure 4 of "Rapid and lasting gains from solving illegal fishing")
# Author: Reniel B. Cabral
# Date: 31 August 2017

#load the libraries needed for plotting. If these libraries are not yet installed in your computer, you can install them by
# using the code: install.packages(c("reshape2","ggrepel","gridExtra","grid"))
library(reshape2)
library(ggrepel)
library(gridExtra)
library(grid)

#Fig 4a. f is F/Fmsy value of the median fishery per region

#Read data file. Be sure to change the address to your local file address 
IUUregion <- read.csv("C:/Users/Ren/Documents/GitHub/Rapid and lasting gains from solving illegal fishing/IUUregion.csv")

#Adding a column to the data file (IUUregion) containing the f values per region resulting from addressing IUU fishing in the region
IUUregion$Change<-IUUregion$f*(100-IUUregion$IUU)/100

#Plotting Fig. 4a
data_melt <- melt(IUUregion, id.vars = c("Region","RegionNo","IUU","Production"))
IUUFAO<-ggplot(data_melt, aes(x=IUU, y=value)) +
  geom_segment(data=IUUregion,aes(x = IUU, xend = IUU, y = f, yend = Change), arrow = arrow(), colour = "gray", size = 1, alpha = 0.3) +
  geom_point(aes(shape=variable,size=Production),alpha = 0.5,color="blue") +
  scale_shape_manual(values=c(1,16),labels = c("Current", "Address IUU fishing")) +
  geom_text_repel(data = subset(data_melt, variable == c("f")), aes(label = Region), size = 5, box.padding = unit(0.35, "lines"), point.padding = unit(0.3, "lines")) +
  scale_x_continuous(limits = c(0, 40)) +
  scale_y_continuous(limits = c(0.4, 2.5)) +
  theme_minimal()
IUUreform<-IUUFAO+theme(axis.text.x=element_text(size=14,face="bold",color="black"),axis.text.y=element_text(size=14,face="bold",color="black"),axis.title.y=element_text(size=20), legend.title = element_text(size=14), legend.text = element_text(size=14),plot.title = element_text(size=32,face="bold"),axis.title.x=element_text(size=20))+labs(y = expression(F/F[MSY]),shape = "")+
  geom_hline(yintercept=1, linetype="dashed", color = "black") + theme(axis.title.x=element_blank(), axis.text.x=element_blank(), axis.ticks.x=element_blank()) +
  scale_size_area(max_size = 15,breaks = c(0.1e7, 1e7, 2e7),name="Harvest (MT)")+theme(legend.position=c(0.8,0.87))


#Fig 4b. f is F/Fmsy value of the catch-weighted average fishery per region
#Read data file. Be sure to change the address to your local file address 
IUUregionWT <- read.csv("C:/Users/Ren/Documents/GitHub/Rapid and lasting gains from solving illegal fishing/IUUregionWeighted.csv")

#Adding a column to the data file (IUUregion) containing the f values per region resulting from addressing IUU fishing in the region
IUUregionWT$Change<-IUUregionWT$f*(100-IUUregionWT$IUU)/100

#Plotting Fig. 4b
data_meltWT <- melt(IUUregionWT, id.vars = c("Region","RegionNo","IUU","Production"))
IUUFAOWT<-ggplot(data_meltWT, aes(x=IUU, y=value)) +  
  geom_segment(data=IUUregionWT,aes(x = IUU, xend = IUU, y = f, yend = Change), arrow = arrow(), colour = "gray", size = 1, alpha=0.3) +
  geom_point(aes(shape=variable,size=Production),alpha = 0.5,color="blue") +
  scale_shape_manual(values=c(1,16),labels = c("Current", "Address IUU fishing")) +
  geom_text_repel(data = subset(data_meltWT, variable == c("f")), aes(label = Region), size = 5, box.padding = unit(0.35, "lines"), point.padding = unit(0.3, "lines")) +
  scale_x_continuous(limits = c(0, 40)) +
  scale_y_continuous(limits = c(0.4, 2.5)) +
  theme_minimal()
IUUreformWT<-IUUFAOWT+theme(legend.position="none",axis.text.x=element_text(size=14,face="bold",color="black"),axis.text.y=element_text(size=14,face="bold",color="black"),axis.title.y=element_text(size=20), plot.title = element_text(size=32,face="bold"),axis.title.x=element_text(size=20))+labs(y = expression(F/F[MSY]),x="% of catch that is IUU",shape = "")+
  geom_hline(yintercept=1, linetype="dashed", color = "black")+
  scale_size_area(max_size = 15)

FONTSIZE<-28
IUUreform<- arrangeGrob(IUUreform, top = textGrob("a", x = unit(0, "npc"), y   = unit(1, "npc"), just=c("left","top"),gp=gpar(col="black", fontsize=FONTSIZE, fontfamily="Arial")))
IUUreformWT<- arrangeGrob(IUUreformWT, top = textGrob("b", x = unit(0, "npc"), y   = unit(1, "npc"), just=c("left","top"),gp=gpar(col="black", fontsize=FONTSIZE, fontfamily="Arial")))


PlotFig4<-grid.arrange(IUUreform,IUUreformWT, nrow = 2)
PlotFig4

#saving the plot
ggsave("C:/Users/Ren/Documents/GitHub/Rapid and lasting gains from solving illegal fishing/Fig4.tiff",PlotFig4, width = 7, height = 14, dpi = 300)