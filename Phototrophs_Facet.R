library(ggplot2)
#GSB, PSB, PnSB
phototrophs$Date_F <- factor(phototrophs$Date, levels=c('May', 'July', 'September'))
Phototrophs<-ggplot(data=phototrophs, aes(y=Abundance, x=Depth, fill=Group)) +
  geom_bar(stat="identity", position="stack", colour="black")+
  facet_grid(.~Date_F)+
  coord_flip()+
  scale_x_reverse(breaks=seq(0,21,1), name="Depth (m)")+
  theme_classic()+
  ylab("Relative Abundance (%)")+
  scale_fill_manual(values=c("green","violet","purple"))
Phototrophs

Phototrophs2 <- Phototrophs + theme(axis.title.x = element_text(size=15, face="bold"),
                                    axis.title.y = element_text(size=15, face="bold"),
                                    strip.text.x = element_text(size=15, color = "black"),
                                    axis.text.x = element_text(size=12, color = "black"),
                                    axis.text.y = element_text(size = 12, colour = "black"),
                                    plot.caption = element_text(size = 10))
P4 <- Phototrophs2 + ggtitle("CANYON LAKE (2017)")
P4
ggsave("CL_Phototrophs.pdf", plot=P4, height=6, width=5, units="in")

###
#Cyanobacteria
phototrophs$Date_F <- factor(phototrophs$Date, levels=c('May', 'July', 'September'))
Phototrophs<-ggplot(data=phototrophs, aes(y=Abundance, x=Depth, fill=Group)) +
  geom_bar(stat="identity", position="stack", colour="black")+
  facet_grid(.~Date_F)+
  coord_flip()+
  scale_x_reverse(breaks=seq(0,14,1), name="Depth (m)")+
  theme_classic()+
  ylab("Relative Abundance (%)")+
  scale_fill_manual(values=c("lightgrey"))
Phototrophs

Phototrophs2 <- Phototrophs + theme(axis.title.x = element_text(size=15, face="bold"),
                                    axis.title.y = element_text(size=15, face="bold"),
                                    strip.text.x = element_text(size=15, color = "black"),
                                    axis.text.x = element_text(size=12, color = "black"),
                                    axis.text.y = element_text(size = 12, colour = "black"),
                                    plot.caption = element_text(size = 10))
Phototrophs2
P3 <- Phototrophs2 + theme(legend.position = "bottom", legend.box = "horizontal", 
                           legend.text = element_text(size=15),
                           legend.title = element_text(size=15, face="bold", color = "black"))
P4 <- P3 + labs(fill= "Group:")
P4
ggsave("cyanobacteria.pdf", plot=P4, height=5, width=7, units="in")

###Exobio proposal
Phototrophs<-ggplot(data=exobio_proposal, aes(y=Abundance, x=Depth, fill=Group)) +
  geom_bar(stat="identity", position="stack", colour="black")+
  facet_grid(.~Date)+
  coord_flip()+
  scale_x_reverse(breaks=seq(0,25,1), name="Depth (m)")+
  theme_classic()+
  ylab("Relative Abundance (%)")+
  scale_fill_manual(values=c("green","grey"))
Phototrophs

Phototrophs2 <- Phototrophs + theme(axis.title.x = element_text(size=15, face="bold"),
                                    axis.title.y = element_text(size=15, face="bold"),
                                    strip.text.x = element_text(size=15, color = "black"),
                                    axis.text.x = element_text(size=12, color = "black"),
                                    axis.text.y = element_text(size = 12, colour = "black"),
                                    plot.caption = element_text(size = 10))
P4 <- Phototrophs2 + ggtitle("CANYON LAKE (2017)")
P4
ggsave("CL_n2fixation.pdf", plot=P4, height=5, width=7, units="in")

### Individual phototrophs
phototrophs$Date_F <- factor(phototrophs$Date, levels=c('May', 'July'))
Phototrophs<-ggplot(data=phototrophs, aes(y=Abundance, x=Depth, fill=Group)) +
  geom_bar(stat="identity", position="stack", colour="black")+
  facet_grid(.~Date_F)+
  coord_flip()+
  scale_x_reverse(breaks=seq(0,21,1), name="Depth (m)")+
  theme_classic()+
  ylab("Relative Abundance (%)")+
  scale_fill_manual(values=c("purple","violet","purple"))
Phototrophs
ggsave("BL_GSB.pdf", plot=Phototrophs, height=6, width=5, units="in")
ggsave("BL_PnSB.pdf", plot=Phototrophs, height=6, width=5, units="in")
ggsave("BL_PSB.pdf", plot=Phototrophs, height=6, width=5, units="in")
