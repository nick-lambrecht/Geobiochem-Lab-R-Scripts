#This script is to make facet grids using relative abundance data from 16S Sequencing
#The example file that can be used to practice this script is titled 'phototrophs'

#set wd
library(ggplot2)

#Here I set Date as a factor and enter in my 3 months. I'm doing this because R would make my facets in alphabetical order: July, May, September.
#But we want a logical order. Therefore, by making the Date_F factor I can designate the order of my facet grids
phototrophs$Date_F <- factor(phototrophs$Date, levels=c('May', 'July', 'September'))

#Run the ggplot code
#Remember, this is entirely customizable; you can add/subtract different elements depending on what you would like your figure to look like
Phototrophs<-ggplot(data=phototrophs, aes(y=Abundance, x=Depth, fill=Group)) + 
  geom_bar(stat="identity", position="stack", colour="black")+
  facet_grid(.~Date_F)+
  coord_flip()+
  scale_x_reverse(breaks=seq(0,21,1), name="Depth (m)")+
  theme_classic()+
  ylab("Relative Abundance (%)")+
  scale_fill_manual(values=c("green","violet","purple"))
Phototrophs

#Modify elements of the axis titles
Phototrophs2 <- Phototrophs + theme(axis.title.x = element_text(size=15, face="bold"),
                                    axis.title.y = element_text(size=15, face="bold"),
                                    strip.text.x = element_text(size=15, color = "black"),
                                    axis.text.x = element_text(size=12, color = "black"),
                                    axis.text.y = element_text(size = 12, colour = "black"),
                                    plot.caption = element_text(size = 10))

#Add a title 
P4 <- Phototrophs2 + ggtitle("Brownie Lake (2017)")
P4

#The ggsave function is great because you can customize the file type and size
#Keep in mind once you save your file, it will deposit into whatever working directory you specified
ggsave("CL_Phototrophs.pdf", plot=P4, height=6, width=5, units="in")

###########Individual facet grids of phototrophs
#Here I want to create facet grids of only one type of phototroph
#You can take the practice file, move the spread sheet with the corresponding phototroph to the front, save it, then re-read it into R

#Same step as before, except I was only wanting to look at May and July
phototrophs$Date_F <- factor(phototrophs$Date, levels=c('May', 'July'))


Phototrophs<-ggplot(data=phototrophs, aes(y=Abundance, x=Depth, fill=Group)) +
  geom_bar(stat="identity", position="stack", colour="black")+
  facet_grid(.~Date_F)+
  coord_flip()+
  scale_x_reverse(breaks=seq(0,21,1), name="Depth (m)")+
  theme_classic()+
  ylab("Relative Abundance (%)")+
  scale_fill_manual(values=c("purple"))
Phototrophs

#What I did here was move the corresponding sheet to the front of the excel file, run the code 3 times, and save the code with the corresponding phototroph classification
ggsave("BL_GSB.pdf", plot=Phototrophs, height=6, width=5, units="in")
ggsave("BL_PnSB.pdf", plot=Phototrophs, height=6, width=5, units="in")
ggsave("BL_PSB.pdf", plot=Phototrophs, height=6, width=5, units="in")
