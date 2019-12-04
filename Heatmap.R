#I followed this tutorial: https://sebastianraschka.com/Articles/heatmaps_in_r.html
#The goal of this code was to create a heatmap to show the distribution of various Fe gene (and Fe gene families) across many metagenomic bins
#Documentation for heatmap.2() can be found here: https://www.rdocumentation.org/packages/gplots/versions/3.0.1.1/topics/heatmap.2
library(gplots)
library(RColorBrewer)
#Set wd
#Read data in; my file was a .csv
#Use my data set as practice. Practice file is located in the repository for download
#The practice dataset was created by running FeGenie via conda, a Python package
FeGenie <- read.csv("../fegenie_output/FeGenie-heatmap-data.csv")

#Store rownames as an object
rnames <- FeGenie[,1]

#convert data frame into a matrix
FeG_data <- data.matrix(FeGenie[,2:ncol(FeGenie)])

#add row names back in
rownames(FeG_data) <- rnames

#making custom color palette
my_palette <- colorRampPalette(c("lightgray", "yellow", "red"))(n = 299)
# (optional) defines the color breaks manually for a "skewed" color transition
col_breaks = c(seq(0,0.09,length=100),  # for lightgray
               seq(0.1,0.5,length=100),           # for yellow
               seq(0.51,1.20,length=100))             # for red

#Saving heatmap as a PDF file
# MUST finish the code by entering the dev.off() command at the bottom. Otherwise the pdf won't publish to your working directory
pdf("../fegenie_output/heatmap.pdf",width = 10, height = 10, pointsize = 10)

heatmap.2(FeG_data,
          cellnote = FeG_data, #cellnote adds values in the boxes
          main = "Fe Gene Families",
          notecol = "black",
          density.info="none",
          trace="none",
          margins =c(12,9),
          col=my_palette,
          breaks=col_breaks,
          dendrogram = "none",
          srtCol = 45,
          Colv="NA",
          Rowv = "NULL")

dev.off()
          
    
