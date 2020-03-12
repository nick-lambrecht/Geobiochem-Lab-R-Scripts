#set working directory
setwd("~/Box/Documents/Graduate_School/Swanner_Lab/Manuscripts/Heileges_Meer/Output_Files")

#Load packages that are needed for analysis
library(ggplot2)
library(vegan)
library(dplyr)
library(scales)
library(grid)
library(reshape2)
library(phyloseq)
library(tidyverse)

#The following files can be found on the Github homepage. Download them and upload to R.
# Assign variables for imported data. These files are what will be used to make your phyloseq object and they are output files from Mothur.
sharedfile = "stability.trim.contigs.good.unique.good.filter.unique.precluster.pick.pick.opti_mcc.shared"
taxfile = "stability.trim.contigs.good.unique.good.filter.unique.precluster.pick.pick.opti_mcc.0.03.cons.taxonomy"
mapfile = "HMmeta2.txt"

# Import mothur data. This merges your shared file and taxonomy file together.
mothur_data <- import_mothur(mothur_shared_file = sharedfile, mothur_constaxonomy_file = taxfile)
mothur_data

# Import sample metadata. You will need a descriptive metadata file where there is a column called 'Group'. This column matches your sample names from your mothur output files.
# Include additional columns where you may want to group samples together. These can be continuous (numbers) or discreet (non-numeric) variables.
map <- read.delim(mapfile)
map
rownames(map) <- map$Group
map

# Here what I am doing is making Depth, which are my samples, go numerically. So when I plot, there won't be a random distribution of samples; it'll go from top of the water column to bottom
map$Depth_f <- factor(map$Depth, levels=c('0','1', '2', '3', '4', '5', '5.1', '5.8','6', '6.8','7', '8', '9', '9.5','10'))

# This is another little hack. What I want to do here is when I am plotting Oxygen, I want my legend to be in the order of the redox layers of the lake. Doing this will ensure that you get the order of the discreet variable the way you want it to show up in figures.
map$Oxygen <- factor(map$Oxygen, levels=c('oxic', 'suboxic','anoxic'))
map <- sample_data(map)
map

# Here we are merging your mothur files with your metadata files
moth_merge <- merge_phyloseq(mothur_data, map)
moth_merge

#--------alpha diversity-------
# Here I am pruning, or removing any OTU's that have a 0 abundance. In this class, no OTU's have zero abundance in the dataset.
MM <- prune_taxa(taxa_sums(moth_merge) > 0, moth_merge)
MM

# This function estimates a number of alpha-diversity metrics and returns a ggplot plotting object
# You must use untrimmed, non-normalized count data for meaningful results, as many of these estimates are highly dependent on the number of singletons. 
# In the 'measures()' you can add different types of metrics. See the R help for what kinds of Alpha diversity metrics can be used
# Depth_f is from my depth modification above, and shape command is making my values have different shapes by sampling month (which I denoted on my metadata file)
P = plot_richness(MM, x="Depth_f", shape="Month", measures=c("Shannon"))
P
P2 = P + geom_point(size=3, alpha=0.7) + labs(x = "Depth (m)") + theme(axis.title.x = element_text(size=15, face="bold"),
                                                                       panel.grid.minor = element_blank(),
                                                                       panel.border = element_rect(color = "black"),
                                                                       legend.title = element_text(size=12),
                                                                       legend.position = c(0.15, 0.85),
                                                                       legend.background = element_rect(color = "black", linetype = "solid"),
                                                                       legend.text = element_text(size=12),
                                                                       panel.grid.major = element_blank(),
                                                                       axis.title.y = element_text(size=15, face="bold"),
                                                                       strip.text.y = element_text(size=15, color = "black"),
                                                                       strip.text.x = element_text(size=15, color = "black"),
                                                                       axis.text.x = element_text(size=12, color = "black"),
                                                                       axis.text.x.bottom = element_text(angle=0, hjust=0.5),
                                                                       axis.text.y = element_text(size = 12, colour = "black"),
                                                                       axis.line = element_line(colour = "black"))
P2
# GGsave will save file to your working directory. You can modify file type, size, and units the size is in
ggsave("HM_AlphaDiversity_Shannon.pdf", plot=P2, height=5, width=6, units="in")    
#--------alpha diversity-------

#Make rarefaction curve. See what the unrarefied curve looks like
rarecurve(t(otu_table(moth_merge)), step=50, cex=0.5)

# rarefy without replacement. Here we are rarefying down to 90% of the sample with the lowest OTU.
ps.rarefied = rarefy_even_depth(moth_merge, rngseed=1, sample.size=0.9*min(sample_sums(moth_merge)), replace=F)
ps.rarefied

# Rarefied curve.
rarecurve(t(otu_table(ps.rarefied)), step=50, cex=0.5)

#rarefied dataset. Here we are plotting our dataset with the fill color corresponding to the second rank in our taxonomy. So Kingdom (1), Phylum (2)....
plot_bar(ps.rarefied, fill="Rank2")

#rarefied dataset by month. Facet wrapping by month, and the scales are free which means the x axis will vary between the two graphs.
plot_bar(ps.rarefied, fill="Rank2") + facet_wrap(~Month, scales="free_x", nrow=1)

# Tax_glom will merge species that have the same taxonomy at a certain taxaonomic rank. So, it will esentially make your graph more pretty.
ps.phylum = tax_glom(ps.rarefied, taxrank="Rank2", NArm=FALSE)            
ps.phylum
plot_bar(ps.phylum, fill="Rank2") + facet_wrap(~Month, scales= "free_x", nrow=1)

#Add column names. Here, instead of inputing Rank1, Rank2, ect., what we are doing are adding headers to those columns which correspond to the correct taxonomic level.
# Make sure that where you see moth_merge, you have your correct phyloseq object for downstream graph making and analysis.
colnames(tax_table(moth_merge)) <- c("Kingdom", "Phylum", "Class", 
                                     "Order", "Family", "Genus")

# The following code is taking your phyloseq object (moth_merge) and converting it to a new object (HM_phylum). What each part of the code does is highlighted by # to the right of the code line.
# melt to long format (for ggploting) 
# Prune out, or remove, phyla below 2% in each sample...this can be changed for whatever you want to show

HM_phylum <- moth_merge %>%
  tax_glom(taxrank = "Phylum") %>%                     # agglomerate at phylum level
  transform_sample_counts(function(x) {x/sum(x)} ) %>% # Transform to relative abundance
  psmelt() %>%                                         # Melt to long format
  filter(Abundance > 0.02) %>%                         # Filter out low abundance taxa
  arrange(Phylum)                                      # Sort data frame alphabetically by phylum


# Here I am making a custom color palette. You can do whatever color scheme(s) you would like!
phylum_colors <- c(
  "#CBD588", "#5F7FC7", "pink","#DA5724", "#508578", "#CD9BCD",
  "#AD6F3B", "grey","#D14285", "#652926", "#C84248", 
  "#8569D5", "#5E738F","#D1A33D", "#8A7C64", "orange", "blue", "green", "red")

# Plot your new phylose object
P1 = ggplot(HM_phylum, aes(x = Depth, y = Abundance, fill = Phylum)) + 
  facet_grid(Month~.) +
  geom_bar(stat = "identity", width=0.1, color="black") +
  scale_fill_manual(values = phylum_colors) +
  scale_x_continuous(labels = c("0.0", "", "5", "", "10"), name = "Depth (m)") +
  guides(fill = guide_legend(keywidth = 2, keyheight = 1)) +
  ylab("Relative Abundance (Phyla > 2%) \n") +
  ggtitle("Phylum Composition of Heileges Meer Over Two Sampling Campaigns") 
P1
ggsave("HM_phylum.pdf", plot=P1, width=10, height=5, units="in" )



#--------Ordination-------

# Scale reads to even depth (Rarefy). 
HM_scale <- rarefy_even_depth(moth_merge, sample.size = 6502)
HM_scale

# Fix month levels in sample_data. Doing the same thing here like the Oxygen variable above.
sample_data(HM_scale)$Month <- factor(sample_data(HM_scale)$Month, levels = c("May", "September"))

###PCoA - Principle Coordinate Analysis
# Currently supported method options are: c("DCA", "CCA", "RDA", "CAP", "DPCoA", "NMDS", "MDS", "PCoA")
HM_pcoa <- ordinate(physeq = HM_scale, method = "PCoA", distance = "bray")

# Plot ordination
plot_ordination(
  physeq = HM_scale,
  ordination = HM_pcoa,
  color = "Month",
  shape = "Chla_100",
  title = "PCoA of Heileges Meer Microbial Communities") + 
  scale_color_manual(values = c("blue", "red", "orange")) +
  geom_point(aes(color = Month), alpha = 0.7, size = 4) +
  geom_point(colour = "grey90", size = 1.5) 

###NMDS - Non-metric multidimensional scaling
HM_nmds <- ordinate(physeq = HM_scale, method = "NMDS", distance = "bray")

# Plot ordination
nmds = plot_ordination(
  physeq = HM_scale,
  ordination = HM_nmds,
  color = "Oxygen",
  shape = "Month") + 
  scale_color_manual(values = c("blue", "red", "orange")) +
  geom_point(aes(color = Oxygen), alpha = 0.7, size = 4) +
  geom_point(color = "white", size = 1.5) + theme_classic() + theme(axis.text.y.left = element_text(size=12, color = "black"), 
                                                                    axis.text.x.bottom = element_text(size=12, color = "black"),
                                                                    legend.text = element_text(size = 12),
                                                                    legend.title = element_text(size=12),
                                                                    legend.position = c(0.9,0.87),
                                                                    legend.box = "vertical",
                                                                    legend.background = element_rect(color = "black", linetype = "solid"),
                                                                    axis.title.x = element_text(size=15, face="bold"),
                                                                    axis.title.y = element_text(size=15, face="bold"))
nmds2 = nmds + guides(shape= FALSE)
ggsave("NMDS.pdf", plot=nmds2, width=6, height=5, units="in" )
#--------Ordination-------


###Statistics
# Calculate bray curtis distance matrix
HM_bray <- phyloseq::distance(HM_scale, method = "bray")
# make a data frame from the sample_data
sampledf <- data.frame(sample_data(moth_merge))
sampledf
# Adonis test
adonis(HM_bray ~ Chla_100, data = sampledf)
# Homogeneity of dispersion test
beta <- betadisper(HM_bray, sampledf$Chla_100)
permutest(beta)

