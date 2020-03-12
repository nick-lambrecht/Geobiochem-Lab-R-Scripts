#set wd
setwd("~/Box/Documents/Graduate_School/Swanner_Lab/Manuscripts/Heileges_Meer/Output_Files")
#Load libraries
library(ggplot2)
library(vegan)
library(dplyr)
library(scales)
library(grid)
library(reshape2)
library(phyloseq)
library(tidyverse)

#set plotting theme
theme_set(theme_bw())

# Assign variables for imported data
sharedfile = "stability.trim.contigs.good.unique.good.filter.unique.precluster.pick.pick.opti_mcc.shared"
taxfile = "stability.trim.contigs.good.unique.good.filter.unique.precluster.pick.pick.opti_mcc.0.03.cons.taxonomy"
mapfile = "HMmeta2.txt"

# Import mothur data
mothur_data <- import_mothur(mothur_shared_file = sharedfile, mothur_constaxonomy_file = taxfile)
mothur_data

# Import sample metadata
map <- read.delim(mapfile)
map
map$Depth_f <- factor(map$Depth, levels=c('0','1', '2', '3', '4', '5', '5.1', '5.8','6', '6.8','7', '8', '9', '9.5','10'))
map$Oxygen <- factor(map$Oxygen, levels=c('oxic', 'suboxic','anoxic'))
map <- sample_data(map)
map
rownames(map) <- map$Group
map

moth_merge <- merge_phyloseq(mothur_data, map)
moth_merge

#--------alpha diversity-------
MM <- prune_taxa(taxa_sums(moth_merge) > 0, moth_merge)
MM

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
ggsave("HM_AlphaDiversity_Shannon.pdf", plot=P2, height=5, width=6, units="in")    
#--------alpha diversity-------

#Make rarefaction curve
rarecurve(t(otu_table(moth_merge)), step=50, cex=0.5)
# rarefy without replacement
ps.rarefied = rarefy_even_depth(moth_merge, rngseed=1, sample.size=0.9*min(sample_sums(moth_merge)), replace=F)
ps.rarefied
rarecurve(t(otu_table(ps.rarefied)), step=50, cex=0.5)
#rarefied dataset
plot_bar(ps.rarefied, fill="Rank2")
#rarefied dataset by month
plot_bar(ps.rarefied, fill="Rank2") + facet_wrap(~Month, scales="free_x", nrow=1)
ps.phylum = tax_glom(ps.rarefied, taxrank="Rank2", NArm=FALSE)            
ps.phylum
plot_bar(ps.phylum, fill="Rank2") + facet_wrap(~Month, scales= "free_x", nrow=1)

# Make a data frame with a column for the read counts of each sample
sample_sum_df <- data.frame(sum = sample_sums(moth_merge))
sample_sum_df

# Histogram of sample read counts
ggplot(sample_sum_df, aes(x = sum)) + 
  geom_histogram(color = "black", fill = "indianred", binwidth = 2500) +
  ggtitle("Distribution of sample sequencing depth") + 
  xlab("Read counts") +
  theme(axis.title.y = element_blank())


# melt to long format (for ggploting) 
# prune out phyla below 2% in each sample

HM_phylum <- moth_merge %>%
  tax_glom(taxrank = "Phylum") %>%                     # agglomerate at phylum level
  transform_sample_counts(function(x) {x/sum(x)} ) %>% # Transform to rel. abundance
  psmelt() %>%                                         # Melt to long format
  filter(Abundance > 0.02) %>%                         # Filter out low abundance taxa
  arrange(Phylum)                                      # Sort data frame alphabetically by phylum

phylum_colors <- c(
  "#CBD588", "#5F7FC7", "pink","#DA5724", "#508578", "#CD9BCD",
  "#AD6F3B", "grey","#D14285", "#652926", "#C84248", 
  "#8569D5", "#5E738F","#D1A33D", "#8A7C64", "orange", "blue", "green", "red")

# Plot 
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

#------Pre-processing------
#Add column names
colnames(tax_table(moth_merge)) <- c("Kingdom", "Phylum", "Class", 
                                     "Order", "Family", "Genus")
cyano <- subset_taxa(moth_merge, Phylum="Cyanobacteria")
plot_heatmap(cyano, "Family")

ntaxa(moth_merge)
nsamples(moth_merge)
sample_names(moth_merge)
rank_names(moth_merge)
otu_table(moth_merge) [1:5, 1:5]


# Scale reads to even depth 
HM_scale <- rarefy_even_depth(moth_merge, sample.size = 6502)
HM_scale
HM_rarified <- data.frame(sum = sample_sums(HM_scale))
HM_rarified

# Fix month levels in sample_data
sample_data(HM_scale)$Month <- factor(sample_data(HM_scale)$Month, levels = c("May", "September"))

#--------Ordination-------
###PCoA
HM_pcoa <- ordinate(physeq = HM_scale, method = "PCoA", distance = "bray")
# Plot 
plot_ordination(
  physeq = HM_scale,
  ordination = HM_pcoa,
  color = "Month",
  shape = "Chla_100",
  title = "PCoA of Heileges Meer Microbial Communities") + 
  scale_color_manual(values = c("blue", "red", "orange")) +
  geom_point(aes(color = Month), alpha = 0.7, size = 4) +
  geom_point(colour = "grey90", size = 1.5) 

###NMDS
HM_nmds <- ordinate(physeq = HM_scale, method = "NMDS", distance = "bray")
# Plot 
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

