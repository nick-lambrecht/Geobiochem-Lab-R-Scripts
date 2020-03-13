# The goal of this script was to run a ONE-WAY ANOVA on an Incubation experiment
# Use 'Incubations_ANOVA' file for practice
# I used this helpful workflow as reference: http://www.sthda.com/english/wiki/one-way-anova-test-in-r#check-your-data
# Set working directory
library("dplyr")
library("devtools")
library("ggpubr")
library("rstatix")

# Assumptions underlying ONE-WAY ANOVA that need to be checked to ensure you can run the analysis:
  # Assumption of independence: sample means in each group are independent of one another (This is checked by study design)
  # Assumption of normality: samples were normalized to amount of water filtered
  # Assumption of homogeneity of variance: sample variances on the dependent variable are equal across groups

### Assumption of normality check: Density plot and Q-Q plot can be used to check normality visually.

# Density plot provides a judgment about whether the distribution is bell shaped, which is good
ggdensity(Incubations_ANOVA$Normalized_C_mg_mL, 
          main = "Density Plot",
          xlab = "Groups")
# Q-Q plot draws a correlation between a given sample and the normal distribution. 
# If all points fall approximately along the referenfce line, we can assume normality
ggqqplot(Incubations_ANOVA$Normalized_C_mg_mL)

# Check normality statistically
# shapiro.test() can be used to perform the Shapiro-Wilk test of normality for one variable (univariate):
# if p-value > 0.05, then the distribution of the data are not significantly different from normal distribution. 
# Therefore, we can assume normality
shapiro.test(Incubations_ANOVA$Normalized_C_mg_mL)


### Assumption of Homogeneity of variance check using Bartlett Test
# If p-value is greater than significance level (0.05), there is no evidence to suggest that the variance is significantly different for the 4 treatment groups. 
# The assumption of homogeneity of variance is met
bartlett.test(Normalized_C_mg_mL~Groups, Incubations_ANOVA)

# Visualize data via boxplot
attach(Incubations_ANOVA)
str(Incubations_ANOVA)

#Boxplot plain
boxplot(Normalized_C_mg_mL ~ Groups)
 
#Boxplot color
ggboxplot(Incubations_ANOVA, x = "Groups", y = "Normalized_C_mg_mL", 
          color = "Groups", palette = c("#00AFBB", "#E7B800", "#FC4E07", "lightgreen"),
          order = c("A", "B", "C", "D"),
          ylab = "POC (mg/mL)", xlab = "Treatment")

##### ANOVA ANALYSIS #####
#compute One-Way ANOVA
Inc.av <- aov(Normalized_C_mg_mL ~ Groups, data = Incubations_ANOVA)
summary(Inc.av)
## Interpretation: As the p-value (listed here as Pr(>F)) is less than the significance level 0.05, we can conclude that there are significant differences between at least two groups.
## We figure out which groups by running a Tukey's Post Hoc Test

### Tukeys Post-Hoc Test
# Since we know that some of the group means are signficiantly different, we want to know which groups are different --> post hoc test
TukeyHSD(Inc.av)
# OUTPUT: Tukey multiple comparisons of means: 95% family-wise confidence level
#      diff         lwr           upr          p adj
# B-A  0.0006353290 -9.851844e-05 0.0013691763 0.0917270
# C-A  0.0004790188 -2.548286e-04 0.0012128662 0.2346683
# D-A  0.0010307475  2.969001e-04 0.0017645949 0.0086648  *
# C-B -0.0001563101 -8.901575e-04 0.0005775373 0.9010509
# D-B  0.0003954186 -3.384288e-04 0.0011292660 0.3716315
# D-C  0.0005517287 -1.821187e-04 0.0012855761 0.1527791

# diff: difference between means of the two groups
# lwr, upr: the lower and the upper end point of the confidence interval at 95% (default)
# p adj: p-value after adjustment for the multiple comparisons.
### Significant differences between groups can be seen where p < 0.05

### Adding significance to boxplot
plot1 <- ggboxplot(Incubations_ANOVA, x = "Groups", y = "Normalized_C_mg_mL", 
                   color = "Groups", palette = c("#00AFBB", "#E7B800", "#FC4E07", "lightgreen"),
                   order = c("A", "B", "C", "D"),
                   ylab = "POC (mg/mL)", xlab = "Treatment")
plot1

# Add Anova significance: there is a sig. diff between at least 2 groups
plot2 <- plot1 + stat_compare_means(method = "anova", label.y = .0051)
plot2

#Perform pairwise comparisons between treatment groups to assess which ones are statistically different from one another
# Note: CAN NOT PERFORM t-test because that would greatly increase likelihood of Type 1 error..use output from TukeyHSD(Inc.av)
Inc.av2 <- aov(Normalized_C_mg_mL ~ Groups, data = Incubations_ANOVA) %>%
  tukey_hsd()
Inc.av2

# Visualize: Specify the comparisons you want
# Here is where you can add significant differences between groups on your graphs
# I went into Illustrator and just removed the 'n.s.' associations
plot3 <- plot2 + stat_pvalue_manual(Inc.av2, label = "p = {p.adj.signif}", y.position = c(.0043, .0043, .0050, .0043, .0043, .0043))
plot3
ggsave("Incubations_compared.pdf", plot=plot3, width=5, height=5, units="in" )

# Perform pairwise comparison against all (average of all samples)
# Here we are finding the mean of all samples over all treatment groups, and comparing the mean of each treatment against the overall mean.
# Not sure what this tells us but here's how to do it
compare_means(Normalized_C_mg_mL ~ Groups, data = Incubations_ANOVA, ref.group = ".all.", method = "t.test")

# Visualize: Specify the comparisons you want
# hide nonsignificant (ns) using hide.ns=TRUE inside the stat_compare_means
plot4 <- plot1 + stat_compare_means(label.y=.005, label = "p.signif", method = "t.test", ref.group = ".all.") + geom_hline(yintercept = mean(Incubations_ANOVA$Normalized_C_mg_mL), linetype=2)
plot4
