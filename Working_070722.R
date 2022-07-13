#Morton root traits analyses D01
#Note by Young Oh
#070722
#lesson01: Cmd+Enter = Run

#echo = Should the code be shown in the final document? (TRUE, FALSE)
#eval = Should the results be shown in the final document? (TRUE, FALSE)
#e.g.{r echo=TRUE, eval=FALSE} 
#Things to know before analyzing
# Get mean(mean(name), standard deviation (sd(name)), and number of observations (length(name)):
# Sums of rows and columns: rowSums(name), colSums(name)
# Transpose a matrix: flipping a matrix so that the rows become the columns and vice versa = t(name):

# Clean workspace
rm(list=ls())
#This package is great for documents that you will be sharing – 
#it means that your collaborators will not have to spend time finding any packages they don’t have.
install.packages("pacman")
install.packages("mvtnorm")
install.packages("survival")
install.packages("TH.data")
install.packages("MASS")
install.packages("magicaxis")
install.packages("ggpmisc")
install.packages("plotrix")
install.packages("tidyverse")
install.packages("hrbrthemes")
install.packages("viridis")
install.packages("viridisLite")
install.packages("ggeffects")

# Libraries
library(plotrix)
library(ggpmisc)
library(ggplot2)
library(ggpubr)
library(magicaxis)
library(car)
library(emmeans)
library(pacman)#This package is great for documents that you will be sharing – 
#it means that your collaborators will not have to spend time finding any packages they don’t have.
library(tidyverse)
library(hrbrthemes)
library(viridis)
library(ggeffects)
# Set working directory to my workspace: can be varied depending users
setwd("/Users/young/Desktop/Git_Phillips_Lab/PhillipsLab/Morton_analyses")
# What is the current working directory?: getwd()

#Read data
rtrait <- read.csv("Exudation_cleansed_exported_070722.csv")


#The Question we are interested in 
# To what extent does root exudation rate vary among species? [ANOVA, though we're limited by study design - the 4-6 individual trees of each species are all located in the same plot, which isn't ideal for test assumptions.]
# 2) Does root exudation rate vary with plant or root traits? Which variables best predict root exudation rates? [ordination/PERMANOVA; mixed linear models that are similar to ANOVA and regressions but take plot-level replicates into account]
# 3) What are the relationships between root exudation rates (and other plant and root traits) and soil properties (rhizosphere effects in N cycling; aggregate stability)? [ordination/PERMANOVA; mixed linear models that are similar to ANOVA and regressions but take plot-level replicates into account]:

# To what extent does root exudation rate vary among species? [ANOVA, though we're limited by study design - the 4-6 individual trees of each species are all located in the same plot, which isn't ideal for test assumptions.]
# 1) Statistics analyses using ANOVA
# 2) Summary Table 
# 3) Important Figures

# 1) To answer the root trait variations among species we set out to use ANOVA to compare the variation between the group means to the variation within the groups (F-stat.). Here, we are using lm for ANOVA since lm is easier to be generalized when there is different n. (ANOVA is good when we have continuous response variable (measured traits) per a factor variable (species, myco association, etc) that defines the groups). 

# Take a subset and drop empty levels with droplevels.
#rtriatsub <- droplevels(subset(rtrait, Species %in% c("Exudation_rate","Diam","SRL","SRA","RTD","Mass")))

# Perform an ANOVA on a fitted model with three factors, giving F-statistics

#Diff in exudation rate among Species 
fit1 <- lm(Exudation_rate ~ Species, data=rtrait)
summary(fit1)
#->not sig except larch.

#Diff in exudation rate across MF types
fit2 <- lm(Exudation_rate ~ Myco, data=rtrait)
summary(fit2)
#->sig

#Diff in exudation rate across phylo types 
fit3 <- lm(Exudation_rate ~ Phylo, data=rtrait)
summary(fit3)
#->sig

#Diff in exudation rate across leaf types 
fit4 <- lm(Exudation_rate ~ Leaf, data=rtrait)
summary(fit4)
#->sig

#Comparing nested models by performing a likelihood ratio test
anova(fit1,fit2)
anova(fit1,fit3)
anova(fit1,fit4)
#The results shows all models are sigficantly different

# Perform an ANOVA on a fitted model, giving F-statistics
library(car)
anova(fit1)#among species; sig
anova(fit2)#between myco types; sig
anova(fit3)#angiosperm vs gymnosperm ; not sig
anova(fit4)#ev vs. dec;not sig


#to see overall species effect on exudation rates, multiple comparison
#First, fit the linear model again (a one-way ANOVA,
# because species is a factor)
lmSpec <- lm(Exudation_rate ~ Species, data=rtrait)
library(emmeans)
tukey_Spec <- emmeans(lmSpec, 'Species')
pairs(tukey_Spec)
#Only pairs with Larch turn out to be significant

# Perform an ANOVA on a fitted model, giving F-statistics
library(car)
anova(fit2)

#examining interaction factor in MF effect
fit_mf <- lm(Exudation_rate ~ Myco + Species + Myco:Species, data=rtrait)
Anova(fit_mf)
#The table shows the interaction is significant. Thus, we examine the significance by visualizing the interaction
with(rtrait, interaction.plot(Myco, Species, Exudation_rate))
#failed to plot properly; needs more investigation

# Estimate marginal means associated with 'MF types' for each different species.
# This specified contrasts using a character string in the one-way ANOVA example.
# The below procedure won't be necessary for this analysis
library(emmeans)
fit_multi <- emmeans(fit_mf, ~ Species | Myco, nesting = NULL)
pairs(fit_multi)
# Only the interaction with larch turned out to be significant; but need more investigation

#with ggplot2 we can visualize the significance
library(ggpubr)
#Boxplot for exudation rate among groups: mycorrhizal types
ggboxplot(rtrait, x = "Myco", y = "Exudation_rate", color = "Myco", palette = "jco") +
  stat_compare_means(method = "anova") + geom_jitter(color="black", size=0.4, alpha=0.9) +
  ggtitle("Boxplot for exudation rate among groups") +
  xlab("Myco. Type") + ylab(expression(Exudation~rate~ ~(mgC~g^-1~day^-1)))

#added dots using jitter function
ggboxplot(rtrait, x = "Myco", y = "Exudation_rate", color = "Myco", palette = "jco")+
  stat_compare_means(method = "anova")

#Example for plot with dots using jitter

#Boxplot for exudation rate among groups: Myco types
AMvsECM <- rtrait %>% ggplot( aes(x = Myco, y = Exudation_rate, fill=Myco)) + geom_boxplot() +
  scale_fill_viridis(discrete = TRUE, alpha=0.6) +
  geom_jitter(color="black", size=0.4, alpha=0.9) +
  theme_ipsum() +
  theme(
    legend.position="none",
    plot.title = element_text(size=11)
  ) +
  ggtitle("Boxplot for exudation rate among groups: Myco Type") +
  xlab("Myco. Type") + ylab(expression(log10~Exudation~rate~ ~(mgC~g^-1~day^-1))) + stat_compare_means(method = "anova")
AMvsECM + scale_y_continuous(trans='log10')
#Statistically different

#Boxplot for exudation rate among groups: Phylo types
Angio_vs_Gym <- rtrait %>% ggplot( aes(x = Phylo, y = Exudation_rate, fill=Phylo)) + geom_boxplot() +
  scale_fill_viridis(discrete = TRUE, alpha=0.6) +
  geom_jitter(color="black", size=0.4, alpha=0.9) +
  theme_ipsum() +
  theme(
    legend.position="none",
    plot.title = element_text(size=11)
  ) +
  ggtitle("Boxplot for exudation rate among groups: Phylo Type") +
  xlab("Phylo Type") + ylab(expression(log10~Exudation~rate~ ~(mgC~g^-1~day^-1))) + stat_compare_means(method = "anova")
Angio_vs_Gym + scale_y_continuous(trans='log10')
#Not statistically different, but we can still observe meaningful differences

#Boxplot for exudation rate among groups: Leaf types
dec_vs_ev <- rtrait %>% ggplot( aes(x = Leaf, y = Exudation_rate, fill=Leaf)) + geom_boxplot() +
  scale_fill_viridis(discrete = TRUE, alpha=0.6) +
  geom_jitter(color="black", size=0.4, alpha=0.9) +
  theme_ipsum() +
  theme(
    legend.position="none",
    plot.title = element_text(size=11)
  ) +
  ggtitle("Boxplot for exudation rate among groups: Phylo Type") +
  xlab("Leaf Type") + ylab(expression(log10~Exudation~rate~ ~(mgC~g^-1~day^-1))) + stat_compare_means(method = "anova")
dec_vs_ev + scale_y_continuous(trans='log10')
#Not statistically different, 

#more simple ways
#Boxplot for exudation rate among groups: Phylo. types
Angio_vs_Gym <- ggboxplot(rtrait, x = "Phylo", y = "Exudation_rate", color = "Phylo", palette = "jco") +
  stat_compare_means(method = "anova") + geom_jitter(color="black", size=0.4, alpha=0.9) +
  ggtitle("Boxplot for exudation rate among groups") +
  xlab("Phylo. Type") + ylab(expression(Exudation~rate~ ~(mgC~g^-1~day^-1)))

#Boxplot for exudation rate among groups: Leaf. types
ggboxplot(rtrait, x = "Leaf", y = "Exudation_rate", color = "Leaf", palette = "jco") +
  stat_compare_means(method = "anova") + geom_jitter(color="black", size=0.4, alpha=0.9) +
  ggtitle("Boxplot for exudation rate among groups") +
  xlab("Leaf. Type") + ylab(expression(Exudation~rate~ ~(mgC~g^-1~day^-1)))

#Boxplot that is groupin species into specific groups: myco type
p1 <- ggplot(rtrait, aes(x=Species, y=Exudation_rate, fill=Myco)) + 
  geom_boxplot() +
  facet_wrap(~Myco) + geom_jitter(color="black", size=0.4, alpha=0.9) + stat_compare_means(method = "anova")+
  xlab("Species with MF Type") + ylab(expression(log10~Exudation~rate~ ~(mgC~g^-1~day^-1)))+
  ggtitle("Boxplot for exudation rate among groups: Myco. Type")

p1 + facet_wrap(~Myco) + scale_y_continuous(trans='log10')
# Note: Among AM tree species exudation rates are not significantly different while EcM species are different; interesting. 
#But AM and EcM as group are different significantly 

#Boxplot that is groupin species into specific groups: Phylo type
p2 <- ggplot(rtrait, aes(x=Species, y=Exudation_rate, fill=Phylo)) + 
  geom_boxplot() +
  facet_wrap(~Phylo) + geom_jitter(color="black", size=0.7, alpha=0.9) + stat_compare_means(method = "anova") +
  xlab("Species with Phylo Type") + ylab(expression(log10~Exudation~rate~ ~(mgC~g^-1~day^-1)))+
  ggtitle("Boxplot for exudation rate among groups: Phylo. Type")

p2 + facet_wrap(~Phylo) + scale_y_continuous(trans='log10')
# Note: in gymnosperm exudation rates are significantly different while angiosperms are not

#Boxplot that is groupin species into specific groups: Leaf type
p3 <- ggplot(rtrait, aes(x=Species, y=Exudation_rate, fill=Leaf)) + 
  geom_boxplot() +
  facet_wrap(~Leaf) + geom_jitter(color="black", size=0.7, alpha=0.9) + stat_compare_means(method = "anova") +
  xlab("Species with Phylo Type") + ylab(expression(log10~Exudation~rate~ ~(mgC~g^-1~day^-1)))+
  ggtitle("Boxplot for exudation rate among groups: Leaf. Type")

p3 + facet_wrap(~Leaf) + scale_y_continuous(trans='log10')
# Note: Leaf type shows within each leaf type group all the species demonstrate statistical difference, 
#while dec vs. ev as group demonstrate no stat. significance.

# To answer the correlation and best predictor in the following codes we will show important correlations among roots traits with simple multiple linear regression models
#Questions
#2) Does root exudation rate vary with plant or root traits? Which variables best predict root exudation rates? 
#[ordination/PERMANOVA; mixed linear models that are similar to ANOVA and regressions 
#but take plot-level replicates into account]
#3) What are the relationships between root exudation rates (and other plant and root traits) and soil properties (rhizosphere effects in N cycling; aggregate stability)? [ordination/PERMANOVA; mixed linear models that are similar to ANOVA and regressions but take plot-level replicates into account]:
#Regression
#in the following codes we will show important correlations among roots traits with simple multiple linear regression models
#I am here to follow the instruction from the site, https://ourcodingclub.github.io/tutorials/mixed-models/

library(tidyverse)
#Fit the model with exudation rate as the response and Surf_area as the predictor and have a look at the output:
prelim_plot <- ggplot(rtrait, aes(x = Surf_area, y = Exudation_rate)) +
geom_point() +
geom_smooth(method = "lm") +  stat_regline_equation(label.y = 400, aes(label = ..eq.label..)) +
  stat_regline_equation(label.y = 350, aes(label = ..rr.label..))
prelim_plot
#appeared to have some correlation here.
model_ER_SA <- lm(Exudation_rate ~ Surf_area, data=rtrait)
residualPlot(model_ER_SA)
qqPlot(model_ER_SA)

#Now separating AM vs EcM
plt <- ggplot(rtrait,aes(x = Surf_area, y = Exudation_rate, color=Myco, shape=Myco)) + 
  geom_point(size=3) + 
  geom_smooth(method = "lm", aes(fill=Myco)) +
  stat_regline_equation(label.y = 400, aes(label = ..eq.label..)) +
  stat_regline_equation(label.y = 350, aes(label = ..rr.label..))

plt + facet_wrap(~Myco)

#trying out making separate data with repect to myco type
#generating the dataset with AM only
rtrait_AM <- rtrait[rtrait$Myco %in% c("AM"),]
#generating the dataset with EcM only
rtrait_EcM <- rtrait[rtrait$Myco %in% c("EM"),]

#making linear fitting models to test stat. significance
model_ER_SA_AM <- lm(Exudation_rate ~ Surf_area, data=rtrait_AM)
summary(model_ER_SA_AM)
residualPlot(model_ER_SA_AM)
qqPlot(model_ER_SA_AM)

model_ER_SA_EM <- lm(Exudation_rate ~ Surf_area, data=rtrait_EcM)
summary(model_ER_SA_EM)
residualPlot(model_ER_SA_EM)
qqPlot(model_ER_SA_EM)

#It turns out AM species have no stat significance between Surf_area and exudation rate (p=0.7294), while EcM species do have 
#significant correlation (p=.002415).

split_plot_by_species_EM <- ggplot(aes(Surf_area, Exudation_rate), data = rtrait_EcM) + 
  geom_point() + 
  facet_wrap(~ Species) + # create a facet for each mountain range
  xlab("Surf_area") + 
  ylab("Exudation rate")
split_plot_by_species_EM

split_plot_by_species_AM <- ggplot(aes(Surf_area, Exudation_rate), data = rtrait_AM) + 
  geom_point() + 
  facet_wrap(~ Species) + # create a facet for each mountain range
  xlab("Surf_area") + 
  ylab("Exudation rate")
split_plot_by_species_AM

#Now, examining species (plot difference) as a fixed effect to the model above
Species_lm_AM <- lm(Exudation_rate ~ Surf_area + Species, data = rtrait_AM)
summary(Species_lm_AM)

Species_lm_EM <- lm(Exudation_rate ~ Surf_area + Species, data = rtrait_EcM)
summary(Species_lm_EM)
#These analyses also indicate correlation between Surf_area and Exudation_rate is significant only for EcM species, not for AM species

Species_lm_all <- lm(Exudation_rate ~ Surf_area + Species, data = rtrait)
summary(Species_lm_all)
#However, if we take all into account, the correlation is statistically significant

#Question here is whether we should perform random effects that would show grouping factors

# Scatterplot Matrix with functional types: MF, Phylo, Leaf types

# with different myco type
spm(~Exudation_rate+Diam+SRL+SRA+RTD+Mass|Myco, data=rtrait, main="Myco")
spm(~Exudation_rate+Diam+SRL+SRA+RTD+Mass|Myco, data=rtrait_AM, main="Myco")
spm(~Exudation_rate+Diam+SRL+SRA+RTD+Mass|Myco, data=rtrait_EcM, main="Myco")

# with different Phylo type
spm(~Exudation_rate+Diam+SRL+SRA+RTD+Mass|Phylo, data=rtrait, main="Phylo")
#AM only
spm(~Exudation_rate+Diam+SRL+SRA+RTD+Mass|Phylo, data=rtrait_AM, main="Phylo")
#EcM only
spm(~Exudation_rate+Diam+SRL+SRA+RTD+Mass|Phylo, data=rtrait_EcM, main="Phylo")

# with different Leaf type
spm(~Exudation_rate+Diam+SRL+SRA+RTD+Mass|Leaf, data=rtrait, main="Leaf")
#AM only
spm(~Exudation_rate+Diam+SRL+SRA+RTD+Mass|Leaf, data=rtrait_AM, main="Leaf")
#EcM only
spm(~Exudation_rate+Diam+SRL+SRA+RTD+Mass|Leaf, data=rtrait_EcM, main="Leaf")


#Plotting model predictions
install.packages("ggeffects")
library(ggeffects)


install.packages("multcomp")
library(multcomp)
multcomp::cld(fit_multi_2)
# the above won't be necessary for this project



#An example of taking a subset of two traits 
Exudation_only <- subset(roottrait,Myco %in% c("AM"), select=c(Exudation_rate,Diam))
# Save the entire workspace: allowing us to continue later exactly where you left off - without having to execute all the code again: 
# So you can return to where you were, like this: load("august8.RData"):

#The Question we are interested in 
# To what extent does root exudation rate vary among species? [ANOVA, though we're limited by study design - the 4-6 individual trees of each species are all located in the same plot, which isn't ideal for test assumptions.]
# 2) Does root exudation rate vary with plant or root traits? Which variables best predict root exudation rates? [ordination/PERMANOVA; mixed linear models that are similar to ANOVA and regressions but take plot-level replicates into account]
# 3) What are the relationships between root exudation rates (and other plant and root traits) and soil properties (rhizosphere effects in N cycling; aggregate stability)? [ordination/PERMANOVA; mixed linear models that are similar to ANOVA and regressions but take plot-level replicates into account]:

# To what extent does root exudation rate vary among species? [ANOVA, though we're limited by study design - the 4-6 individual trees of each species are all located in the same plot, which isn't ideal for test assumptions.]
# 1) Statistics analyses using ANOVA
# 2) Summary Table 
# 3) Important Figures

# 1) To answer the root trait variations among species we set out to use ANOVA to compare the variation between the group means to the variation within the groups (F-stat.). Here, we are using lm for ANOVA since lm is easier to be generalized when there is different n. (ANOVA is good when we have continuous response variable (measured traits) per a factor variable (species, myco association, etc) that defines the groups). 
# Take a subset and drop empty levels with droplevels.
#rtriatsub <- droplevels(subset(rtrait, Species %in% c("Exudation_rate","Diam","SRL","SRA","RTD","Mass")))

# Making a quick summary table
#with(rtriatsub, tapply(Exudation_rate, Species, mean)): this doesn't work, why?

# Gets means and standard deviation of rating by Species:
rating_exudation_species <- with(rtrait, tapply(Exudation_rate, Species, FUN=mean))
rating_exudation_SD_species <- with(rtrait, tapply(Exudation_rate, Species, FUN=sd))
install.packages("plotrix")
library(plotrix)
b <- barplot(rating_exudation_species, col="white", width=0.5, space=0.5, ylim=c(0,150))
plotCI(b, rating_exudation_SD_species, uiw=rating_exudation_SD_species, add=TRUE, pch=NA)

# Gets means and standard deviation of rating by Myco types
rating_exudation_Myco <- with(rtrait, tapply(Exudation_rate, Myco, FUN=mean))
rating_exudation_SD_Myco <- with(rtrait, tapply(Exudation_rate, Myco, FUN=sd))
b_Myco <- barplot(rating_exudation_Myco, col="white", width=0.5, space=0.5, ylim=c(0,150))
plotCI(b_Myco, rating_exudation_Myco, uiw=rating_exudation_SD_Myco, add=TRUE, pch=NA)

# Take a subset of the data with only 'AM' Myco type,
# keep only 'Species' and 'Exudation_rate' 

AMsubs_SP_ER <- subset(rtrait, Myco %in% c("AM"), select=c(Species,Exudation_rate))
EMsubs_SP_ER <- subset(rtrait, Myco %in% c("EM"), select=c(Species,Exudation_rate))

# Gets means and standard deviation of rating by Myco_AM types
rating_exudation_AM <- with(AMsubs_SP_ER, tapply(Exudation_rate, Species, FUN=mean))
rating_exudation_SD_AM <- with(AMsubs_SP_ER, tapply(Exudation_rate, Species, FUN=sd))
rating_exudation_EM <- with(EMsubs_SP_ER, tapply(Exudation_rate, Species, FUN=mean))
rating_exudation_SD_EM <- with(EMsubs_SP_ER, tapply(Exudation_rate, Species, FUN=sd))

# Set up two plots side-by-side
par(mfrow=c(1,2))

#AM on the left side
b_Myco_AM <- barplot(rating_exudation_AM, col="white", width=0.5, space=0.5, ylim=c(0,150))
plotCI(b_Myco_AM, rating_exudation_AM, uiw=rating_exudation_SD_Myco, add=TRUE, pch=NA)
#EM on the right side
b_Myco_EM <- barplot(rating_exudation_EM, col="white", width=0.5, space=0.5, ylim=c(0,150))
plotCI(b_Myco, rating_exudation_EM, uiw=rating_exudation_SD_EM, add=TRUE, pch=NA)

#stat analysis

#Fitting a one-way ANOVA with lm on Exudation_rate ~ Species
fit1 <- lm(Exudation_rate ~ Species, data=rtrait)
# more details of the fit using summary
summary(fit1)

#Fitting a one-way ANOVA with lm on Exudation_rate ~ Myco
fit1_1 <- lm(Exudation_rate ~ Myco, data=rtrait)
# more details of the fit using summary
summary(fit1_1)

#Fitting a one-way ANOVA with lm on Exudation_rate ~ Phylo
fit1_2 <- lm(Exudation_rate ~ Phylo, data=rtrait)
# more details of the fit using summary
summary(fit1_2)

#Fitting a one-way ANOVA with lm on Exudation_rate ~ Leaf
fit1_3 <- lm(Exudation_rate ~ Leaf, data=rtrait)
# more details of the fit using summary
summary(fit1_3)

#Fitting a one-way ANOVA with lm on RTD ~ Species
fit2 <- lm(RTD ~ Species, data=rtrait)
# more details of the fit using summary
summary(fit2)

#Fitting a one-way ANOVA with lm on SRL ~ Species
fit3 <- lm(SRL ~ Species, data=rtrait)
# more details of the fit using summary
summary(fit3)

#Now that we have a framework to examine significant diff. in root traits among various assigned groups, we set out to do multiple comparison

# First fit the linear model again (a one-way ANOVA,
# because species is a factor)

lmSpec <- lm(Exudation_rate ~ Species, data=rtrait)

# Load package
library(emmeans)
tukey_Spec <- emmeans(lmSpec, 'Species')
pairs(tukey_Spec)
summary(tukey_Spec)

#’single-step method’
pairs(tukey_Spec, adjust="bonferroni")

# A plot of a fitted 'emmeans' object (multiple comparison)
pwpp(tukey_Spec)

# Count nr of observations
xtabs( ~ Species + Myco, data=rtrait)

# Fit linear model
fit_2nd <- lm(Exudation_rate ~ Leaf + Myco, data=rtrait)
library(car)
# Perform an ANOVA on a fitted model, giving F-statistics
Anova(fit_2nd)
# Perform an ANOVA on a fitted model with three factors, giving F-statistics
fit_3rd <- lm(Exudation_rate ~ Leaf + Myco + Phylo, data=rtrait)
Anova(fit_3rd)

#Figuring our the interactions
# Two equivalent ways of specifying a linear model that includes all main effects
# and interactions:

fit_4th <- lm(Exudation_rate ~ Myco + Leaf + Myco:Leaf, data=rtrait)
Anova(fit_4th)
#no sig. interactions

#An interaction plot for Myco vs. Leaf
with(rtrait, interaction.plot(Myco, Leaf, Exudation_rate))


#Plotting
#
#For plotting, here is instruction for ggplot2
#http://blog.echen.me/2012/01/17/quick-introduction-to-ggplot2/

#The following two invocations are equivalent.
#qplot(director, data = movies, geom = "bar", ylab = "# movies")
#qplot(Sepal.Length, Petal.Length, data = iris)

ggplot(rtrait, aes(x=Surf_area, y=Exudation_rate, color=Myco, shape=Myco)) + geom_point(size=3) + 
  geom_smooth(method="lm", aes(fill=Myco))

ggplot(rtrait, aes(x=SRL, y=Exudation_rate, color=Myco, shape=Myco)) + geom_point(size=3) + 
  geom_smooth(method="lm", aes(fill=Myco))

#Facets divide a plot into separate subplots based on one or more discrete variables
plt <- ggplot(rtrait, aes(x=Surf_area, y=Exudation_rate, color=Myco, shape=Myco)) + geom_point(size=3) +
geom_smooth(method="lm", aes(fill=Myco)
plt + facet_wrap(~Myco)

install.packages("magicaxis")
library(magicaxis)

ggplot(rtrait,aes(x = Surf_area, y = Exudation_rate, color=Myco, shape=Myco)) + 
  geom_point(size=3) + 
  geom_smooth(method = "lm", aes(fill=Myco)) +
  stat_regline_equation(label.y = 400, aes(label = ..eq.label..)) +
  stat_regline_equation(label.y = 350, aes(label = ..rr.label..))

plt <- ggplot(rtrait,aes(x = Surf_area, y = Exudation_rate, color=Myco, shape=Myco)) + 
  geom_point(size=3) + 
  geom_smooth(method = "lm", aes(fill=Myco)) +
  stat_regline_equation(label.y = 400, aes(label = ..eq.label..)) +
  stat_regline_equation(label.y = 350, aes(label = ..rr.label..)) + stat_compare_means(method = "t.test")

plt + facet_wrap(~Myco)

ggscatter(rtrait, x = "Surf_area", y = "Exudation_rate",
add = "reg.line", conf.int = TRUE,
cor.coef = TRUE, cor.method = "pearson",
xlab = "Surf_area", ylab = "Exudation_rate")

#Correlation sig. testing
ggqqplot(rtrait$Exudation_rate, ylab = "Exudation_rate")
res <- cor.test(rtrait$Exudation_rate, rtrait$Surf_area,
method = "pearson")
res

#Spearman’s rho statistic is also used to estimate a rank-based measure of association. This test may be used 
#if the data do not come from a bivariate normal distribution.
res2 <-cor.test(rtrait$Exudation_rate, rtrait$Surf_area, method = "spearman")
res2

install.packages("ggpmisc")
library(ggpmisc)
library(ggplot2)

my.formula = y ~ poly(x,2)

#linear fitting
ggplot(data = rtrait, aes(x = Surf_area, y = Exudation_rate)) + geom_point(size=3) + geom_smooth(formula = y ~ x, method = "lm")

#polynomial fitting
ggplot(data = rtrait, aes(x = Surf_area, y = Exudation_rate)) + geom_point(size=3) + geom_smooth(formula= y ~ poly(x,2), method = "lm")

ggboxplot(rtrait, x = "Myco", y = "Exudation_rate", color = "Myco", palette = "jco")+
stat_compare_means(method = "anova")

  
boxplot(Exudation_rate ~ Species, data=rtrait, ylab="Exudation_rate", xlab="Species")
boxplot(Exudation_rate ~ Myco, data=rtrait, ylab="Exudation rate", xlab="Myco. types")
boxplot(Exudation_rate ~ Phylo, data=rtrait, ylab="Exudation rate", xlab="Phylo. types")
boxplot(Exudation_rate ~ Leaf, data=rtrait, ylab="Exudation rate", xlab="Leaf. types")

#boxplots to quickly plot means and ranges for exudation varying with 1) species, 2) Myco types:
boxplot(Exudation_rate ~ Species, data=rtrait, ylab="Exudation_rate", xlab="Species")
boxplot(Exudation_rate ~ Myco, data=rtrait, ylab="Exudation rate", xlab="Myco. types")
boxplot(Exudation_rate ~ Phylo, data=rtrait, ylab="Exudation rate", xlab="Phylo. types")
#visualizing correlation between main function types and exudation rates (Myco/Leaf/Phylo vs. Exudation):
scatterplot(Exudation_rate ~ Mass | Myco, data=rtrait, xlab="Mass", ylab="Exudation_rate",main="Enhanced Scatter Plot")
scatterplot(Exudation_rate ~ Mass | Leaf, data=rtrait, xlab="Mass", ylab="Exudation_rate",main="Enhanced Scatter Plot")
# Basic Scatterplot Matrix
pairs(~Exudation_rate+Diam+SRL+SRA+RTD+Mass,data=rtrait,main="Scatterplot Matrix")
# Scatterplot Matrix with functional types
spm(~Exudation_rate+Diam+SRL+SRA+RTD+Mass|Myco, data=rtrait, main="Myco")