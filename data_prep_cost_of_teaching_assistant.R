##########################################################
## This R code accompanies Gurven M., Davison R., & T.S. Kraft (XXX), "The optimal timing of learning and instruction across the life course." The script below reproduces the empirical analyses of the cost of teaching in Section 1 of the supplementary materials. 
## The analysis investigates the potential cost of teaching from hunting with assistants (see other code file for preparation of data on cooperative foraging with young group members).
##########################################################

# Clear the workspace
rm(list=ls())

# This analysis makes use of the cross-cultural hunting database of foraging societies compiled by Koster et al. (In press). A preprint of the associated manuscript can be found at: https://www.biorxiv.org/content/biorxiv/early/2019/03/12/574483.full.pdf
# The authors of this manuscript have generously made their complete database available for use by other researchers. In order to access these data directly through R, one must first install from Richard McElreath's github by running the following two lines (these can be ignored after the first time running).
library(devtools)
devtools::install_github("rmcelreath/cchunts")

# the "cchunts" package contains data and helper functions from Koster et al. (In press).
library(cchunts)

# load other packages that will be used
library(rstan)
library(brms)
library(lme4)
library(tidyverse)
library(mice)

# function for calculating standard error
stderr <- function(x, na.rm=FALSE) {
  if (na.rm) x <- na.omit(x)
  sqrt(var(x)/length(x))
}


# Begin by using the helper function "make_joint" to assemble all data files from Koster et al. into a single dataframe
ad <- make_joint()

# We are aware of a single typo in the database that can be corrected as follows:
ad$trip_id[which(ad$trip_id == 8235 & ad$day_id == 3861)] <- max(ad$trip_id, na.rm=T)+1

# Koster et al. provide the "prep_data" function to put the data into a format for their analysis using a Stan model. This function also identifies some cases of missing variables that can reasonably be set to 0, or flagged with -1.
prepped <- prep_data(ad)
ad$dogs <- prepped$dogs
ad$gun <- prepped$firearms
ad$pooled <- prepped$pooled

# Because we do not use the -1 flags for marginalization, we revert those cases back to NA
ad$dogs[which(ad$dogs == -1)] <- NA
ad$gun[which(ad$gun == -1)] <- NA

# Calculate the number of unique primary foragers for each trip
ad <- ad %>% group_by(trip_id) %>% mutate(number_primary_foragers= length(unique(forager_id)))

# Koster et al. use an elegant imputation strategy to deal with uncertainty in forager ages. For simplicity, we just take averages of age intervals while noting that it would be possible to construct a similar routine to Koster et al. here if desired. 
# Single age variable for primary foragers
ad$age_mean <- ad$age_dist_1
ad$age_mean[which(ad$age_type == "Uniform")] <- rowMeans(ad[which(ad$age_type == "Uniform"), c("age_dist_1", "age_dist_2")])

#Single age categories for assistants
for(i in 1:9){
  ad[ , paste0("a_", i, "_age_mean")] <- ad[ , paste0("a_", i, "_age_dist_1")]
  
  ad[which(ad$age_type == "Uniform") , paste0("a_", i, "_age_mean")] <- rowMeans( ad[which(ad$age_type == "Uniform"), c(paste0("a_", i, "_age_dist_1"), paste0("a_", i, "_age_dist_2"))])
}

# count number of people accompanying
ad$number.accomp <- apply(ad[, c("a_1_id", "a_2_id", "a_3_id", "a_4_id", "a_5_id", "a_6_id", "a_7_id", "a_8_id", "a_9_id")], 1, function(x) sum(!is.na(x)))

# Calculate age differences for assistants
for(t in 1:9) {ad[paste("age_diff_",t,sep="")] <- ad$age_mean - ad[, paste("a_", t, "_age_mean", sep="")]}

# indicator variable of whether at least 1 person is accompanying
ad$has.accompany <- ifelse(!is.na(ad$a_1_id), 1, 0)

# Some studies never recorded the presence of assistants, and these societies are all removed here from the the analysis
soc_accomp <- as.data.frame(xtabs(~society_id+has.accompany, data=ad))
ad <- ad[which(!ad$society_id %in% soc_accomp$society_id[which(soc_accomp$has.accompany == 1 & soc_accomp$Freq == 0)]), ]

# Define teaching as accompanyment by an assistant of less than 20 years old
for(t in 1:6){
  ad[paste("a_",t,"_teach", sep="")] <- ifelse(
    (ad[, paste("a_", t, "_age_mean", sep="")] < 20 &
       ad[, paste("a_", t, "_age_mean", sep="")] < ad$age_mean), 1, 0)
}

# Sum up the number of assistants "taught" for each row
ad$num.taught <- rowSums(ad[, c("a_1_teach", "a_2_teach", "a_3_teach", "a_4_teach", "a_5_teach", "a_6_teach")], na.rm=T)

# Create binary variable that incidates at least 1 assistant taught
ad$teach_yn <- (ad$num.taught >= 1)*1  #425 cases of teaching by this definition

## Compile final analysis dataframe
# filter out large (> 5 members) groups, and very old individuals: 571 rows removed (only 32 of those cases are individuals older than age 75)
mod_dat <- ad[ which(ad$number_primary_foragers < 6 & !is.na(ad$age_mean) & ad$age_mean < 75), ]

# reduce to only variables utilized in analysis
mod_dat <- select(mod_dat, harvest, age_mean, teach_yn, trip_duration, dogs, gun, num.taught, number_primary_foragers, forager_id, society, sex, pooled)

# convert to factor data type
mod_dat$society <- as.factor(mod_dat$society)
mod_dat$teach_yn <- as.factor(mod_dat$teach_yn)

# standardize age for more efficient sampling
mean_age <- mean(mod_dat$age_mean, na.rm=T)  #save for later processing
sd_age <- sd(mod_dat$age_mean, na.rm=T)
mod_dat$age_mean_z <- (mod_dat$age_mean - mean_age)/sd_age

## Multiple imputation
apply(mod_dat, 2, function(x) sum(is.na(x))) # see number of missing values in each variable

# Use multivariate imputation from mice package to generate 5 imputations
mod_dat_imp <- mice(mod_dat, m = 5, print = FALSE)
