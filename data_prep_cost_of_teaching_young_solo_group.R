##########################################################
## This R code accompanies Gurven M., Davison R., & T.S. Kraft (XXX), "The optimal timing of learning and instruction across the life course." The script below reproduces one of the empirical analyses of the cost of teaching in Section 1, and both the analyses of Section 2, of the supplementary materials. 
## For the analysis of the potential cost of teaching, this extracts data on cooperative hunts between primary foragers (see other code file for data preparation for assistants).
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
ae <- make_joint()

# We are aware of a single typo in the database that can be corrected as follows:
ae$trip_id[which(ae$trip_id == 8235 & ae$day_id == 3861)] <- max(ae$trip_id, na.rm=T)+1

# Koster et al. provide the "prep_data" function to put the data into a format for their analysis using a Stan model. This function also identifies some cases of missing variables that can reasonably be set to 0, or flagged with -1.
prepped <- prep_data(ae)
ae$dogs <- prepped$dogs
ae$gun <- prepped$firearms
ae$pooled <- prepped$pooled

# Because we do not use the -1 flags for marginalization, we revert those cases back to NA
ae$dogs[which(ae$dogs == -1)] <- NA
ae$gun[which(ae$gun == -1)] <- NA

# Calculate the number of unique primary foragers for each trip
ae <- ae %>% group_by(trip_id) %>% mutate(number_primary_foragers= length(unique(forager_id)))

# Koster et al. use an elegant imputation strategy to deal with uncertainty in forager ages. For simplicity, we just take averages of age intervals while noting that it would be possible to construct a similar routine to Koster et al. here if desired. 
# Single age variable for primary foragers
ae$age_mean <- ae$age_dist_1
ae$age_mean[which(ae$age_type == "Uniform")] <- rowMeans(ae[which(ae$age_type == "Uniform"), c("age_dist_1", "age_dist_2")])

#Single age categories for assistants
for(i in 1:9){
  ae[ , paste0("a_", i, "_age_mean")] <- ae[ , paste0("a_", i, "_age_dist_1")]
  
  ae[which(ae$age_type == "Uniform") , paste0("a_", i, "_age_mean")] <- rowMeans( ae[which(ae$age_type == "Uniform"), c(paste0("a_", i, "_age_dist_1"), paste0("a_", i, "_age_dist_2"))])
}

# remove societies where there was never anything other than solo foraging
soc_solo <- as.data.frame(xtabs(~society_id+number_primary_foragers, data=ae))
ae <- ae[which(ae$society_id %in% soc_solo$society_id[which(soc_solo$number_primary_foragers != "1" & soc_solo$Freq > 0)]), ]

# separate out data for analysis of 1) solo vs group hunts, and 2) inclusion of young hunters in cooperative groups (Fig. 6 in paper)
# For 1):
solo_dat <- ae

solo_dat$solo <- ifelse(solo_dat$number_primary_foragers > 1, 0, 1)

# standardize age for more efficient sampling
mean_age_solo <- mean(ae$age_mean, na.rm=T)  #save for later processing
sd_age_solo <- sd(ae$age_mean, na.rm=T)
solo_dat$age_mean_z <- (ae$age_mean - mean_age_solo)/sd_age_solo

# reduce dataset to analysis columns
solo_dat <- select(solo_dat, age_mean_z, solo, dogs, gun, forager_id, society, sex)

## Multiple imputation
apply(solo_dat, 2, function(x) sum(is.na(x))) # see number of missing values in each variable

# Use multivariate imputation from mice package to generate 5 imputations
solo_imp <- mice(solo_dat, m = 5, print = FALSE)


# For 2):
young_dat <- ae

# Use a simple binning procedure to determine if any foragers in a group are less than 20 years old, and assign a binary variable based on this outcome
young_dat$includes_young <- NA
for ( i in 1:nrow(young_dat) ){
  if(young_dat$number_primary_foragers[i] == 1) {
    next}
  
  trip <- young_dat$trip_id[i]
  rows <- which(young_dat$trip_id == trip)
  ages <- young_dat$age_mean[rows[!(rows %in% i)]]
  
  if(any(ages < 20, na.rm=T) == T) {
    young_dat$includes_young[i] <- 1
  }else{
    young_dat$includes_young[i] <- 0
  }
}

# apply exclusion criteria to remove cases of solo hunts (not possible to have a young group member in this case) as well as remove small number of cases where age of focal forager is unknown.

young_dat <- young_dat[which(!is.na(young_dat$age_mean) &
                               young_dat$number_primary_foragers != 1),
                       c("harvest", "trip_duration", "pooled", "age_mean", "dogs", "gun", "forager_id", "society", "sex", "includes_young", "number_primary_foragers")]

# standardize age for more efficient sampling
mean_age_young <- mean(young_dat$age_mean, na.rm=T)  #save for later processing
sd_age_young <- sd(young_dat$age_mean, na.rm=T)
young_dat$age_mean_z <- (young_dat$age_mean - mean_age_young)/sd_age_young

## Multiple imputation
apply(young_dat, 2, function(x) sum(is.na(x))) # see number of missing values in each variable

# Use multivariate imputation from mice package to generate 5 imputations
young_imp <- mice(young_dat, m=5, print=F)