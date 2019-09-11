###########################################
### Modeling and plotting
###########################################
# This will produce the analysis and plot for Figure 5A
# To run the following code, first run file: "data_prep_cost_of_teaching_young_solo_group.R"

# View default priors
get_prior(harvest~includes_young + s(age_mean_z) + sex + trip_duration + dogs + gun + pooled*number_primary_foragers+(1|forager_id) + (1|society), data=complete(young_imp, 1), family=hurdle_lognormal)

## define priors
# Can set all population paremter priors to a normal distribution with mean 0 and sd 20 to be weakly informative. For now we do not set any priors, leaving the improper flat prior.
# prior1 <- c(set_prior("normal(0,20)", class = "b", coef = ""))

## Run model
mod_teacher_cost_group <- brm_multiple(harvest~includes_young + s(age_mean_z) + sex + trip_duration + dogs + gun + pooled*number_primary_foragers+(1|forager_id) + (1|society), 
                                           data=young_imp,
                                           cores = 2, chains=4, iter = 2000, warmup = 1000, 
                                           family=hurdle_lognormal, 
                                           control = list(adapt_delta = 0.975))


plot(mod_teacher_cost_group)   # view trace plots
summary(mod_teacher_cost_group)  # view model summary, R_hat, n_eff

### Plotting
# Recreate figure from paper
len <- 200
reps <- 2
# set levels at which to predict at (range of ages, no dogs, no guns, 1 primary forager, median trip duration, non-pooled harvest, male forager)
pdat <- data.frame(
  age_mean_z = rep(seq(min(mod_dat$age_mean_z, na.rm=T), max(mod_dat$age_mean_z, na.rm=T), length.out=len), reps),
  includes_young = as.factor(rep(c(0,1), each=len)),
  dogs = rep(0, len*reps), 
  gun = rep(0, len*reps),
  number_primary_foragers = rep(1, len*reps),
  trip_duration = rep(median(mod_dat$trip_duration, na.rm=T), len*reps),
  pooled = rep(0, len*reps), 
  sex = rep("M", len*reps) 
)

# Add marginal predictions of the regression curve to levels specified
pdat <-cbind(pdat, yp=fitted(mod_teacher_cost_group, newdata= pdat, re_formula=NA, scale="response") )

# Back-transform standardized ages to natural scale
pdat$age_mean <- (pdat$age_mean_z*sd_age_young) + mean_age_young


# Recreate plot from paper
ggplot(pdat, aes(x=age_mean, y= yp.Estimate, group=includes_young)) + 
  geom_line(aes(colour=includes_young)) + labs(x="Age", y="Harvest (kg meat)") + 
  geom_ribbon(aes(ymin=pdat$yp.Q2.5, ymax=pdat$yp.Q97.5, fill=includes_young), alpha=0.2) +
  theme_classic(base_size = 15) +
  scale_colour_discrete(name = "", labels=c("No young members", "Includes young members"))+
  scale_fill_discrete(name= "", labels=c("No young members", "Includes young members")) +
  theme(legend.position=c(.71,.98)) +
  annotate("text", x=20, y=16, label="(A)", size=7)


# print data for calculation in text of paper
pdat <- data.frame(age_mean = c(35, 35), includes_young = c(0,1), dogs = rep(0, 2), gun = rep(0, 2), number_primary_foragers = rep(2, 2), sex = c("M", "M"), trip_duration = c(7,7), pooled=c(0,0) )
