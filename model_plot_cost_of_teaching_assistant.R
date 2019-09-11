###########################################
### Modeling and plotting
###########################################
# This will produce the analysis and plot for Figure 5B
# To run the following code, first run file: "data_prep_cost_of_teaching_assistant.R"

# View default priors
get_prior(harvest~teach_yn + s(age_mean) + sex + trip_duration + dogs + gun + pooled*number_primary_foragers+(1|forager_id) + (1|society), data=mod_dat, family=hurdle_lognormal)

## define priors
# Can set all population paremter priors to a normal distribution with mean 0 and sd 20 to be weakly informative. For now we do not set any priors, leaving the improper flat prior.
# prior1 <- c(set_prior("normal(0,20)", class = "b", coef = ""))

## Run model
mod_teacher_cost_assistant <- brm_multiple(harvest~teach_yn + s(age_mean_z) + sex + trip_duration + dogs + gun + pooled*number_primary_foragers+(1|forager_id) + (1|society), 
                                           data=mod_dat_imp,
                                           cores = 2, chains=4, iter = 2000, warmup = 1000, 
                                           family=hurdle_lognormal, 
                                           control = list(adapt_delta = 0.975))

plot(mod_teacher_cost_assistant)   # view trace plots
summary(mod_teacher_cost_assistant)  # view model summary, R_hat, n_eff

### Plotting
# Recreate figure from paper
len <- 200
reps <- 2
# set levels at which to predict at (range of ages, no dogs, no guns, 1 primary forager, median trip duration, non-pooled harvest, male forager)
pdat <- data.frame(
  age_mean_z = rep(seq(min(mod_dat$age_mean_z, na.rm=T), max(mod_dat$age_mean_z, na.rm=T), length.out=len), reps),
  teach_yn = as.factor(rep(c(0,1), each=len)),
  dogs = rep(0, len*reps), 
  gun = rep(0, len*reps),
  number_primary_foragers = rep(1, len*reps),
  trip_duration = rep(median(mod_dat$trip_duration, na.rm=T), len*reps),
  pooled = rep(0, len*reps), 
  sex = rep("M", len*reps) 
)

# Add marginal predictions of the regression curve to levels specified
pdat <-cbind(pdat, yp=fitted(mod_teacher_cost_assistant, newdata= pdat, re_formula=NA, scale="response") )

# Back-transform standardized ages to natural scale
pdat$age_mean <- (pdat$age_mean_z*sd_age) + mean_age


# Recreate plot from paper
ggplot(pdat, aes(x=age_mean, y= yp.Estimate, group=teach_yn)) + 
  geom_line(aes(colour=teach_yn)) + labs(x="Age", y="Harvest (kg meat)") + 
  geom_ribbon(aes(ymin=pdat$yp.Q2.5, ymax=pdat$yp.Q97.5, fill=teach_yn), alpha=0.2) +
  theme_classic(base_size = 15) +
  scale_colour_discrete(name = "", labels=c("No assistant(s)", "With assistant(s)"))+
  scale_fill_discrete(name= "", labels=c("No assistant(s)", "With assistant(s)")) +
  theme(legend.position=c(.8,.95)) +
  annotate("text", x=8, y=15, label="(B)", size=7)
