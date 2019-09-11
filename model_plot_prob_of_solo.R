###########################################
### Modeling and plotting
###########################################
# This will produce the analysis and plot for Figure 6A
# To run the following code, first run file: "data_prep_cost_of_teaching_young_solo_group.R"

# View default priors
get_prior(solo ~ poly(age_mean_z, 3) + dogs + gun + (1|forager_id) + (1|society), 
          data=complete(solo_imp, 1), family="bernoulli")

## define priors
# Hard to define priors well for polynomials. See section in McElreath's Statistical Rethinking. Use improper flat priors for now.
# prior1 <- c(set_prior("normal(0,20)", class = "b", coef = ""))

## Run model
mod_solo <- brm_multiple(solo ~ poly(age_mean_z, 3) + dogs + gun + (1|forager_id) + (1|society), 
                                           data=solo_imp,
                                           cores = 2, chains=4, iter = 2000, warmup = 1000, 
                                           family="bernoulli",
                                           control = list(adapt_delta = 0.975))


plot(mod_solo)   # view trace plots
summary(mod_solo)  # view model summary, R_hat, n_eff

### Plotting
# Recreate figure from paper
len <- 200
reps <- 2
# set levels at which to predict at (range of ages, no dogs, no guns, 1 primary forager, median trip duration, non-pooled harvest, male forager)
pdat <- data.frame(
  age_mean_z = rep(seq(min(solo_dat$age_mean_z, na.rm=T), max(solo_dat$age_mean_z, na.rm=T), length.out=len), reps),
  dogs = rep(0, len*reps), 
  gun = rep(0, len*reps)
)

# Add marginal predictions of the regression curve to levels specified
# Note that warning from brms about using only 1st imputed data set is a misnomer, it is in fact using all imputed data sets
pdat <-cbind(pdat, yp=predict(mod_solo, newdata= pdat, re_formula=NA, scale="response") )

# Back-transform standardized ages to natural scale
pdat$age_mean <- (pdat$age_mean_z*sd_age_solo) + mean_age_solo


# Recreate plot from paper
ggplot(pdat, aes(x=age_mean, y= yp.Estimate)) + 
  geom_line() + labs(x="Age", y="Probability of hunting solo") + 
  geom_ribbon(aes(ymin=pdat$yp.Q2.5, ymax=pdat$yp.Q97.5), alpha=0.2) +
  theme_classic(base_size = 11) +
  lims(y=c(0,1)) +
  annotate("text", x = 8, y = 1, label ="(A)")

# alternative code to use with package sjPlot
# solo_plot <- plot_model(solo.mod, type="pred", terms = c("age_mean [all]"), condition = c(dogs = 0, gun=0)) + labs(x="Age", y= "Probability of hunting solo") + ggtitle("") + theme_classic(base_size=11) + lims(y=c(0,1)) + annotate("text", x = 8, y = 1, label ="(A)")
