###########################################
### Modeling and plotting
###########################################
# This will produce the analysis and plot for Figure 6B
# To run the following code, first run file: "data_prep_cost_of_teaching_young_solo_group.R"

# View default priors
get_prior(includes_young ~ poly(age_mean_z, 3) + dogs + gun + number_primary_foragers + (1|forager_id) + (1|society), 
          data=complete(young_imp, 1), family="bernoulli")

## define priors
# Hard to define priors well for polynomials. See section in McElreath's Statistical Rethinking. Here we implement some very weakly informative priors. These seem to improve speed of sampling without affecting results.
prior1 <- c(prior(normal(0,10), class = b),
            prior(normal(0,50), class = b, coef = polyage_mean32),
            prior(normal(0,50), class =b, coef = polyage_mean33))

## Run model
mod_young <- brm_multiple(includes_young ~ poly(age_mean_z, 3) + dogs + gun + number_primary_foragers + (1|forager_id) + (1|society), 
                         data=young_imp,
                         cores = 2, chains=4, iter = 2000, warmup = 1000, 
                         family="bernoulli",
                         control = list(adapt_delta = 0.975, max_treedepth=13),
                         prior=prior1)

plot(mod_young)   # view trace plots
summary(mod_young)  # view model summary, R_hat, n_eff

### Plotting
# Recreate figure from paper
len <- 200
reps <- 2
# set levels at which to predict at (range of ages, no dogs, no guns, 1 primary forager, median trip duration, non-pooled harvest, male forager)
pdat <- data.frame(
  age_mean_z = rep(seq(min(mod_young$age_mean_z, na.rm=T), max(mod_young$age_mean_z, na.rm=T), length.out=len), reps),
  dogs = rep(0, len*reps), 
  gun = rep(0, len*reps),
)

# Add marginal predictions of the regression curve to levels specified
pdat <-cbind(pdat, yp=fitted(mod_young, newdata= pdat, re_formula=NA, scale="response") )

# Back-transform standardized ages to natural scale
pdat$age_mean <- (pdat$age_mean_z*sd_age_young) + mean_age_young


# Recreate plot from paper
ggplot(pdat, aes(x=age_mean, y= yp.Estimate)) + 
  geom_line() + labs(x="Age", y="Probability of hunting with young (<20 yo) individual") + 
  geom_ribbon(aes(ymin=pdat$yp.Q2.5, ymax=pdat$yp.Q97.5, fill=teach_yn), alpha=0.2) +
  theme_classic(base_size = 11) +
  lims(y=c(0,1)) +
  annotate("text", x = 8, y = 1, label ="(A)")

# alternative code to use with package sjPlot
# solo_plot <- plot_model(solo.mod, type="pred", terms = c("age_mean [all]"), condition = c(dogs = 0, gun=0)) + labs(x="Age", y= "Probability of hunting solo") + ggtitle("") + theme_classic(base_size=11) + lims(y=c(0,1)) + annotate("text", x = 8, y = 1, label ="(A)")







young_plot <- plot_model(young.mod.brm_poly, type="pred", terms=c("age_mean [all]"))+ labs(x="Age", y= "Probability of hunting with young (<20 yo) individual") + ggtitle("") + theme_classic(base_size=11) + lims(y=c(0,1)) + annotate("text", x = 8, y = 1, label ="(B)")
setwd("/Users/thomaskraft/Dropbox/tsimane/teaching/figures")
ggsave(filename="young_hunt_mod.png")