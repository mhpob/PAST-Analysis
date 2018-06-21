library(lubridate); library(ggplot2); library(dplyr)
load('secor.sb.rda')

# Data prep ----
valid.fish <- secor.sb %>%
  group_by(transmitter) %>%
  summarize(max = max(date.local)) %>%
  left_join(secor.sb) %>%
  filter(grepl('[34]/\\d*/2014', tag.date) |
           (tag.date == '10/30/2014' & year(date.local) >= 2015),
         !grepl('465', transmitter)) %>%
  mutate(year = year(date.local)) %>%
  group_by(transmitter, year, max) %>%
  summarize(max.yr = max(date.local),
            max.mo = month(max.yr)) %>%
  # Select fish that either 1) made it into August of that year, or 2) who
  # weren't heard after July of a year, but were heard in years after
  filter(max.mo >= 8 | max > max.yr)

valid.data <- ungroup(valid.fish) %>%
  left_join(mutate(secor.sb, year = year(date.local))) %>% 
  mutate(age = age + (year - 2014),
         year = as.factor(year),
         coastal = case_when(array %in% c('VA Coast', 'MD Coast', 'DE Coast',
                                          'NYB', 'Hudson', 'Long Island', 'Mass',
                                          'New Jersey') ~ T,
                             T ~ F)) %>% 
  group_by(transmitter, year, age, length, weight, sex) %>% 
  summarize(coastal = T %in% coastal) %>% 
  mutate(c.num = ifelse(coastal == T, 1, 0))

# Logistic regression plot
library(ggplot2)
logis_plot <- function(data, xvar, group = year){
  xvar <- enquo(xvar)
  group <- enquo(group)
  ggplot() + geom_point(data = data, aes(x = !! xvar, y = c.num,
                                               color = !! group)) +
    stat_smooth(data = data, aes(x = !! xvar, y = c.num,
                                       color = !! group),
                method = 'glm', method.args = list(family = 'binomial')) +
    labs(x = xvar, y = 'Proportion Coastal', color = group) +
    theme_bw()
}

logis_plot(valid.data, age)
# logis_plot(valid.data, length)
# logis_plot(valid.data, weight)


### Per-year model fitting and bootstrapping ----
log_emig <- split(valid.data, valid.data$year)

## Logistic regression on a variable vs T/F coastal
l_e_fit <- function(data, variable){
  form <- as.formula(paste0('c.num ~ ', variable))
  
  lapply(data, function(x){
    glm(form, family = 'binomial', data = x)
  })
  
}

age_fit <- l_e_fit(log_emig, 'age')
tl_fit <- l_e_fit(log_emig, 'length')
wt_fit <- l_e_fit(log_emig, 'weight')

## Bootstrapping
library(boot)

# Funtion to be bootstrapped
boot_fun <- function(x, indices, form){
  d <- x[indices,]
  fit <- glm(form, family = 'binomial', data = d)
  coeffs <- coef(fit)
  
  # Formula below is reduction of ((log(prop / (1 - prop)) - intercept) / age)
  # where prop = 0.5. Used to calculate age @ 50% coastal. Can be adjusted
  # later if different proportions are desired
  pct50 <- setNames(-coeffs[1] / coeffs[2], 'pct50')
  coeffs <- c(coeffs, pct50)
  
  # Returns GLM coefficients and the length at 50% coastal
  coeffs
}

# Apply 10k times over each year subset.
l_e_boot <- function(data, variable){
  form <- as.formula(paste0('c.num ~ ', variable))
  ncore <- as.integer(Sys.getenv('NUMBER_OF_PROCESSORS')) - 2
  
  lapply(data, function(x){
    boot(data = x, statistic = boot_fun, R = 10000, form = form,
         parallel = 'snow', ncpus = ncore)
  })
}

age_boot <- l_e_boot(log_emig, 'age')
tl_boot <- l_e_boot(log_emig, 'length')
wt_boot <- l_e_boot(log_emig, 'weight')

# BCa (adjusted bootstrap percentile) 95% CIs
l_e_bootci <- function(boot.fit){
  lapply(boot.fit, function(x){
    temp <- list(pct50 = boot.ci(x, index = 3, type = 'bca'),
                 intercept = boot.ci(x, index = 1, type = 'bca'),
                 hold = boot.ci(x, index = 2, type = 'bca'))
    names(temp)[3] <- names(x$t0)[2]
    temp
  })
}

age_bootci <- l_e_bootci(age_boot)
tl_bootci <- l_e_bootci(tl_boot)
wt_bootci <- l_e_bootci(wt_boot)

# lapply(wt_boot, `[`, 't0')
# lapply(wt_fit, summary)

## Logit model differences significance test
# Method from http://derekogle.com/fishR/examples/oldFishRVignettes/Maturity.pdf
model <- glm(c.num ~ age * year, family = 'binomial',
             data = valid.data, subset = (year %in% c(2014, 2015)))
drop1(model, ~., test = 'Chisq')

# Method from https://stats.stackexchange.com/questions/316801/how-to-compare-logistic-regression-curves
modelnoint <- glm(c.num ~ age + year, family = 'binomial',
                  data = valid.data, subset = (year %in% c(2014, 2015)))

anova(modelnoint, model, test = 'Chisq')



# Confidence interval calculation and plotting ----
## Percentile
fun <- function(x, coefs){ 1 / (1 + exp(-(coefs[2] * x + coefs[1])))}

# Points where to evaluate the model
x_eval <- seq(6, 13, length.out = 100)

# Matrix with the predictions
Pred_mat <- apply(age_boot[['2014']]$t, 1, function(coefs) fun(x_eval, coefs))

# Pack the estimates for plotting
Estims_plot <- cbind(
  age = x_eval, 
  as.data.frame(t(apply(Pred_mat, 1, function(y_est) c(
    median_est = median(y_est), 
    ci_lower_est = quantile(y_est, probs = 0.025, names = FALSE), 
    ci_upper_est = quantile(y_est, probs = 0.975, names = FALSE)
  ))))
)

ggplot() + geom_point(data = filter(valid.data, year == 2017), aes(x = age, y = c.num))+
  geom_ribbon(data = Estims_plot, aes(x = age, ymin  = ci_lower_est, 
                                      ymax = ci_upper_est, alpha = 0.7)) +
  stat_smooth(data = filter(valid.data, year == 2017), aes(x = age, y = c.num),
              method = 'glm', method.args = list(family = 'binomial'), se = F)

## From bootstrapping
b_fun <- function(x, indices){
  d <- x[indices,]
  fit <- glm(c.num ~ age, family = 'binomial', data = d)
  coeffs <- coef(fit)
  
  # Formula below is reduction of ((log(prop / (1 - prop)) - intercept) / age)
  # where prop = 0.5. Used to calculate age @ 50% coastal. Can be adjusted
  # later if different proportions are desired
  pct50 <- setNames(-coeffs[1] / coeffs[2], 'pct50')
  coeffs <- c(coeffs, pct50)
  
  p <- range(x$age)
  p_seq <- seq(p[1], p[2], length.out = 100)
  hold <- predict(fit, data.frame(age = p_seq), type = 'response')
  names(hold) <- p_seq
  
  # Returns GLM coefficients, length at 50% coastal, and predicted values
  k <- c(coeffs, hold)
  k
}

# Bootstrap
l <- boot(data = log_emig[[1]], statistic = b_fun, R = 10000)

# Use boot.ci on predictions
m <- sapply(4:103,  function(x){
  hold <- boot.ci(l, index = x, type = 'perc')
  hold$perc[4:5]})

# Plot predictions
plot(m[1,] ~ seq(6,13, length.out = 100), ylim = c(0, 1))
points(m[2,]~ seq(6,13, length.out = 100))
abline(h = 0.5)
lines(predict(fit, data.frame(age = seq(6,13, length.out = 100)), type = 'response') ~
        seq(6,13, length.out = 100))
