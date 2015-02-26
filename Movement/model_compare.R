### Negative log-likelihood equations -------------------------------------------

# Unbounded power law (infinite tail)
# equation (5) in Edwards et al. (2007)
pl_nll <-  function(mu, x, n, a){
  if(mu <= 1){
    stop(
      "WARNING: Cannot have mu less than or equal to 1 for pow law over inf range"
      ) # Change starting value if this appears
  } 
  else{
    -(n * log(mu - 1) + n *(mu - 1) * log(a) - mu * sum(log(x)))
  }
}

# Unbounded exponential (infinite tail)
# equation (6) in Edwards et al. (2007)
exp_nll = function(lambda, x, n, a){
  -(n * log(lambda) + n * lambda * a - lambda * sum(x))
}

# Bounded power law
plb_nll = function(mu, x, n, a, b){
  if(mu == 1){
    # equation (A.25) in Edwards (2011)
    -(-n * log(log(b / a)) - sum(log(x)))
  }
  else{
    # equation (A.23) in Edwards (2011)
    -(n * log((mu - 1) / (a ^ (1 - mu) - b ^ (1 - mu))) - mu * sum(log(x)))
  }
}

# Bounded exponential
# equation (A.27) in Edwards (2011)
expb_nll = function(lambda, x, n, a, b){

  -(n * log(abs(lambda)) - n * log(abs(exp(-lambda * a) - exp(-lambda * b))) -
      lambda * sum(x))
}

### Data -----------------------------------------------------------------------
load('movement/sb_speed.rda')
library(dplyr)
spawn <- secor.sb %>% 
  filter(phase == 'spawning',
         mean.sp.bl > 0) %>% 
  select(mean.sp.bl) %>% 
  mutate(rank = rank(mean.sp.bl, ties.method = 'first')) %>% 
  arrange(rank)

run <- secor.sb %>% 
  filter(phase == 'running',
         mean.sp.bl > 0) %>% 
  select(mean.sp.bl) %>% 
  mutate(rank = rank(mean.sp.bl, ties.method = 'first')) %>% 
  arrange(rank)

a <- min(spawn$mean.sp.bl)
b <- 8
n <- dim(spawn)[1]



### PL Neg-loglikelihood -------------------------------------------------------
## Analytical solution (Box 1 of Edwards et al. 2007):
pl_muanal <- 1/(-log(a) + sum(log(run$mean.sp.bl)) / n) + 1

## Numerical solution, check to make sure this matches with analytical
mu.start <- 1.05
pl_out <- nlm(pl_nll, mu.start, x = run$mean.sp.bl, n = n, a = a)
# µ estimate is pl_out$estimate
# Negative log likelihood is pl_out$minimum


## Normalization constant in probability density function
# See equation (3) in Edwards et al. (2007)
pl_C <- (pl_out$estimate - 1) * a ^ (pl_out$estimate - 1)

## AIC and BIC calculation
# Estimated two parameters (µ and a) for unbounded tail
n.param <- 2
AIC_pl <- 2 * pl_out$minimum + 2 * n.param
BIC_pl <- 2 * pl_out$minimum + n.param * log(n) # From B&A p286

## Calculation of 95% CI of MLE µ:
# Range of µ to try, can't be <= 1
muvec <- seq(1.01, 1.3, 0.0001)

pl_muvary <- NULL # negative log-likelihood or each µ value
for(i in 1:length(muvec)){
  pl_muvary[i] = pl_nll(muvec[i], x = run$mean.sp.bl, n = n, a = a)
}

critval <- pl_out$minimum + qchisq(0.95, 1) / 2  
                  # 1 dof. Hilborn and Mangel (1997) p163.

# 95% CI:
c(min(muvec[pl_muvary < critval]), max(muvec[pl_muvary < critval]))
# Visually
plot(muvec, pl_muvary)
abline(h = critval, col = "red")


### Exp Neg-loglikelihood ------------------------------------------------------
## Analytical solution (Box 1, Edwards et al. 2007)
exp_lamb_anal <-  1 / (sum(run$mean.sp.bl) / n - a)    # MLE for lambda

## Numerical solution, check to make sure this matches analytical
lambda.start <- 0.1
exp_out <- nlm(exp_nll, lambda.start, x = run$mean.sp.bl, n = n, a = a)


## Normalization constant in probability density function
exp_C <- exp_out$estimate * exp(exp_out$estimate * a)

## AIC and BIC calculation
# Estimated two parameters (lambda and a) for unbounded tail
n.param <- 2
AIC_exp <- 2 * exp_out$minimum + 2 * n.param   
BIC_exp <- 2 * exp_out$minimum + n.param * log(n) # B&A p286


## Calculation of 95% CI of MLE lambda: 
lambdavec <- seq(round(0.5 * exp_out$estimate, digits=5), 2 * exp_out$estimate,
                 0.00001)

exp_lambdavary <- NULL
for(i in 1:length(lambdavec)){
  exp_lambdavary[i] = exp_nll(lambdavec[i], x = run$mean.sp.bl, n = n, a = a)
}

critval <- exp_out$minimum + qchisq(0.95, 1) / 2
                 # 1 dof. Hilborn and Mangel (1997) p163.

# 95% CI:
c(min(lambdavec[exp_lambdavary < critval]),
  max(lambdavec[exp_lambdavary < critval]))
# Visually
plot(lambdavec, exp_lambdavary)
abline(h = critval, col = "red")


### PLB Neg-loglikelihood ------------------------------------------------------
mu.start <- 0.6
plb_out <- nlm(plb_nll, mu.start, x = run$mean.sp.bl, n = n, a = a, b = b)


## Normalization constant in probability density function
plb_C <- (plb_out$estimate - 1) / (a ^ (1 - plb_out$estimate) -
                                    b ^ (1 - plb_out$estimate))

# Estimated three parameters (µ, a, and b) for unbounded tail
n.param <- 3
AIC_plb <- 2 * plb_out$minimum + 2 * n.param
BIC_plb <- 2 * plb_out$minimum + n.param * log(n) # B&A p286


## Calculation of 95% CI of MLE µ:
# Range of mu to try for PLB model, can't be <= 1
muvec <- seq(0.5, 1, 0.0001)

plb_muvary <- NULL
for(i in 1:length(muvec)){
  plb_muvary[i] <- plb_nll(muvec[i], x = run$mean.sp.bl, n = n, a = a, b = b)
}

critval <- plb_out$minimum + qchisq(0.95 , 1) / 2
                    # 1 dof. Hilborn and Mangel (1997) p163.
# 95% CI:
c(min(muvec[plb_muvary < critval]), max(muvec[plb_muvary < critval]))
# Visually:
plot(muvec, plb_muvary)
abline(h = critval, col = "red")


### Exp Neg-loglikelihood ------------------------------------------------------
lambda.start <- 0.1
expb_out <- nlm(expb_nll, lambda.start, x = run$mean.sp.bl, n = n, a = a, b = b)


## Normalization constant in probability density function
expb_C <- expb_out$estimate / (exp(-expb_out$estimate * a) -
                                exp(-expb_out$estimate * b))

## AIC and BIC calculation
# Estimated three parameters (lambda, a, and b)
n.param <- 3
AIC_expb <- 2 * expb_out$minimum + 2 * n.param
BIC_expb <- 2 * expb_out$minimum + n.param * log(n) # From B&A p286


## Calculation of 95% CI of MLE lambda: 
lambdavec <- seq(round(0.5*expb_out$estimate, digits=5), 2 * expb_out$estimate,
                 0.00001)

expb_lambdavary = NULL
for(i in 1:length(lambdavec)){
  expb_lambdavary[i] = expb_nll(lambdavec[i],
                                x = run$mean.sp.bl, n = n, a = a, b = b)
}

critval = expb_out$minimum + qchisq(0.95 , 1) / 2
                # 1 dof. Hilborn and Mangel (1997) p163.

# 95% CI:
c(min(lambdavec[expb_lambdavary < critval]),
  max(lambdavec[expb_lambdavary < critval]))
# Visually
plot(lambdavec, expb_lambdavary)
abline(h = critval, col = "red")
