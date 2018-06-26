### Negative log-likelihood equations -------------------------------------------

# Unbounded power law (infinite tail)
# equation (5) in Edwards et al. (2007)
pl_nll <-  function(mu, data, n, a){
  if(mu <= 1){
    stop(
      "WARNING: Cannot have mu less than or equal to 1 for pow law over inf range"
      ) # Change starting value if this appears
  } 
  else{
    -(n * log(mu - 1) + n *(mu - 1) * log(a) - mu * sum(log(data)))
  }
}

# Unbounded exponential (infinite tail)
# equation (6) in Edwards et al. (2007)
exp_nll = function(lambda, data, n, a){
  -(n * log(lambda) + n * lambda * a - lambda * sum(data))
}

# Bounded power law
plb_nll = function(mu, data, n, a, b){
  if(mu == 1){
    # equation (A.25) in Edwards (2011)
    -(-n * log(log(b / a)) - sum(log(data)))
  }
  else{
    # equation (A.23) in Edwards (2011)
    -(n * log((mu - 1) / (a ^ (1 - mu) - b ^ (1 - mu))) - mu * sum(log(data)))
  }
}

# Bounded exponential
# equation (A.27) in Edwards (2011)
expb_nll = function(lambda, data, n, a, b){

  -(n * log(abs(lambda)) - n * log(abs(exp(-lambda * a) - exp(-lambda * b))) -
      lambda * sum(data))
}



### Data -----------------------------------------------------------------------
load('movement/sb.speed.rda')
library(dplyr)
spawn <- sb.speed %>% 
  filter(phase == 'spawning',
         mean.sp.bl <= 8) %>% 
  select(mean.sp.bl)

run <- sb.speed %>% 
  filter(phase == 'running',
         mean.sp.bl <= 8) %>% 
  select(mean.sp.bl)

sp.pot <- sb.speed %>% 
  filter(phase == 'spawning',
         system == 'Potomac',
         mean.sp.bl <= 8) %>% 
  select(mean.sp.bl)

r.pot <- sb.speed %>% 
  filter(phase == 'running',
         system == 'Potomac',
         mean.sp.bl <= 8) %>% 
  select(mean.sp.bl)


med <- sb.speed %>% 
  filter(length.bin == '55-65',
         mean.sp.bl <= 8) %>% 
  select(mean.sp.bl)

m.spawn <- sb.speed %>% 
  filter(length.bin == '55-65',
         phase == 'spawning',
         mean.sp.bl <= 8) %>% 
  select(mean.sp.bl)

m.run <- sb.speed %>% 
  filter(length.bin == '55-65',
         phase == 'running',
         mean.sp.bl <= 8) %>% 
  select(mean.sp.bl)

m.male <- sb.speed %>% 
  filter(length.bin == '55-65',
         sex == 'M',
         mean.sp.bl <= 8) %>% 
  select(mean.sp.bl)

m.female <- sb.speed %>% 
  filter(length.bin == '55-65',
         sex == 'F',
         mean.sp.bl <= 8) %>% 
  select(mean.sp.bl)

l <- sb.speed %>% 
  filter(length.bin == '65-80',
         mean.sp.bl <= 8) %>% 
  select(mean.sp.bl)

l.spawn <- sb.speed %>% 
  filter(length.bin == '65-80',
         phase == 'spawning',
         mean.sp.bl <= 8) %>% 
  select(mean.sp.bl)

l.run <- sb.speed %>% 
  filter(length.bin == '65-80',
         phase == 'running',
         mean.sp.bl <= 8) %>% 
  select(mean.sp.bl)

l.male <- sb.speed %>% 
  filter(length.bin == '65-80',
         sex == 'M',
         mean.sp.bl <= 8) %>% 
  select(mean.sp.bl)

l.female <- sb.speed %>% 
  filter(length.bin == '65-80',
         sex == 'F',
         mean.sp.bl <= 8) %>% 
  select(mean.sp.bl)


xl <- sb.speed %>% 
  filter(length.bin == '>80',
         mean.sp.bl <= 8) %>% 
  select(mean.sp.bl)

xl.spawn <- sb.speed %>% 
  filter(length.bin == '>80',
         phase == 'spawning',
         mean.sp.bl <= 8) %>% 
  select(mean.sp.bl)

xl.run <- sb.speed %>% 
  filter(length.bin == '>80',
         phase == 'running',
         mean.sp.bl <= 8) %>% 
  select(mean.sp.bl)

xl.male <- sb.speed %>% 
  filter(length.bin == '>80',
         sex == 'M',
         mean.sp.bl <= 8) %>% 
  select(mean.sp.bl)

xl.female <- sb.speed %>% 
  filter(length.bin == '>80',
         sex == 'F',
         mean.sp.bl <= 8) %>% 
  select(mean.sp.bl)

male <- sb.speed %>% 
  filter(sex == 'M',
         mean.sp.bl <= 8) %>% 
  select(mean.sp.bl)

male.spawn <- sb.speed %>% 
  filter(sex == 'M',
         phase == 'spawning',
         mean.sp.bl <= 8) %>% 
  select(mean.sp.bl)

male.run <- sb.speed %>% 
  filter(sex == 'M',
         phase == 'running',
         mean.sp.bl <= 8) %>% 
  select(mean.sp.bl)

female <-  sb.speed %>% 
  filter(sex == 'F',
         mean.sp.bl <= 8) %>% 
  select(mean.sp.bl)

female.spawn <-  sb.speed %>% 
  filter(sex == 'F',
         phase == 'spawning',
         mean.sp.bl <= 8) %>% 
  select(mean.sp.bl)

female.run <-  sb.speed %>% 
  filter(sex == 'F',
         phase == 'running',
         mean.sp.bl <= 8) %>% 
  select(mean.sp.bl)

run.15 <- sb.speed %>% 
  filter(phase == 'running',
         mean.sp.bl <= 8) %>% 
  sample_frac(0.15) %>% 
  select(mean.sp.bl)


param <- function(data){
  list(a = min(data), b = max(data), n = length(data))
}

Data <- sp.pot$mean.sp.bl
abn <- param(Data)


### PL Neg-loglikelihood -------------------------------------------------------
## Analytical solution (Box 1 of Edwards et al. 2007):
pl_muanal <- 1/(-log(abn$a) + sum(log(Data)) / abn$n) + 1

## Numerical solution, check to make sure this matches with analytical
mu.start <- ifelse(pl_muanal - 0.1 < 1.05, 1.05, pl_muanal - 0.1)
pl_out <- nlm(pl_nll, mu.start, data = Data, n = abn$n, a = abn$a)
# µ estimate is pl_out$estimate
# Negative log likelihood is pl_out$minimum


## Normalization constant in probability density function
# See equation (3) in Edwards et al. (2007)
C <- (pl_out$estimate - 1) * abn$a ^ (pl_out$estimate - 1)

## AIC and BIC calculation
# Estimated two parameters (µ and a) for unbounded tail
n.param <- 2
AIC <- 2 * pl_out$minimum + 2 * n.param
BIC <- 2 * pl_out$minimum + n.param * log(abn$n) # From B&A p286

## Calculation of 95% CI of MLE µ:
# Range of µ to try, can't be <= 1
muvec <- seq(ifelse(pl_out$estimate - 0.4 <= 1, 1.01, pl_out$estimate - 0.4),
             pl_out$estimate + 0.4, 0.0001)

pl_muvary <- NULL # negative log-likelihood or each µ value
for(i in 1:length(muvec)){
  pl_muvary[i] = pl_nll(muvec[i], data = Data, n = abn$n, a = abn$a)
}

critval <- pl_out$minimum + qchisq(0.95, 1) / 2  
                  # 1 dof. Hilborn and Mangel (1997) p163.
# # Visually
# plot(muvec, pl_muvary)
# abline(h = critval, col = "red")

# Put things together
pl <- list(mu = pl_out$estimate,
           ci.95 = c(min(muvec[pl_muvary < critval]),
                     max(muvec[pl_muvary < critval])),
           aic = AIC,
           bic = BIC,
           norm.const = C)

rm(mu.start, pl_out, C, n.param, AIC, BIC, muvec, pl_muvary, i, critval)


### Exp Neg-loglikelihood ------------------------------------------------------
## Analytical solution (Box 1, Edwards et al. 2007)
exp_lamb_anal <-  1 / (sum(Data) / abn$n - abn$a)    # MLE for lambda

## Numerical solution, check to make sure this matches analytical
lambda.start <- 0.05
exp_out <- nlm(exp_nll, lambda.start, data = Data, n = abn$n, a = abn$a)


## Normalization constant in probability density function
C <- exp_out$estimate * exp(exp_out$estimate * abn$a)

## AIC and BIC calculation
# Estimated two parameters (lambda and a) for unbounded tail
n.param <- 2
AIC <- 2 * exp_out$minimum + 2 * n.param   
BIC <- 2 * exp_out$minimum + n.param * log(abn$n) # B&A p286


## Calculation of 95% CI of MLE lambda: 
lambdavec <- seq(round(0.5 * exp_out$estimate, digits=5), 2 * exp_out$estimate,
                 0.00001)

exp_lambdavary <- NULL
for(i in 1:length(lambdavec)){
  exp_lambdavary[i] = exp_nll(lambdavec[i], data = Data,
                              n = abn$n, a = abn$a)
}

critval <- exp_out$minimum + qchisq(0.95, 1) / 2
                 # 1 dof. Hilborn and Mangel (1997) p163.

# # Visually
# plot(lambdavec, exp_lambdavary)
# abline(h = critval, col = "red")

# Put things together
exp <- list(lambda = exp_out$estimate,
            ci.95 = c(min(lambdavec[exp_lambdavary < critval]),
                      max(lambdavec[exp_lambdavary < critval])),
            aic = AIC,
            bic = BIC,
            norm.const = C)

rm(lambda.start, exp_out, C, n.param, AIC, BIC, lambdavec,
   exp_lambdavary, i, critval)


### PLB Neg-loglikelihood ------------------------------------------------------
mu.start <- 1.01
plb_out <- nlm(plb_nll, mu.start, data = Data,
               n = abn$n, a = abn$a, b = abn$b)


## Normalization constant in probability density function
C <- (plb_out$estimate - 1) / (abn$a ^ (1 - plb_out$estimate) -
                                    abn$b ^ (1 - plb_out$estimate))

# Estimated three parameters (µ, a, and b) for unbounded tail
n.param <- 3
AIC <- 2 * plb_out$minimum + 2 * n.param
BIC <- 2 * plb_out$minimum + n.param * log(abn$n) # B&A p286


## Calculation of 95% CI of MLE µ:
# Range of mu to try for PLB model, can't be <= 1
muvec <- seq(plb_out$estimate - 0.2, plb_out$estimate + 0.2, 0.0001)

plb_muvary <- NULL
for(i in 1:length(muvec)){
  plb_muvary[i] <- plb_nll(muvec[i], data = Data,
                           n = abn$n, a = abn$a, b = abn$b)
}

critval <- plb_out$minimum + qchisq(0.95 , 1) / 2
                    # 1 dof. Hilborn and Mangel (1997) p163.

# # Visually:
# plot(muvec, plb_muvary)
# abline(h = critval, col = "red")

# Put things together
plb <- list(mu = plb_out$estimate,
            ci.95 = c(min(muvec[plb_muvary < critval]),
                      max(muvec[plb_muvary < critval])),
            aic = AIC,
            bic = BIC,
            norm.const = C)

rm(mu.start, plb_out, C, n.param, AIC, BIC, muvec, plb_muvary, i, critval)


### ExpB Neg-loglikelihood ------------------------------------------------------
lambda.start <- 0.1
expb_out <- nlm(expb_nll, lambda.start, data = Data,
                n = abn$n, a = abn$a, b = abn$b)


## Normalization constant in probability density function
C <- expb_out$estimate / (exp(-expb_out$estimate * abn$a) -
                                exp(-expb_out$estimate * abn$b))

## AIC and BIC calculation
# Estimated three parameters (lambda, a, and b)
n.param <- 3
AIC <- 2 * expb_out$minimum + 2 * n.param
BIC <- 2 * expb_out$minimum + n.param * log(abn$n) # From B&A p286


## Calculation of 95% CI of MLE lambda: 
lambdavec <- seq(round(0.5*expb_out$estimate, digits=5), 2 * expb_out$estimate,
                 0.00001)

expb_lambdavary = NULL
for(i in 1:length(lambdavec)){
  expb_lambdavary[i] = expb_nll(lambdavec[i], data = Data,
                                n = abn$n, a = abn$a, b = abn$b)
}

critval = expb_out$minimum + qchisq(0.95 , 1) / 2
                # 1 dof. Hilborn and Mangel (1997) p163.

# # Visually
# plot(lambdavec, expb_lambdavary)
# abline(h = critval, col = "red")

# Put things together
expb <- list(lambda = expb_out$estimate,
             ci.95 = c(min(lambdavec[expb_lambdavary < critval]),
                       max(lambdavec[expb_lambdavary < critval])),
             aic = AIC,
             bic = BIC,
             norm.const = C)

rm(lambda.start, expb_out, C, n.param, AIC, BIC, lambdavec,
   expb_lambdavary, i, critval)