walk.model <- function(data){
  # Get parameter values
  abn <- list(a = min(data), b = max(data), n = length(data))
  
  
  ### Negative log-likelihood equations
  # Unbounded power law (infinite tail); equation (5) in Edwards et al. (2007)
  pl_nll <- function(mu, input, n, a){
    if(mu <= 1){
      stop(
        "WARNING: Cannot have mu less than or equal to 1 for pow law over inf range."
      ) # Change starting value if this appears
    }
    else{
      -(n * log(mu - 1) + n *(mu - 1) * log(a) - mu * sum(log(input)))
    }
  }
  
  
  # Unbounded exponential (infinite tail); equation (6) in Edwards et al. (2007)
  exp_nll <- function(lambda, input, n, a){
    -(n * log(lambda) + n * lambda * a - lambda * sum(input))
  }
  
  
  # Bounded power law
  plb_nll <- function(mu, input, n, a, b){
    if(mu == 1){
      # equation (A.25) in Edwards (2011)
      -(-n * log(log(b / a)) - sum(log(input)))
    }
    else{
      # equation (A.23) in Edwards (2011)
      -(n * log((mu - 1) / (a ^ (1 - mu) - b ^ (1 - mu))) - mu * sum(log(input)))
    }
  }
  
  
  # Bounded exponential; equation (A.27) in Edwards (2011)
  expb_nll <- function(lambda, input, n, a, b){
    -(n * log(abs(lambda)) - n * log(abs(exp(-lambda * a) - exp(-lambda * b))) -
        lambda * sum(input))
  }
  
  
  ### PL Neg-loglikelihood
  ## Analytical solution (Box 1 of Edwards et al. 2007):
  pl_muanal <- 1/(-log(abn$a) + sum(log(data)) / abn$n) + 1
  
  ## Numerical solution, check to make sure this matches with analytical
  mu.start <- ifelse(pl_muanal - 0.1 < 1.05, 1.05, pl_muanal - 0.1)
  pl_out <- nlm(pl_nll, mu.start, input = data, n = abn$n, a = abn$a)
  
  if(abs(pl_out - pl_muanal) > 10 ^ (-5)){
    stop("Analytical and numerical disagree for PL model.")
  }
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
    pl_muvary[i] = pl_nll(muvec[i], input = data, n = abn$n, a = abn$a)
  }
  
  critval <- pl_out$minimum + qchisq(0.95, 1) / 2 # 1 DF, Hilborn and Mangel (1997) p163
  
  # Put things together
  pl <- data.frame(model = 'PL',
                   param = 'mu')
  
  
  pl <- list(mu = pl_out$estimate,
             ci.95 = c(min(muvec[pl_muvary < critval]),
                       max(muvec[pl_muvary < critval])),
             aic = AIC,
             bic = BIC,
             norm.const = C)
  
  rm(mu.start, pl_out, C, n.param, AIC, BIC, muvec, pl_muvary, i, critval)
}
