##### No censoring #####

### Exponential model

data_Exp <- function(sample_size, lambda0)
{
  Ti <- rexp(n = sample_size, rate = lambda0)
  
  return(Ti)
}

data_ex <- data_Exp(sample_size = 100, lambda0 = 0.5)
hist(data_ex)
summary(data_ex)

# Maximum likelihood estimation

log_likelihood_Exp <- function(data, lambda)
{
  ll <- sum(dexp(data, rate = lambda, log = TRUE))
  
  return(ll)
}

MLE_Exp <- function(data, lambda_initial)
{
  nll <- function(lambda)
  {
    value <- -log_likelihood_Exp(data = data, lambda = lambda)
    
    return(value)
  }
  
  lambda_hat <- nlminb(start = lambda_initial,
                       objective = nll)$par
  
  return(lambda_hat)
}

MLE_Exp(data = data_Exp(sample_size = 100, lambda0 = 0.5),
        lambda_initial = 0.5)

# Monte Carlo simulations

lambda_hat_MC <- rep(0, length = 1000)
for (sn in 1:1000)
{
  set.seed(sn + 12345)
  lambda_hat_MC[sn] <- MLE_Exp(
    data = data_Exp(sample_size = 100, lambda0 = 0.5),
    lambda_initial = 0.5)
}
hist(lambda_hat_MC)
summary(lambda_hat_MC)

##### Right-censoring #####

### Exponential model

data_Exp_RC <- function(sample_size, lambda0, c0, lambda0_C)
{
  Ti <- rexp(n = sample_size, rate = lambda0)
  Ci <- c0 + rexp(n = sample_size, rate = lambda0_C)
  Yi <- pmin(Ti, Ci)
  Di <- (Ti <= Ci)
  
  data_RC <- data.frame(Yi = Yi,
                        Di = Di,
                        Ti = Ti,
                        Ci = Ci)
  
  return(data_RC)
}

data_ex <- data_Exp_RC(
  sample_size = 100,
  lambda0 = 0.5,
  c0 = 0.2,
  lambda0_C = 0.2)
mean(data_ex$Di)

# Biased MLE

data_ex <- data_Exp_RC(
  sample_size = 100,
  lambda0 = 0.5,
  c0 = 0.2,
  lambda0_C = 0.2)

MLE_Exp(
  data = data_ex$Yi,
  lambda_initial = 0.5)

MLE_Exp(
  data = data_ex$Yi[data_ex$Di == 1],
  lambda_initial = 0.5)

# Monte Carlo simulations

lambda_hat_bias1_MC <- rep(0, length = 1000)
lambda_hat_bias2_MC <- rep(0, length = 1000)
for (sn in 1:1000)
{
  set.seed(sn + 12345)
  data_ex <- data_Exp_RC(
    sample_size = 100,
    lambda0 = 0.5,
    c0 = 0.2,
    lambda0_C = 0.2)
  
  lambda_hat_bias1_MC[sn] <- MLE_Exp(
    data = data_ex$Yi,
    lambda_initial = 0.5)
  
  lambda_hat_bias2_MC[sn] <- MLE_Exp(
    data = data_ex$Yi[data_ex$Di == 1],
    lambda_initial = 0.5)
}
hist(lambda_hat_bias1_MC)
summary(lambda_hat_bias1_MC)
hist(lambda_hat_bias2_MC)
summary(lambda_hat_bias2_MC)

# Correct MLE

log_likelihood_Exp_RC <- function(data, lambda)
{
  ll <- sum(
    data$Di * dexp(data$Yi,
                   rate = lambda,
                   log = TRUE) +
      (1 - data$Di) * pexp(data$Yi,
                           rate = lambda,
                           lower.tail = FALSE,
                           log.p = TRUE)
  )
  
  return(ll)
}

MLE_Exp_RC <- function(data, lambda_initial)
{
  nll <- function(lambda)
  {
    value <- -log_likelihood_Exp_RC(data = data, lambda = lambda)
    
    return(value)
  }
  
  lambda_hat <- nlminb(start = lambda_initial,
                       objective = nll)$par
  
  return(lambda_hat)
}

MLE_Exp_RC(data = data_Exp_RC(sample_size = 100,
                              lambda0 = 0.5,
                              c0 = 0.2,
                              lambda0_C = 0.2),
           lambda_initial = 0.5)

# Monte Carlo simulations

lambda_hat_MC <- rep(0, length = 1000)
for (sn in 1:1000)
{
  set.seed(sn + 12345)
  lambda_hat_MC[sn] <- MLE_Exp_RC(
    data = data_Exp_RC(sample_size = 100,
                       lambda0 = 0.5,
                       c0 = 0.2,
                       lambda0_C = 0.2),
    lambda_initial = 0.5)
}
hist(lambda_hat_MC)
summary(lambda_hat_MC)

### Kaplan-Meier estimator

KaplanMeier <- function(data)
{
  t_points <- sort(unique(data$Yi[data$Di == 1]))
  failure_t <- colSums(outer(data$Yi, t_points, FUN = "==") * data$Di)
  at_risk <- colSums(outer(data$Yi, t_points, FUN = ">="))
  hazard <- failure_t / at_risk
  survival <- cumprod(1 - hazard)
  
  return(data.frame(time = c(0, t_points),
                    hazard = c(0, hazard),
                    survival = c(1, survival)))
}

test <- KaplanMeier(data_Exp_RC(sample_size = 100,
                                lambda0 = 0.5,
                                c0 = 0.2,
                                lambda0_C = 0.2))
test
plot(test$time, test$survival, type = 's',
     xlab = "time", ylab = "survival probability")
t_grids <- seq(0, 7, 0.01)
surv0_t_grids <- pexp(t_grids,
                      rate = 0.5,
                      lower.tail = FALSE,
                      log.p = FALSE)
lines(t_grids, surv0_t_grids, col = 2)










