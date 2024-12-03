rm(list = ls())

library(tidyverse)
library(rjags)

# Start with CRD ####

# RCP 4.5 ####


# Air and water temperature data: CR loccation, 4.5 scenario
CR_Temps <- read.csv("CR_45SNAPForecast.csv", header=TRUE)

# Air temperature variables
CR_AirTemp45 <- CR_Temps %>%
  select(date, Airport, Air_MonthAvg, std, precision, Region)

# Water temperature variables
CR_WaterTemp45 <- CR_Temps %>%
  select(date, BVS, CAB, TIN, SQR, CAN, EYS, TIS, WDD, RHM)

dlm_pooled <- "
model{
#### Priors
for(p in 1:np){
x[1,p] ~ dnorm(x_ic, tau_ic) # Initial condition of water temperature
}
tau_obs ~ dgamma(a_obs, r_obs) # Prior on observation error
tau_add ~ dgamma(a_add, r_add) # Prior on process error

#### Fixed Effects
beta ~ dmnorm(mu_beta, tau_beta) # Prior on beta coefficients

for(p in 1:np){
int[p] ~ dnorm(mu_int, tau_int) # Prior on pond-specific intercepts
}

#### Data Model
for(t in 1:n){ # loop over all time steps
for(p in 1:np){
OBS[t,p] ~ dnorm(x[t,p], tau_obs) # Observed water temperature is drawn from latent air temperature with observation uncertainty
}
Xf[t] ~ dnorm(muAirTemp[t], tauAirTemp[t]) # Latent air temperature is drawn from mean and precision of forecasated air temperature
}

#### Process Model
for(t in 2:n){ # loop over all time steps except teh first (we defined ic above)
for(p in 1:np){
mu[t,p] <- x[t-1,p] + int[p] + beta[1] * x[t-1,p] + beta[2] * Xf[t] # Mean water temperature is a function of the previous time step and current air temperature
x[t,p] ~ dnorm(mu[t,p], tau_add) # Latent water temperature is drawn from mean water temperature with process uncertainty
}
}
}
"

## Define data

# Empty list
data <- list()
# Water temperature observations
data$OBS <- dplyr::select(CR_WaterTemp45, -date)
# Number of time steps
data$n <- nrow(data$OBS)
# Number of ponds
data$np <- ncol(data$OBS)
# Initial water temperature mean
data$x_ic <- 0.1
# Initial water temperature precision
data$tau_ic = 0.1
# Prior parameters for observation and process uncertainty
data$a_obs = 1
data$r_obs = 1
data$a_add = 1
data$r_add = 1
# Prior parameters for beta coefficients
data$mu_beta <- c(0, 0)
data$tau_beta <- diag(x = c(0.001, 0.001), nrow = 2, ncol = 2)
# Prior parameters for intercept
data$mu_int <- 0
data$tau_int <- 0.001
# Mean air temperature estimate
data$muAirTemp <- CR_AirTemp45$Air_MonthAvg
# Air temperature precision
data$tauAirTemp <- CR_AirTemp45$precision

# Create JAGS model with 3 chains
jm <- jags.model(file = textConnection(dlm_pooled), data = data, n.chains = 3)

# Posterior samples of parameters
CRD45_out_params <- coda.samples(model = jm,
                               variable.names = c('beta', 'int',
                                                  'tau_add', 'tau_obs'),
                               n.iter = 100000, thin = 5)

# Posterior samples of response variables
CRD45_out_response <- coda.samples(model = jm,
                                 variable.names = c('x', 'OBS'),
                                 n.iter = 100000, thin = 5)

plot(CRD45_out_params)
gelman.diag(CRD45_out_params, confidence = 0.99)



# RCP 8.5 ####


# Air and water temperature data: CR loccation, 4.5 scenario
CR_Temps <- read.csv("CR_85SNAPForecast.csv", header=TRUE)

# Air temperature variables
CR_AirTemp85 <- CR_Temps %>%
  select(date, Airport, Air_MonthAvg, std, precision, Region)

# Water temperature variables
CR_WaterTemp85 <- CR_Temps %>%
  select(date, BVS, CAB, TIN, SQR, CAN, EYS, TIS, WDD, RHM)

dlm_pooled <- "
model{
#### Priors
for(p in 1:np){
x[1,p] ~ dnorm(x_ic, tau_ic) # Initial condition of water temperature
}
tau_obs ~ dgamma(a_obs, r_obs) # Prior on observation error
tau_add ~ dgamma(a_add, r_add) # Prior on process error

#### Fixed Effects
beta ~ dmnorm(mu_beta, tau_beta) # Prior on beta coefficients

for(p in 1:np){
int[p] ~ dnorm(mu_int, tau_int) # Prior on pond-specific intercepts
}

#### Data Model
for(t in 1:n){ # loop over all time steps
for(p in 1:np){
OBS[t,p] ~ dnorm(x[t,p], tau_obs) # Observed water temperature is drawn from latent air temperature with observation uncertainty
}
Xf[t] ~ dnorm(muAirTemp[t], tauAirTemp[t]) # Latent air temperature is drawn from mean and precision of forecasated air temperature
}

#### Process Model
for(t in 2:n){ # loop over all time steps except teh first (we defined ic above)
for(p in 1:np){
mu[t,p] <- x[t-1,p] + int[p] + beta[1] * x[t-1,p] + beta[2] * Xf[t] # Mean water temperature is a function of the previous time step and current air temperature
x[t,p] ~ dnorm(mu[t,p], tau_add) # Latent water temperature is drawn from mean water temperature with process uncertainty
}
}
}
"

## Define data

# Empty list
data <- list()
# Water temperature observations
data$OBS <- dplyr::select(CR_WaterTemp85, -date)
# Number of time steps
data$n <- nrow(data$OBS)
# Number of ponds
data$np <- ncol(data$OBS)
# Initial water temperature mean
data$x_ic <- 0.1
# Initial water temperature precision
data$tau_ic = 0.1
# Prior parameters for observation and process uncertainty
data$a_obs = 1
data$r_obs = 1
data$a_add = 1
data$r_add = 1
# Prior parameters for beta coefficients
data$mu_beta <- c(0, 0)
data$tau_beta <- diag(x = c(0.001, 0.001), nrow = 2, ncol = 2)
# Prior parameters for intercept
data$mu_int <- 0
data$tau_int <- 0.001
# Mean air temperature estimate
data$muAirTemp <- CR_AirTemp85$Air_MonthAvg
# Air temperature precision
data$tauAirTemp <- CR_AirTemp85$precision

# Create JAGS model with 3 chains
jm <- jags.model(file = textConnection(dlm_pooled), data = data, n.chains = 3)

# Posterior samples of parameters
CRD85_out_params <- coda.samples(model = jm,
                                 variable.names = c('beta', 'int',
                                                    'tau_add', 'tau_obs'),
                                 n.iter = 100000, thin = 5)

# Posterior samples of response variables
CRD85_out_response <- coda.samples(model = jm,
                                   variable.names = c('x', 'OBS'),
                                   n.iter = 100000, thin = 5)

plot(CRD85_out_params)
gelman.diag(CRD85_out_params, confidence = 0.99)




# Continue with YF ####

# RCP 4.5 ####


# Air and water temperature data: CR loccation, 4.5 scenario
YF_Temps <- read.csv("YF_45SNAPForecast.csv", header=TRUE)

# Air temperature variables
YF_AirTemp45 <- YF_Temps %>%
  select(date, Airport, Air_MonthAvg, std, precision, Region)

# Water temperature variables
YF_WaterTemp45 <- YF_Temps %>%
  select(date, )

dlm_pooled <- "
model{
#### Priors
for(p in 1:np){
x[1,p] ~ dnorm(x_ic, tau_ic) # Initial condition of water temperature
}
tau_obs ~ dgamma(a_obs, r_obs) # Prior on observation error
tau_add ~ dgamma(a_add, r_add) # Prior on process error

#### Fixed Effects
beta ~ dmnorm(mu_beta, tau_beta) # Prior on beta coefficients

for(p in 1:np){
int[p] ~ dnorm(mu_int, tau_int) # Prior on pond-specific intercepts
}

#### Data Model
for(t in 1:n){ # loop over all time steps
for(p in 1:np){
OBS[t,p] ~ dnorm(x[t,p], tau_obs) # Observed water temperature is drawn from latent air temperature with observation uncertainty
}
Xf[t] ~ dnorm(muAirTemp[t], tauAirTemp[t]) # Latent air temperature is drawn from mean and precision of forecasated air temperature
}

#### Process Model
for(t in 2:n){ # loop over all time steps except teh first (we defined ic above)
for(p in 1:np){
mu[t,p] <- x[t-1,p] + int[p] + beta[1] * x[t-1,p] + beta[2] * Xf[t] # Mean water temperature is a function of the previous time step and current air temperature
x[t,p] ~ dnorm(mu[t,p], tau_add) # Latent water temperature is drawn from mean water temperature with process uncertainty
}
}
}
"

## Define data

# Empty list
data <- list()
# Water temperature observations
data$OBS <- dplyr::select(YF_WaterTemp45, -date)
# Number of time steps
data$n <- nrow(data$OBS)
# Number of ponds
data$np <- ncol(data$OBS)
# Initial water temperature mean
data$x_ic <- 0.1
# Initial water temperature precision
data$tau_ic = 0.1
# Prior parameters for observation and process uncertainty
data$a_obs = 1
data$r_obs = 1
data$a_add = 1
data$r_add = 1
# Prior parameters for beta coefficients
data$mu_beta <- c(0, 0)
data$tau_beta <- diag(x = c(0.001, 0.001), nrow = 2, ncol = 2)
# Prior parameters for intercept
data$mu_int <- 0
data$tau_int <- 0.001
# Mean air temperature estimate
data$muAirTemp <- YF_AirTemp45$Air_MonthAvg
# Air temperature precision
data$tauAirTemp <- YF_AirTemp45$precision

# Create JAGS model with 3 chains
jm <- jags.model(file = textConnection(dlm_pooled), data = data, n.chains = 3)

YF45_out_params <- coda.samples(model = jm,
                                 variable.names = c('beta', 'int',
                                                    'tau_add', 'tau_obs'),
                                 n.iter = 100000, thin = 5)

# Posterior samples of response variables
YF45_out_response <- coda.samples(model = jm,
                                   variable.names = c('x', 'OBS'),
                                   n.iter = 100000, thin = 5)

plot(YF45_out_params)
gelman.diag(YF45_out_params, confidence = 0.99)



# RCP 8.5 ####


# Air and water temperature data: CR loccation, 4.5 scenario
CR_Temps <- read.csv("YF_85SNAPForecast.csv", header=TRUE)

# Air temperature variables
YF_AirTemp85 <- YF_Temps %>%
  select(date, Airport, Air_MonthAvg, std, precision, Region)

# Water temperature variables
YF_WaterTemp85 <- YF_Temps %>%
  select(date, )

dlm_pooled <- "
model{
#### Priors
for(p in 1:np){
x[1,p] ~ dnorm(x_ic, tau_ic) # Initial condition of water temperature
}
tau_obs ~ dgamma(a_obs, r_obs) # Prior on observation error
tau_add ~ dgamma(a_add, r_add) # Prior on process error

#### Fixed Effects
beta ~ dmnorm(mu_beta, tau_beta) # Prior on beta coefficients

for(p in 1:np){
int[p] ~ dnorm(mu_int, tau_int) # Prior on pond-specific intercepts
}

#### Data Model
for(t in 1:n){ # loop over all time steps
for(p in 1:np){
OBS[t,p] ~ dnorm(x[t,p], tau_obs) # Observed water temperature is drawn from latent air temperature with observation uncertainty
}
Xf[t] ~ dnorm(muAirTemp[t], tauAirTemp[t]) # Latent air temperature is drawn from mean and precision of forecasated air temperature
}

#### Process Model
for(t in 2:n){ # loop over all time steps except teh first (we defined ic above)
for(p in 1:np){
mu[t,p] <- x[t-1,p] + int[p] + beta[1] * x[t-1,p] + beta[2] * Xf[t] # Mean water temperature is a function of the previous time step and current air temperature
x[t,p] ~ dnorm(mu[t,p], tau_add) # Latent water temperature is drawn from mean water temperature with process uncertainty
}
}
}
"

## Define data

# Empty list
data <- list()
# Water temperature observations
data$OBS <- dplyr::select(YF_WaterTemp85, -date)
# Number of time steps
data$n <- nrow(data$OBS)
# Number of ponds
data$np <- ncol(data$OBS)
# Initial water temperature mean
data$x_ic <- 0.1
# Initial water temperature precision
data$tau_ic = 0.1
# Prior parameters for observation and process uncertainty
data$a_obs = 1
data$r_obs = 1
data$a_add = 1
data$r_add = 1
# Prior parameters for beta coefficients
data$mu_beta <- c(0, 0)
data$tau_beta <- diag(x = c(0.001, 0.001), nrow = 2, ncol = 2)
# Prior parameters for intercept
data$mu_int <- 0
data$tau_int <- 0.001
# Mean air temperature estimate
data$muAirTemp <- YF_AirTemp85$Air_MonthAvg
# Air temperature precision
data$tauAirTemp <- YF_AirTemp85$precision

# Create JAGS model with 3 chains
jm <- jags.model(file = textConnection(dlm_pooled), data = data, n.chains = 3)

# Posterior samples of parameters
YF85_out_params <- coda.samples(model = jm,
                                 variable.names = c('beta', 'int',
                                                    'tau_add', 'tau_obs'),
                                 n.iter = 100000, thin = 5)

# Posterior samples of response variables
YF85_out_response <- coda.samples(model = jm,
                                   variable.names = c('x', 'OBS'),
                                   n.iter = 100000, thin = 5)

plot(YF85_out_params)
gelman.diag(YF85_out_params, confidence = 0.99)
