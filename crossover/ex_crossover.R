## Package Sample size

if(!require(ggplot2)) install.packages("ggplot2")
if(!require(survival)) install.packages("survival")
if(!require(trtswitch)) install.packages("trtswitch")
if(!require(rpsftm)) install.packages("rpsftm")

# Look at the documentation of the functions if necessary. You can use the ? operator for this (e.g., ?tssim).
# If you get stuck on a question, you can always ask or look at the provided solution.
# It is not planned that all exercises are completed within the given 30 minutes.

# First simulate some data with treatment switching
df_long <- tssim(
  tdxo = 1, coxo = 1, allocation1 = 1, allocation2 = 1,
  p_X_1 = 0.3, p_X_0 = 0.3,
  rate_T = 0.002, beta1 = -0.4, beta2 = 0.3,
  gamma0 = 0.3, gamma1 = -0.9, gamma2 = 0.7, gamma3 = 1.1, gamma4 = -0.8,
  zeta0 = -3.5, zeta1 = 0.5, zeta2 = 0.2, zeta3 = -0.4,
  alpha0 = 0.5, alpha1 = 0.5, alpha2 = 0.4,
  theta1_1 = -0.4, theta1_0 = -0.4, theta2 = 0,
  rate_C = 0.0000855, accrualIntensity = 20/30,
  plannedTime = 1350, fixedFollowup = F, days = 30,
  n = 500, NSim = 1, seed = 54321)[[1]]


df <- df_long
df$A <- df$A + df$trtrand

df <- df[order(df$id, df$tstop), ]

unique_ids <- unique(df$id)

df_short <- data.frame(
  id          = unique_ids,
  time        = tapply(df$timeOS, df$id, function(x) x[1]), # Observed time to event or censoring.
  event       = tapply(df$died, df$id, function(x) x[1]), # Indicator if an event was observed or not (1 for event)
  treat       = tapply(df$trtrand, df$id, function(x) x[1]), # Randomized treatment assignment (0 = control, 1 = experimental)
  rx          = tapply(df$A, df$id, mean), # Percentage of time spend on treatment (1 for patients that where assigned the experimental treatment)
  censor_time = tapply(df$censor_time, df$id, function(x) x[1]), # Administrative censoring time.
  base_cov    = tapply(df$bprog, df$id, function(x) x[1]) # Baseline prognosis (possible confounder)
)



# 1.1
# First, take a look at the function tssim, which we will use to simulate some data.
# There are a lot of parameters, you don't have to understand all of them (in the documentation the formulas for the simulations are given)
# Importantly the parameter beta1 controls the true treatment effect. 
# With exp(beta1) being the Hazard ratio (HR) between the two treatment arms
#
# What is the true HR?
# What are the output variables?
#
# Additionally, a dataset df_short is created - what are its variables?


# 1.2 ITT
# Now fit a Cox proportional hazards model using coxph.
# Fit the survival time against the assigned treatment.
# Use the df_short dataset.
# What is the estimated HR (exp(coef))?


# 1.3 RPSFT
# Now fit a RPSFT model.
# Use the rpsftm function from the rpsftm package (since trtswitch also contains a function with the same name).
# You can specify this by calling rpsftm::rpsftm.
# Use the df_short dataset.
# What is its estimated HR (exp(psi))?


# 1.4 IPCW
# Now fit a IPCW model using the ipcw function.
# What is it's estimated HR?

# Here is the function you can use, just fill in the correct values in the ""
fit_ipcw <- ipcw(
  df_long,
  id = "",
  tstart = "",
  tstop = "",
  event = "",
  treat = "",
  swtrt = "",
  swtrt_time = "",
  base_cov = "",
  numerator = "",
  denominator = "",
  logistic_switching_model = T,
  swtrt_control_only = TRUE,
  boot = F)


fit_ipcw$hr
fit_ipcw$hr_CI



# 1.5
# Compare the estimated HR of all three models and compare it with the true HR of ~0.67.
# What do you noticed? 


# 1.6 RPSFT-psi
# Now we generate data with way much stronger treatment effect.
# For this new data set fit a rpsft model again. You might get an warning.
# Change the psi interval. Also check the evaluated psi values visually (So the psi and there corresponding Z-values)
# Reminder: RPSFT chooses the psi with an evaluated test statistic of 0 (and +/- 1.96 for the CI)

df_long_strong <- tssim(
  tdxo = 1, coxo = 1, allocation1 = 1, allocation2 = 1,
  p_X_1 = 0.3, p_X_0 = 0.3,
  rate_T = 0.002, beta1 = -1.4, beta2 = 0.3,
  gamma0 = 0.3, gamma1 = -0.9, gamma2 = 0.7, gamma3 = 1.1, gamma4 = -0.8,
  zeta0 = -3.5, zeta1 = 0.5, zeta2 = 0.2, zeta3 = -0.4,
  alpha0 = 0.5, alpha1 = 0.5, alpha2 = 0.4,
  theta1_1 = -0.4, theta1_0 = -1.4, theta2 = 0,
  rate_C = 0.0000855, accrualIntensity = 20/30,
  plannedTime = 1350, fixedFollowup = F, days = 30,
  n = 500, NSim = 1, seed = 54321)[[1]]


df <- df_long_strong
df$A <- df$A + df$trtrand

df <- df[order(df$id, df$tstop), ]

unique_ids <- unique(df$id)

df_short_strong <- data.frame(
  id          = unique_ids,
  time        = tapply(df$timeOS, df$id, function(x) x[1]),
  event       = tapply(df$died, df$id, function(x) x[1]),
  treat       = tapply(df$trtrand, df$id, function(x) x[1]),
  rx          = tapply(df$A, df$id, mean),
  censor_time = tapply(df$censor_time, df$id, function(x) x[1]),
  base_cov    = tapply(df$bprog, df$id, function(x) x[1])
)



# 1.7
# Now we change the value theta1_0 in the tssim function
# What does it do?
# The assumption of what model is broken?


df_long_broken <- tssim(
  tdxo = 1, coxo = 1, allocation1 = 1, allocation2 = 1,
  p_X_1 = 0.3, p_X_0 = 0.3,
  rate_T = 0.002, beta1 = -0.4, beta2 = 0.3,
  gamma0 = 0.3, gamma1 = -0.9, gamma2 = 0.7, gamma3 = 1.1, gamma4 = -0.8,
  zeta0 = -3.5, zeta1 = 0.5, zeta2 = 0.2, zeta3 = -0.4,
  alpha0 = 0.5, alpha1 = 0.5, alpha2 = 0.4,
  theta1_1 = -0.4, theta1_0 = -0.2, theta2 = 0.2, # here a change was made
  rate_C = 0.0000855, accrualIntensity = 20/30,
  plannedTime = 1350, fixedFollowup = F, days = 30,
  n = 500, NSim = 1, seed = 54321)[[1]]

df <- df_long_broken
df$A <- df$A + df$trtrand

df <- df[order(df$id, df$tstop), ]

unique_ids <- unique(df$id)

df_short_broken <- data.frame(
  id          = unique_ids,
  time        = tapply(df$timeOS, df$id, function(x) x[1]),
  event       = tapply(df$died, df$id, function(x) x[1]),
  treat       = tapply(df$trtrand, df$id, function(x) x[1]),
  rx          = tapply(df$A, df$id, mean),
  censor_time = tapply(df$censor_time, df$id, function(x) x[1]),
  base_cov    = tapply(df$bprog, df$id, function(x) x[1])
)

# 1.8
# Now fit all 3 models to the new data. What can you notice about the diffrent HR?
