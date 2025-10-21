## Package Sample size

if(!require(ggplot2)) install.packages("ggplot2")
if(!require(rpact)) install.packages("rpact")


# Look at the documentation of the functions if necessary. You can use the ? operator for this (e.g., ?pnorm).
# If you get stuck on a question, you can always ask or look at the provided solution.
# It is not planned that all exercises are completed within the given 30 minutes.

# 1 rpact

# 1.1
# Use getDesignGroupSequential to specify a trial design.
# Set a one-sided test with an alpha level of 2.5%. The power should be 80%.
# We will not perform any interim analysis yet, so set kMax to 1.

# Sol:
design_standard <- getDesignGroupSequential(
  sided = 1,        # one-sided
  alpha = 0.025,    # 2.5% one-sided alpha
  beta  = 0.20,     # 1 - power 
  kMax  = 1         # no interim analyses
)
summary(design_standard)


# 1.2
# With this object, use getSampleSizeSurvival to determine the number of patients that need to be enrolled.
# A lambda of 0.064 is assumed for the treatment group, and a lambda of 0.089 for the control group.
# A dropout rate of 15% per year is expected.
# Additionally, a constant enrollment over 6 months is assumed.
# How many patients need to be enrolled to achieve a total study duration (from first randomization to final analysis) of 1 year?
# At how many events is the final analysis performed?


# Sol:
sampleSize <- getSampleSizeSurvival(
  design = design_standard,
  lambda1 = 0.064,                 # treatment hazard
  lambda2 = 0.089,                 # control hazard
  dropoutRate1 = 0.15,             # 15% per year dropout 
  dropoutRate2 = 0.15,
  dropoutTime  = 12,               # 12 months = 1 year for the 15% year dropout
  accrualTime = 6,                 # constant enrollment over 6 months
  followUpTime = 6
)
summary(sampleSize)

ceiling(sampleSize$maxNumberOfSubjects)    # patients to enroll
# 622
ceiling(sampleSize$maxNumberOfEvents)     # events at final analysis
# 289


# 1.3
# If you did everything correctly, you should have gotten:
# Number of patients = 621.9
# Number of events = 288.7
# What is the study duration if you enroll only 400 patients?
# Keep all other parameters the same.

# Sol:
sampleSize_fixPat <- getSampleSizeSurvival(
  design = design_standard,
  lambda1 = 0.064,
  lambda2  = 0.089,
  dropoutRate1 = 0.15,
  dropoutRate2 = 0.15,
  dropoutTime = 12,
  accrualTime = c(0,6),
  maxNumberOfSubjects = 400
)
summary(sampleSize_fixPat)

# 1.4
# Because dropout rates are often underestimated, let’s try some higher rates.
# What is the study duration when the yearly dropout rate is 20% and 25%?

# Sol:
sampleSize_fixPat_20 <- getSampleSizeSurvival(
  design = design_standard,
  lambda1 = 0.064,
  lambda2  = 0.089,
  dropoutRate1 = 0.2,
  dropoutRate2 = 0.2,
  dropoutTime = 12,
  accrualTime = c(0,6),
  maxNumberOfSubjects = 400
)
summary(sampleSize_fixPat_20)

sampleSize_fixPat_25 <- getSampleSizeSurvival(
  design = design_standard,
  lambda1 = 0.064,
  lambda2  = 0.089,
  dropoutRate1 = 0.25,
  dropoutRate2 = 0.25,
  dropoutTime = 12,
  accrualTime = c(0,6),
  maxNumberOfSubjects = 400
)
summary(sampleSize_fixPat_25)


# 1.5
# Now assume a worse treatment effect with a lambda of 0.06942.
# Keep the number of events the same as in 1.3 (i.e., 289).
# What is the power?
# Assume a dropout rate of 15% again and 400 patients.
# Use the getPowerSurvival function (make sure to set directionUpper to the correct value).

# Sol:
power_worse_effect <- getPowerSurvival(
  design = design_standard,
  directionUpper = F,
  lambda1 = 0.06942,
  lambda2  = 0.089,
  dropoutRate1 = 0.15,
  dropoutRate2 = 0.15,
  dropoutTime = 12,
  accrualTime = c(0,6),
  maxNumberOfSubjects = 400,
  maxNumberOfEvents = 289
)
summary(power_worse_effect)

# 1.6
# What is the number of events needed to achieve at least 65% power?
# You need to specify a new design and set the number of patients to 450.

# Sol:
design_P65 <- getDesignGroupSequential(
  kMax = 1,
  alpha = 0.025,
  beta = 0.35,
  sided = 1
)
summary(design_P65)


sampleSize_fixPat_P65 <- getSampleSizeSurvival(
  design = design_P65,
  lambda1 = 0.06942,
  lambda2  = 0.089,
  dropoutRate1 = 0.15,
  dropoutRate2 = 0.15,
  dropoutTime = 12,
  accrualTime = c(0,6),
  maxNumberOfSubjects = 450
)
summary(sampleSize_fixPat_P65)

# 1.7
# You decided to set the analysis time point at the number of events calculated in 1.6.
# What power do you get when the lambda is 0.064 again?

# Sol:
power_standard_effect <- getPowerSurvival(
  design = design_standard,
  directionUpper = F,
  lambda1 = 0.064,
  lambda2  = 0.089,
  dropoutRate1 = 0.15,
  dropoutRate2 = 0.15,
  dropoutTime = 12,
  accrualTime = c(0,6),
  maxNumberOfSubjects = 450,
  maxNumberOfEvents = 363
)
summary(power_standard_effect)

# 1.8
# Now let’s create a design with an interim analysis. Choose the O’Brien & Fleming type alpha spending.
# Define one interim analysis at 70% of the events (information rate).
# Define a futility bound at a z-value of 0. Make sure it is not binding.

# Use the summary function on the design.
# What is the:
# – Inflation factor?
# – Power at the interim analysis?
# Sol:
design_IA <- getDesignGroupSequential(
  kMax = 2,
  alpha = 0.025,
  beta = 0.2,
  sided = 1,
  typeOfDesign = "asOF", informationRates = c( 0.70, 1),
  futilityBounds = 0,
  bindingFutility = F
)
summary(design_IA)



# 1.9
# Now perform the same sample size calculation as in 1.3, but using the new design.
#
# Compare the results:
# – How does the number of events differ?
# – How does the final analysis time differ?
# – What is the "Expected study duration under H1"? Is this the duration my trial will last if H1 is true?
# – What is the "Expected number of events under H0"? (This is not part of the summary, but it is included in the object — you can use print to see it.)


# Sol:
sampleSize_fixPat_IA <- getSampleSizeSurvival(
  design = design_IA,
  lambda1 = 0.064,
  lambda2  = 0.089,
  dropoutRate1 = 0.15,
  dropoutRate2 = 0.15,
  dropoutTime = 12,
  accrualTime = c(0,6),
  maxNumberOfSubjects = 400
)
summary(sampleSize_fixPat_IA)
summary(sampleSize_fixPat)


# 1.10
# The interim analysis from 1.9 seems to work well.
# With only a small increase in the required number of events, the expected number of events was reduced significantly.
# You can improve this further, but it depends on your goal.
# However, you can also make it worse - for example, by placing interim analyses too early or too close to each other.
# Try out some designs to see how “bad” it can get, so you develop an intuition for what to avoid.

# Sol:

# Possible examples
design_example <- getDesignGroupSequential(
  kMax = 2,
  alpha = 0.025,
  beta = 0.2,
  sided = 1,
  typeOfDesign = "asOF", informationRates = c( 0.10, 1),
  futilityBounds = 0,
  bindingFutility = F
)
summary(design_example)

sampleSize_fixPat_IA_example <- getSampleSizeSurvival(
  design = design_example,
  lambda1 = 0.064,
  lambda2  = 0.089,
  dropoutRate1 = 0.15,
  dropoutRate2 = 0.15,
  dropoutTime = 12,
  accrualTime = c(0,6),
  maxNumberOfSubjects = 400
)
summary(sampleSize_fixPat_IA_example)
summary(sampleSize_fixPat)



design_example_2 <- getDesignGroupSequential(
  kMax = 4,
  alpha = 0.025,
  beta = 0.2,
  sided = 1,
  typeOfDesign = "asOF", informationRates = c( 0.70,0.75,0.80, 1),
  futilityBounds = c(0,0,0),
  bindingFutility = F
)
summary(design_example_2)
summary(design_IA)

sampleSize_fixPat_IA_example_2 <- getSampleSizeSurvival(
  design = design_example_2,
  lambda1 = 0.064,
  lambda2  = 0.089,
  dropoutRate1 = 0.15,
  dropoutRate2 = 0.15,
  dropoutTime = 12,
  accrualTime = c(0,6),
  maxNumberOfSubjects = 500
)
summary(sampleSize_fixPat_IA_example_2)
summary(sampleSize_fixPat_IA)



# Optional
# 2. GO/NO-GO
# Remember that log(HR) ~ N(log(HR), 4/d), with d being the number of events.

# 2.1
# Let's assume an event size of 80.
# The GO interval for the HR is (0, 0.8], and the NO-GO interval is (0.8, ∞).
# Calculate the GO and NO-GO probabilities for a true HR of 0.65.

# Sol:

d <-  80
HR_log <- log(0.65)

# GO Probability
pnorm(log(0.8), mean = HR_log, sd = sqrt(4/d))

# NO-GO Probability
1-pnorm(log(0.8), mean = HR_log, sd = sqrt(4/d))


# 2.2
# Add the consideration zone from [0.8, 0.9].
# So the GO interval is (0, 0.8), the consideration zone is [0.8, 0.9], and the NO-GO interval is (0.9, ∞).
# Calculate the probabilities for all three intervals.

# Sol:

d = 80
HR_log <- log(0.65)

# GO Probability
pnorm(log(0.8), mean = HR_log, sd = sqrt(4/d))

# NO-GO Probability
1-pnorm(log(0.9), mean = HR_log, sd = sqrt(4/d))

# Consideration zone Probability
pnorm(log(0.9), mean = HR_log, sd = sqrt(4/d)) - pnorm(log(0.8), mean = HR_log, sd = sqrt(4/d))

# 2.3 
# Now plot the probabilities for the three intervals as a function of the event size d.
# The x-axis should represent the event size d, and the y-axis the probability. Plot this for HR values of 0.69 and 0.65.
# You might find ggplot2::geom_function helpful.

# Sol:

# GO / Consideration / NO-GO as functions of event size d
GO_fun <- function(d, HR_true) {
  pnorm(log(0.8), mean = log(HR_true), sd = sqrt(4 / d))
}
Consider_fun <- function(d, HR_true) {
  pnorm(log(0.9), mean = log(HR_true), sd = sqrt(4 / d)) -
    pnorm(log(0.8), mean = log(HR_true), sd = sqrt(4 / d))
}
NO_fun <- function(d, HR_true) {
  1 - pnorm(log(0.9), mean = log(HR_true), sd = sqrt(4 / d))
}

HR_low <- 0.65
HR_high <- 0.69

ggplot(data.frame(x = c(10, 300)), aes(x = x)) +
  
  geom_function(fun = \(x) GO_fun(x, HR_low), aes(color = "GO (HR=0.65)")) +
  geom_function(fun = \(x) Consider_fun(x, HR_low), aes(color = "Consider (HR=0.65)")) +
  geom_function(fun = \(x) NO_fun(x, HR_low), aes(color = "NO-GO (HR=0.65)")) +
  
  geom_function(fun = \(x) GO_fun(x, HR_high), aes(color = "GO (HR=0.69)"), linetype = "dashed") +
  geom_function(fun = \(x) Consider_fun(x, HR_high), aes(color = "Consider (HR=0.69)"), linetype = "dashed") +
  geom_function(fun = \(x) NO_fun(x, HR_high), aes(color = "NO-GO (HR=0.69)"), linetype = "dashed") +
  
  labs(
    title = "Decision Probabilities vs. Event Size",
    subtitle = "Solid = HR 0.71 | Dashed = HR 0.69",
    x = "Number of Events (d)",
    y = "Probability",
    color = "Decision Zone"
  ) +
  theme_minimal(base_size = 13)


