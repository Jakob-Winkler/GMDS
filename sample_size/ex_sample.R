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



# 1.2
# With this object, use getSampleSizeSurvival to determine the number of patients that need to be enrolled.
# A lambda of 0.064 is assumed for the treatment group, and a lambda of 0.089 for the control group.
# A dropout rate of 15% per year is expected.
# Additionally, a constant enrollment over 6 months is assumed.
# How many patients need to be enrolled to achieve a total study duration (from first randomization to final analysis) of 1 year?
# At how many events is the final analysis performed?




# 1.3
# If you did everything correctly, you should have gotten:
# Number of patients = 621.9
# Number of events = 288.7
# What is the study duration if you enroll only 400 patients?
# Keep all other parameters the same.


# 1.4
# Because dropout rates are often underestimated, let’s try some higher rates.
# What is the study duration when the yearly dropout rate is 20% and 25%?



# 1.5
# Now assume a worse treatment effect with a lambda of 0.06942.
# Keep the number of events the same as in 1.3 (i.e., 289).
# What is the power?
# Assume a dropout rate of 15% again and 400 patients.
# Use the getPowerSurvival function (make sure to set directionUpper to the correct value).


# 1.6
# What is the number of events needed to achieve at least 65% power?
# You need to specify a new design and set the number of patients to 450.


# 1.7
# You decided to set the analysis time point at the number of events calculated in 1.6.
# What power do you get when the lambda is 0.064 again?


# 1.8
# Now let’s create a design with an interim analysis. Choose the O’Brien & Fleming type alpha spending.
# Define one interim analysis at 70% of the events (information rate).
# Define a futility bound at a z-value of 0. Make sure it is not binding.

# Use the summary function on the design.
# What is the:
# – Inflation factor?
# – Power at the interim analysis?


# 1.9
# Now perform the same sample size calculation as in 1.3, but using the new design.
#
# Compare the results:
# – How does the number of events differ?
# – How does the final analysis time differ?
# – What is the "Expected study duration under H1"? Is this the duration my trial will last if H1 is true?
# – What is the "Expected number of events under H0"? (This is not part of the summary, but it is included in the object — you can use print to see it.)




# 1.10
# The interim analysis from 1.9 seems to work well.
# With only a small increase in the required number of events, the expected number of events was reduced significantly.
# You can improve this further, but it depends on your goal.
# However, you can also make it worse - for example, by placing interim analyses too early or too close to each other.
# Try out some designs to see how “bad” it can get, so you develop an intuition for what to avoid.



# Optional
# 2. GO/NO-GO
# Remember that log(HR) ~ N(log(HR), 4/d), with d being the number of events.

# 2.1
# Let's assume an event size of 80.
# The GO interval for the HR is (0, 0.8], and the NO-GO interval is (0.8, ∞).
# Calculate the GO and NO-GO probabilities for a true HR of 0.65.



# 2.2
# Add the consideration zone from [0.8, 0.9].
# So the GO interval is (0, 0.8), the consideration zone is [0.8, 0.9], and the NO-GO interval is (0.9, ∞).
# Calculate the probabilities for all three intervals.


# 2.3 
# Now plot the probabilities for the three intervals as a function of the event size d.
# The x-axis should represent the event size d, and the y-axis the probability. Plot this for HR values of 0.69 and 0.65.
# You might find ggplot2::geom_function helpful.

