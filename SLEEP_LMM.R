
rm(list=ls())

library(ggplot2)
library(nlme)

getwd()
setwd("C:/Users/Owner/Desktop")
SLEEP <- read.csv("SLEEP_Only_FA_Sep.csv", header = TRUE)

# Mutate brain volume to represent measured volumes in cubic centimeters

SLEEP$Brain_Volume <- SLEEP$Brain_Volume * 0.001

### Response Variable 1 - STN Volume ###

attach(SLEEP)

# Checking the distribution of the response variable

hist(STN_Volume) # Appears normal - good!

# Plot the data

ggplot(SLEEP, aes(STN_Volume, Age)) +
  geom_point() 

ggplot(SLEEP, aes(STN_Volume, Brain_Volume)) +  # Potential correlation here..  difficult to tell
  geom_point() 

# Are the assumptions met?

boxplot(STN_Volume ~ Side)

boxplot(STN_Volume ~ Gender)

boxplot(STN_Volume ~ Patient)  # Something may be going on here...

# Use ggplot2 for interesting boxplot

bp <- ggplot(SLEEP, aes(Patient,STN_Volume, color=Patient)) + geom_boxplot() + 
  geom_jitter(width = 0.05) + theme_classic()

bp + scale_color_manual(values=c("goldenrod3","dodgerblue1")) + theme(legend.position = "none") + 
  theme(axis.title = element_text(face="bold", size=20)) + xlab("Group") + ylab("STN Volume (mm^3)")

# Fit a linear mixed model

SLEEP.lmm <- lme(STN_Volume ~ Age + Brain_Volume + STN_FA + Gender + Side + Patient, ~ 1|Subject, data = SLEEP)
summary(SLEEP.lmm)

# Drop insignificant predictors from the model

SLEEP.lmm <- lme(STN_Volume ~ Brain_Volume + Patient, ~ 1|Subject, data = SLEEP)
summary(SLEEP.lmm) 

# 95% Confidence Intervals 

# True difference in STN Volume between Control and EPD subjects
12.86890 # True difference in STN Volume between Control and EPD subjects

# Multiply 97.5 percentile point of normal distribution by std error from mixed model
1.96 * 4.795765 # 95% CI:  13 ± 9.4 mm^3 == [4, 22], p = .010

plot(SLEEP.lmm) 

qqnorm(resid(SLEEP.lmm))
qqline(resid(SLEEP.lmm)) # Not perfect but looks alright





### Response Variable 2 - EPD STN Volume ###

### Subset the data to only include EPD Patients ###

SLEEP_EPD <- subset(SLEEP, Patient == "EPD")
hist(SLEEP_EPD$STN_FA)

detach(SLEEP)
attach(SLEEP_EPD)

# Checking the distribution of the response variable

hist(STN_Volume) # Appears normal - good!

# Plot the data

ggplot(SLEEP_EPD, aes(STN_Volume, Age)) +
  geom_point() 

ggplot(SLEEP_EPD, aes(STN_Volume, Brain_Volume)) +  # Potential correlation here
  geom_point() 

ggplot(SLEEP_EPD, aes(STN_Volume, UPDS_Motor)) +  # Potential correlation here
  geom_point() 

# Potential correlation between ICA volume and DBS Electrode Tip Migration

boxplot(STN_Volume ~ Side)

boxplot(STN_Volume ~ Gender)

# Fit a linear mixed model

SLEEP_EPD.lmm <- lme(STN_Volume ~ Age + Brain_Volume + STN_FA + Gender + Side + UPDS_Motor, ~ 1|Subject, data = SLEEP_EPD)
summary(SLEEP_EPD.lmm)

# Drop insignificant predictors from the model

SLEEP_EPD.lmm <- lme(STN_Volume ~ UPDS_Motor, ~ 1|Subject, data = SLEEP_EPD)
summary(SLEEP_EPD.lmm) 

# Plot the Data

fun.1 <- function(x) (-0.56)*x + 129.62406

ggplot(SLEEP_EPD, aes(UPDS_Motor, STN_Volume, color = Side)) + geom_point() + 
  stat_function(fun = fun.1, color = "black") + labs(title = "
STN Volume vs. UPDRS Part III Motor Score
", x = "
UPDRS Part III Motor Score
", y = "
STN Volume (mm^3)
")

# Model Estimated STN Volume vs. UPDRS Part III Motor Score: 
# "Change in STN Volume (mm3) attributable to UPDRS Part III Motor Score as estimated 
# by our linear mixed-effects model."

fun.2 <- function(x) (-0.56)*x 

ggplot(SLEEP_EPD, aes(UPDS_Motor, STN_Volume)) + stat_function(fun = fun.2, color = "black") + 
  xlim(15, 65) + ylim(-50, 0) + labs(title = "
Model Estimated STN Volume vs. UPDRS Part III Motor Score
", x = "
UPDRS Part III Motor Score
", y = "
Model Estimated STN Volume (mm^3)
") + theme_classic()

plot(SLEEP_EPD.lmm) 

qqnorm(resid(SLEEP_EPD.lmm))
qqline(resid(SLEEP_EPD.lmm)) # Not perfect but looks alright





### Response Variable 3 - STN FA ###

detach(SLEEP_EPD)
attach(SLEEP)

# Checking the distribution of the response variable

hist(STN_FA) # Appears normal!

# Plot the data

ggplot(SLEEP, aes(STN_FA, Age)) +
  geom_point() 

ggplot(SLEEP, aes(STN_FA, Brain_Volume)) +  
  geom_point() 

ggplot(SLEEP, aes(STN_FA, STN_Volume)) +  # Potential correlation... hard to tell
  geom_point() 

# Potential correlation between ICA volume and DBS Electrode Tip Migration

boxplot(STN_FA ~ Side)

boxplot(STN_FA ~ Gender)

boxplot(STN_FA ~ Patient)  # Something may be going on here...

# Use ggplot2 for interesting boxplot

bp <- ggplot(SLEEP, aes(Patient,STN_FA, color=Patient)) + geom_boxplot() + 
  geom_jitter(width = 0.05) + theme_classic()

bp + scale_color_manual(values=c("goldenrod3","dodgerblue1")) + theme(legend.position = "none") + 
  theme(axis.title = element_text(face="bold", size=20)) + xlab("Group") + ylab("STN FA")

# Fit a linear mixed model

SLEEP_FA.lmm <- lme(STN_FA ~ Age + Brain_Volume + STN_Volume + Gender + Side + Patient, ~ 1|Subject, data = SLEEP)
summary(SLEEP_FA.lmm)

# Drop insignificant predictors from the model

SLEEP_FA.lmm <- lme(STN_FA ~ STN_Volume + Patient, ~ 1|Subject, data = SLEEP)
summary(SLEEP_FA.lmm)

# 95% Confidence Intervals

# True difference in STN FA between Control and EPD subjects
0.0857851 # Value from mixed model

# Multiply 97.5 percentile point of normal distribution by std error from mixed model
1.96 * 0.02555076 # 95% CI:  0.086 ± 0.050 mm^3 == [0.036, 0.136], (p = .0016) 

plot(SLEEP_FA.lmm) 

qqnorm(resid(SLEEP_FA.lmm))
qqline(resid(SLEEP_FA.lmm)) # Not perfect but looks alright





### Response Variable 4 - EPD STN FA ###

detach(SLEEP)
attach(SLEEP_EPD)

# Plot the data

ggplot(SLEEP_EPD, aes(STN_FA, Age)) +
  geom_point() 

ggplot(SLEEP_EPD, aes(STN_FA, STN_Volume)) +  # Potential correlation here
  geom_point() 

ggplot(SLEEP_EPD, aes(STN_FA, UPDS_Motor)) +  # Difficult to interpret - potential correlation
  geom_point() 

# Potential correlation between ICA volume and DBS Electrode Tip Migration

boxplot(STN_FA ~ Side)

boxplot(STN_FA ~ Gender)

# Fit a linear mixed model

SLEEP_FA_EPD.lmm <- lme(STN_FA ~ Age + Brain_Volume + STN_Volume + Gender + Side + UPDS_Motor, 1|Subject, data = SLEEP_EPD)
summary(SLEEP_FA_EPD.lmm)

# Drop insignificant predictors from the model

SLEEP_FA_EPD.lmm <- lme(STN_FA ~ STN_Volume + UPDS_Motor, ~ 1|Subject, data = SLEEP_EPD)
summary(SLEEP_FA_EPD.lmm)  # UPDRS Motor approaches a level of statistical significance -> There may be an association

plot(SLEEP_FA_EPD.lmm) 

qqnorm(resid(SLEEP_FA_EPD.lmm))
qqline(resid(SLEEP_FA_EPD.lmm)) # Not perfect but looks alright





citation("ggplot2")
citation("nlme")

