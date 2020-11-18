# install.packages("ggplot2")
# install.packages("equatiomatic")
library(ggplot2)
library(equatiomatic)

#WA3database <- read.csv("WA_ESSDatabase.csv", header = TRUE)

# Function to calculate Mode
mode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}

# Relevant variables for this assignment:
#	15, trstplc - Trust in the police - dependent variable
# 32, dmcntov - How democratic [country] is overall - independent variable
# 33, lrscale - Placement on left right scale - independent variable

# Cleans trstplc column from unnecessary values
WA3database$trstplc[WA3database$trstplc > 10 | WA3database$trstplc == ""] <- NA

# Cleans dmcntov column from unnecessary values
WA3database$dmcntov[WA3database$dmcntov > 10 | WA3database$dmcntov == ""] <- NA

# Removes any row where trstplc, dmcntov or lrscale are NA
WA3database <- WA3database[!is.na(WA3database$trstplc), ]
WA3database <- WA3database[!is.na(WA3database$dmcntov), ]
WA3database <- WA3database[!is.na(WA3database$lrscale), ]


# -------------------------------
# ----------- trstplc -----------
# -------------------------------

# Mean for trstplc
trstplc_mean <- round(mean(WA3database$trstplc), 2)

# Standard Deviation for trstplc
trstplc_sd <- round(sd(WA3database$trstplc), 2)

# Mode for trstplc
trstplc_mode <- mode(WA3database$trstplc)

# Null Model for trstplc
WA3database$nullModel_trstplc <- mean(WA3database$trstplc)
WA3database$nullModelErrorSQR_trstplc <- (WA3database$trstplc - WA3database$nullModel) ^ 2
nullModelSSE_trstplc <- sum(WA3database$nullModelErrorSQR_trstplc)


# -------------------------------
# ------ dmcntov & trstplc ------
# -------------------------------

# Linear Model for trstplc x dmcntov
trstplcXdmcntov_lm <- lm(WA3database$trstplc ~ WA3database$dmcntov)
trstplcXdmcntov_lm_intercept <- coef(trstplcXdmcntov_lm)[1]
trstplcXdmcntov_lm_slope <- coef(trstplcXdmcntov_lm)[2]

# Proposed Model for trstplc x dmcntov
WA3database$proposedModel_trstplcXdmcntov <- trstplcXdmcntov_lm_intercept + trstplcXdmcntov_lm_slope * WA3database$dmcntov
WA3database$proposedModelErrorSQR_trstplcXdmcntov <- (WA3database$trstplc - WA3database$proposedModel_trstplcXdmcntov) ^ 2
proposedModelSSE_trstplcXdmcntov <- sum(WA3database$proposedModelErrorSQR_trstplcXdmcntov)

# Model Comparison
# PRE from Null Model to Proposed Model
PRE_null_proposed_trstplcXdmcntov = (nullModelSSE_trstplc - proposedModelSSE_trstplcXdmcntov) / nullModelSSE_trstplc
# Test Statistics
tests_trstplcXdmcntov <- anova(trstplcXdmcntov_lm)
F_trstplcXdmcntov <- tests_trstplcXdmcntov[[4]][1]
p_trstplcXdmcntov <- tests_trstplcXdmcntov[[5]][1] # Value is < 2.2e-16, not 0
t_trstplcXdmcntov <- summary(trstplcXdmcntov_lm)[[4]][5]

# Plots trstplc for dmcntov
ggplot(WA3database, aes(x = dmcntov, y = trstplc)) +
  geom_point(
    alpha = 0.002,
    size = 5
  ) +
  xlab("How democratic [country] is overall") +
  ylab("Trust in the police") +
  scale_x_continuous(breaks = seq(0, 10)) +
  scale_y_continuous(breaks = seq(0, 10)) +
  # Plots trstplc mean
  geom_hline(yintercept = trstplc_mean, color = "blue") +
  annotate(geom = "text", label = paste("trstplc Mean: ", trstplc_mean), colour = "blue", x = 0, y = trstplc_mean, vjust = -1, hjust = 0) +
  # Plots trstplc standard deviation
  geom_hline(yintercept = trstplc_sd, color = "red") +
  annotate(geom = "text", label = paste("trstplc Standard Deviation: ", trstplc_sd), colour = "red", x = 0, y = trstplc_sd, vjust = -1, hjust = 0) +
  # Plots trstplc mode
  geom_hline(yintercept = trstplc_mode, color = "green") +
  annotate(geom = "text", label = paste("trstplc Mode: ", trstplc_mode), colour = "green", x = 0, y = trstplc_mode, vjust = -1, hjust = 0) +
  # Plots trstplc x dmcntov  regression line (Model 1)
  geom_abline(slope = trstplcXdmcntov_lm_slope, intercept = trstplcXdmcntov_lm_intercept, color = "purple") +
  annotate(geom = "text", label = paste("Model 1"), colour = "purple", x = 0, y = trstplcXdmcntov_lm_intercept + 0.5, vjust = -1, hjust = 0)