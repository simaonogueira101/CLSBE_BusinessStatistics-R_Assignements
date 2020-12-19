# install.packages("ggplot2")
# install.packages("equatiomatic")
# install.packages("plotly")
install.packages("scatterplot3d")
install.packages("lm.beta")

library(ggplot2)
library(equatiomatic)
library(scatterplot3d)

#####################################
#####################################
#####################################
#####################################
#####################################
#####################################
#####################################
#####################################
# try to group every 2 variables ####
# and see which group gives larger ##
# PRE reduction #####################
#####################################
#####################################
#####################################
#####################################
#####################################
#####################################
#####################################
#####################################
#####################################

WA4database <- read.csv("WA_ESSDatabase.csv", header = TRUE)

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
WA4database$trstplc[WA4database$trstplc > 10 | WA4database$trstplc == ""] <- NA

# Cleans dmcntov column from unnecessary values
WA4database$dmcntov[WA4database$dmcntov > 10 | WA4database$dmcntov == ""] <- NA

# Cleans dmcntov column from unnecessary values
WA4database$lrscale[WA4database$lrscale > 10 | WA4database$lrscale == ""] <- NA

# Removes any row where trstplc, dmcntov or lrscale are NA
WA4database <- WA4database[!is.na(WA4database$trstplc), ]
WA4database <- WA4database[!is.na(WA4database$dmcntov), ]
WA4database <- WA4database[!is.na(WA4database$lrscale), ]


# -------------------------------
# ----------- trstplc -----------
# -------------------------------

# Mean for trstplc
trstplc_mean <- round(mean(WA4database$trstplc), 2)

# Standard Deviation for trstplc
trstplc_sd <- round(sd(WA4database$trstplc), 2)

# Mode for trstplc
trstplc_mode <- mode(WA4database$trstplc)

# Null Model for trstplc
WA4database$nullModel_trstplc <- mean(WA4database$trstplc)
WA4database$nullModelErrorSQR_trstplc <- (WA4database$trstplc - WA4database$nullModel) ^ 2
nullModelSSE_trstplc <- sum(WA4database$nullModelErrorSQR_trstplc)


# -------------------------------
# ------ dmcntov & trstplc ------
# -------------------------------

# Linear Model for trstplc x dmcntov
trstplcXdmcntov_lm <- lm(WA4database$trstplc ~ WA4database$dmcntov)
trstplcXdmcntov_lm_intercept <- coef(trstplcXdmcntov_lm)[1]
trstplcXdmcntov_lm_slope <- coef(trstplcXdmcntov_lm)[2]

# Proposed Model for trstplc x dmcntov
WA4database$proposedModel_trstplcXdmcntov <- trstplcXdmcntov_lm_intercept + trstplcXdmcntov_lm_slope * WA4database$dmcntov
WA4database$proposedModelErrorSQR_trstplcXdmcntov <- (WA4database$trstplc - WA4database$proposedModel_trstplcXdmcntov) ^ 2
proposedModelSSE_trstplcXdmcntov <- sum(WA4database$proposedModelErrorSQR_trstplcXdmcntov)

# Model Comparison
# PRE from Null Model to Proposed Model
PRE_null_proposed_trstplcXdmcntov = (nullModelSSE_trstplc - proposedModelSSE_trstplcXdmcntov) / nullModelSSE_trstplc
# Test Statistics
tests_trstplcXdmcntov <- anova(trstplcXdmcntov_lm)
F_trstplcXdmcntov <- tests_trstplcXdmcntov[[4]][1]
p_trstplcXdmcntov <- tests_trstplcXdmcntov[[5]][1] # Value is < 2.2e-16, not 0
t_trstplcXdmcntov <- summary(trstplcXdmcntov_lm)[[4]][5]

# Plots trstplc for dmcntov
# ggplot(WA4database, aes(x = dmcntov, y = trstplc)) +
#   geom_point(
#     alpha = 0.002,
#     size = 5
#   ) +
#   xlab("How democratic [country] is overall") +
#   ylab("Trust in the police") +
#   scale_x_continuous(breaks = seq(0, 10)) +
#   scale_y_continuous(breaks = seq(0, 10)) +
#   # Plots trstplc mean
#   geom_hline(yintercept = trstplc_mean, color = "blue") +
#   annotate(geom = "text", label = paste("trstplc Mean: ", trstplc_mean), colour = "blue", x = 0, y = trstplc_mean, vjust = -1, hjust = 0) +
#   # Plots trstplc standard deviation
#   geom_hline(yintercept = trstplc_sd, color = "red") +
#   annotate(geom = "text", label = paste("trstplc Standard Deviation: ", trstplc_sd), colour = "red", x = 0, y = trstplc_sd, vjust = -1, hjust = 0) +
#   # Plots trstplc mode
#   geom_hline(yintercept = trstplc_mode, color = "green") +
#   annotate(geom = "text", label = paste("trstplc Mode: ", trstplc_mode), colour = "green", x = 0, y = trstplc_mode, vjust = -1, hjust = 0) +
#   # Plots trstplc x dmcntov  regression line (Model 1)
#   geom_abline(slope = trstplcXdmcntov_lm_slope, intercept = trstplcXdmcntov_lm_intercept, color = "purple") +
#   annotate(geom = "text", label = paste("Model 1"), colour = "purple", x = 0, y = trstplcXdmcntov_lm_intercept + 0.5, vjust = -1, hjust = 0)



# ----------------------------------------
# ------ dmcntov, trstplc & lrscale ------
# ----------------------------------------
library(lm.beta)
library(scatterplot3d)

WA4database <- read.csv("WA_ESSDatabase.csv", header = TRUE)

# Relevant variables for this assignment:
#	15, trstplc - Trust in the police - dependent variable
# 32, dmcntov - How democratic [country] is overall - independent variable
# 33, lrscale - Placement on left right scale - independent variable

# Cleans trstplc column from unnecessary values
WA4database$trstplc[WA4database$trstplc > 10 | WA4database$trstplc == ""] <- NA

# Cleans dmcntov column from unnecessary values
WA4database$dmcntov[WA4database$dmcntov > 10 | WA4database$dmcntov == ""] <- NA

# Cleans dmcntov column from unnecessary values
WA4database$lrscale[WA4database$lrscale > 10 | WA4database$lrscale == ""] <- NA

# Removes any row where trstplc, dmcntov or lrscale are NA
WA4database <- WA4database[!is.na(WA4database$trstplc), ]
WA4database <- WA4database[!is.na(WA4database$dmcntov), ]
WA4database <- WA4database[!is.na(WA4database$lrscale), ]

# Model for trstplc x dmcntov x lrscale
trstplcXdmcntovXlrscale_lm <- lm(WA4database$trstplc ~ WA4database$dmcntov + WA4database$lrscale)
summary(lm(trstplcXdmcntovXlrscale_lm))
summary(lm.beta(trstplcXdmcntovXlrscale_lm))
aov(trstplcXdmcntovXlrscale_lm) 
anova (trstplcXdmcntovXlrscale_lm) 

trstplcXdmcntovXlrscale_plot <- scatterplot3d(
  x = WA4database$dmcntov,
  y = WA4database$lrscale,
  z = WA4database$trstplc,
  type ="p",
  xlab = "How democratic [country] is overall",
  ylab = "Placement on left right scale",
  zlab = "Trust in the police"
)

trstplcXdmcntovXlrscale_plot$plane3d(
  trstplcXdmcntovXlrscale_lm,
  draw_polygon = TRUE,
  draw_lines=TRUE
)