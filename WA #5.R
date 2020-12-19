# install.packages("lm.beta")

library(lm.beta)

WA5database <- read.csv("WA_ESSDatabase.csv", header = TRUE)

# Relevant variables for this assignment:
#	15, trstplc - Trust in the police - dependent variable
# 32, dmcntov - How democratic [country] is overall - independent variable
# 33, lrscale - Placement on left right scale - independent variable
# 10, pplfair - Most people try to take advantage of you, or try to be fair - independent variable
# 38, stfedu - State of education in country nowadays - independent variable

# Cleans trstplc column from unnecessary values
WA5database$trstplc[WA5database$trstplc > 10 | WA5database$trstplc == ""] <- NA

# Cleans dmcntov column from unnecessary values
WA5database$dmcntov[WA5database$dmcntov > 10 | WA5database$dmcntov == ""] <- NA

# Cleans lrscale column from unnecessary values
WA5database$lrscale[WA5database$lrscale > 10 | WA5database$lrscale == ""] <- NA

# Cleans pplfair column from unnecessary values
WA5database$pplfair[WA5database$pplfair > 10 | WA5database$pplfair == ""] <- NA

# Cleans stfedu column from unnecessary values
WA5database$stfedu[WA5database$stfedu > 10 | WA5database$stfedu == ""] <- NA

# Removes any row where trstplc, dmcntov or lrscale are NA
WA5database <- WA5database[!is.na(WA5database$trstplc), ]
WA5database <- WA5database[!is.na(WA5database$dmcntov), ]
WA5database <- WA5database[!is.na(WA5database$lrscale), ]
WA5database <- WA5database[!is.na(WA5database$pplfair), ]
WA5database <- WA5database[!is.na(WA5database$stfedu), ]


# ----------------------------------------
# ------ dmcntov, trstplc & lrscale ------
# ----------------------------------------

# Model for trstplc x dmcntov x lrscale
trstplcXdmcntovXlrscale_lm <- lm(
  WA5database$trstplc ~
  WA5database$dmcntov +
  WA5database$lrscale
)


# ---------------------------------------------------------
# ------ dmcntov, trstplc, lrscale, pplfair & stfedu ------
# ---------------------------------------------------------

# Model for trstplc x dmcntov x lrscale x pplfair x stfedu
trstplcXdmcntovXlrscaleXpplfairXstfedu_lm <- lm(
  WA5database$trstplc ~
  WA5database$dmcntov +
  WA5database$lrscale +
  WA5database$pplfair +
  WA5database$stfedu
)


# ----------------------------------------
# ------------ Model Analysis ------------
# ----------------------------------------

summary(lm.beta(trstplcXdmcntovXlrscale_lm))
summary(lm.beta(trstplcXdmcntovXlrscaleXpplfairXstfedu_lm))

anova(trstplcXdmcntovXlrscale_lm)
anova(trstplcXdmcntovXlrscaleXpplfairXstfedu_lm)
anova(trstplcXdmcntovXlrscaleXpplfairXstfedu_lm, trstplcXdmcntovXlrscale_lm)


