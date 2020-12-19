# install.packages("summarytools")
# install.packages("igraph")
# install.packages("equatiomatic")
# install.packages("psych")
# install.packages("lm.beta")
# install.packages("scatterplot3d")
# install.packages("lm.beta")

library(summarytools)
library(igraph)
library(equatiomatic)
library(psych)
library(lm.beta)
library(scatterplot3d)
library(lm.beta)

WAdatabaseLUCIA <- read.csv("WA_ESSdatabase.csv", header=TRUE)

behaviourdata <- subset(WAdatabaseLUCIA, select = c("ipbhprp","trstprt","rlgdgr","happy","health"))

behaviour<-behaviourdata[!(behaviourdata$trstprt=="77" |
                             behaviourdata$trstprt=="88" |
                             behaviourdata$trstprt=="99" |
                             behaviourdata$rlgdgr=="77" |
                             behaviourdata$rlgdgr=="88" |
                             behaviourdata$rlgdgr=="99"|
                             behaviourdata$happy=="77" |
                             behaviourdata$happy=="88" |
                             behaviourdata$happy=="99" |
                             behaviourdata$ipbhprp=="7" |
                             behaviourdata$ipbhprp=="8"|
                             behaviourdata$ipbhprp=="9" |
                             behaviourdata$health=="7" |
                             behaviourdata$health=="8" |
                             behaviourdata$health=="9"),]

describe(behaviour$trstprt)
describe(behaviour$rlgdgr)
describe(behaviour$happy)
describe(behaviour$health)

Model.3.1 <- lm(behaviour$ipbhprp~behaviour$trstprt+behaviour$rlgdgr)

Model.3.2 <- lm(behaviour$ipbhprp~behaviour$trstprt+behaviour$rlgdgr+
                  behaviour$happy+
                  behaviour$health)

summary(
  lm.beta(Model.3.1)
) # parameters and test statistics for model 1

summary(
  lm.beta(Model.3.2)
) # parameters and test statistics for model 2

extract_eq(Model.3.1)
extract_eq(Model.3.2)

behaviour$propmodel1 <-
  2.790772 +
  0.011811 * behaviour$trstprt +
  -0.056419 * behaviour$rlgdgr # proposed model 1
behaviour$propmodelerrorSQR1 <-
  (behaviour$ipbhprp-behaviour$propmodel1)^2 # squared error
sum(behaviour$propmodelerrorSQR1) # SSE of model 1

behaviour$propmodel2 <-
  2.945279 +
  0.009538 * behaviour$trstprt +
  -0.054912 * behaviour$rlgdgr +
  -0.003360 * behaviour$happy +
  -0.058255 * behaviour$health # proposed model 2
behaviour$propmodelerrorSQR2 <-
  (behaviour$ipbhprp-behaviour$propmodel2)^2 # squared error
sum(behaviour$propmodelerrorSQR2) # SSE of model 2

behaviour$nullmodel <- mean(behaviour$ipbhprp) # null model
behaviour$nullmodelerrorSQR <- (behaviour$ipbhprp-behaviour$nullmodel)^2 # squared difference
sum(behaviour$nullmodelerrorSQR) # sum of the squared error of the null model

PRE_Model.3.1 <- (sum(behaviour$nullmodelerrorSQR)-sum(behaviour$propmodelerrorSQR1))/sum(behaviour$nullmodelerrorSQR)
PRE_Model.3.2 <- (sum(behaviour$nullmodelerrorSQR)-sum(behaviour$propmodelerrorSQR2))/sum(behaviour$nullmodelerrorSQR)
PRE_Change <- PRE_Model.3.2 - PRE_Model.3.1
PRE_Model.3.1

PRE_Model.3.2

PRE_Change

anova(Model.3.1) # SSE and test statistics for model 1
anova(Model.3.2) # SSE test statistics for model 2

anova(
  Model.3.2,
  Model.3.1
) # test statistics for PRE change

summary(
  lm.beta(Model.3.1)
) # parameters and test statistics for model 1

summary(
  lm.beta(Model.3.2)
) # parameters and test statistics for model 2