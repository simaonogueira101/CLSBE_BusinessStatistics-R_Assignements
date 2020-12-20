# Turn off scientific notation
options(scipen = 999)

# Install required packages
# install.packages("lm.beta")
# install.packages("broom")

library(lm.beta)
library(broom)

# Load DF
# FAdatabase <- read.csv("WA_ESSDatabase.csv", header = TRUE)

dependent_var <- "trstplc"
iteration_num <- 4

# Columns to be excluded from analysis
excluded_columns <- c(
  # Dependent variable
  dependent_var,
  
  # DF info variables
  "name",
  "essround",
  "edition",
  "proddate",
  "idno",
  "cntry",
  "region",
  "regunit",
  "inwtm",
  
  # Other unusable variables
  "ctzshipc",
  "cntbrthc",
  "livecnta",
  "lnghom1",
  "lnghom2",
  "fbrncntb",
  "mbrncntb",
  "hhmmb",
  "agea",
  "yrbrn",
  "icpart1",
  "pdjobyr",
  "emplno",
  "eduyrs",
  "njbspv",
  "wkhct",
  "wkhtotp",
  "isco08",
  "prtvtal",
  "wrkorg",
  "prtclal",
  "rlgdnal",
  "rlgdeal",
  "cldgng",
  "rshpscz",
  "rshpsfi",
  "lvgptnea",
  "mbtru",
  "edulvlb",
  "edulvlpb"
)

debug_columns <- c(
  "trstplc",
  "trstlgl",
  "mnactp",
  "tporgwk"
)

# Columns to be used in analysis
# good_columns <- debug_columns
good_columns <- colnames(FAdatabase)[! colnames(FAdatabase) %in% excluded_columns]

# Initiate model variable
model <- c(dependent_var)

# Function to clean database
clean_database <- function (database, variables) {
  # Determine type of scale and clean column
  for (variable in variables) {
    
    max_answer <- max(database[variable], na.rm = TRUE)
    
    if (max_answer > 9) {
      database[variable][database[variable] > 10 | database[variable] == ""] <- NA
    } else if (max_answer > 6) {
      database[variable][database[variable] > 6 | database[variable] == ""] <- NA
    }
    # Remove rows with NA values
    database <- database[!is.na(database[variable]), ]
  }
  
  return(database)
}

progress_total <- iteration_num * length(good_columns) - iteration_num + 1
progress_current <- 0

# Iterate to find each variable for the model
for (i in 1:iteration_num) {
  # Create empty results array
  results <- data.frame(variable = character(0), p_value_individual = numeric(0), r_squared_overall = numeric(0), p_value_overall = numeric(0))
  
  for (column in good_columns[! good_columns %in% model]) {
    # Duplicate DF into a temporary object
    temp_database <- data.frame(FAdatabase)
    
    proposed_model <- c(model, column)
    temp_database <- clean_database(temp_database, proposed_model)
    
    current_model <- ""
    
    for (i in 1:length(model)) {
      if(i < 2) {
        current_model <- paste("temp_database$", current_model, toString(model[i]), " ~ ", sep="")
      } else {
        current_model <- paste(current_model, "temp_database$", toString(model[i]), " + ", sep="")
      }
    }
    
    current_model <- paste(current_model, "temp_database$", toString(column), sep="")

    # Temporary model analysis
    temp_model <- lm(formula(current_model))
    temp_summary <- summary(lm.beta(temp_model))
    
    temp_p_value_individual <- format(round(as.numeric(
      temp_summary$coefficients[length(temp_summary$coefficients)]
    ), 10), nsmall = 10)

    temp_r_squared_overall <- as.numeric(temp_summary$r.squared)
    
    temp_p_value_overall <- format(round(as.numeric(pf(
      temp_summary$fstatistic[[1]],
      temp_summary$fstatistic[[2]],
      temp_summary$fstatistic[[3]],
      lower.tail = FALSE
    )), 10), nsmall = 10)
    
    # Record results
    temp_results <- c(
      column,
      temp_p_value_individual,
      temp_r_squared_overall,
      temp_p_value_overall
    )
    results[nrow(results) + 1,] <- temp_results
    
    # Update progress indicator
    print(paste("Progress: ", format(round(progress_current / progress_total * 100, 0), nsmall = 0), "%", sep=""))
    progress_current <- progress_current + 1
  }
  
  # Order result based on PRE
  results <- results[order(results$r_square, decreasing = TRUE), ]
  # Remove results with no statistical significance
  results <- results[results$p_value_individual < 0.05 & results$p_value_overall < 0.05, ]
  head(results, 10)
  
  # Record winning variable in model
  print("-----------------")
  model <- c(model, results[1, 1])
  print(paste("Variable #", i, ": ", results[1, 1], sep=""))
  print(paste("R-Squared: ", results[1, 2], sep=""))
  print("-----------------")
}

# "trstplc" "trstlgl" "mainact" "mnactp"  "rlgdnm" 
testDB <- clean_database(FAdatabase, c("trstplc", "dmcntov", "lrscale", "trstlgl", "mainact", "mnactp", "rlgdnm"))
summary(lm.beta(lm(testDB$trstplc ~ testDB$dmcntov + testDB$lrscale)))
summary(lm.beta(lm(testDB$trstplc ~ testDB$trstlgl + testDB$mainact + testDB$mnactp + testDB$rlgdnm)))

anova(
  lm(testDB$trstplc ~ testDB$dmcntov + testDB$lrscale),
  lm(testDB$trstplc ~ testDB$trstlgl + testDB$mainact + testDB$mnactp + testDB$rlgdnm)
)
