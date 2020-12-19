# Turn off scientific notation
options(scipen = 999)

# Install required packages
# install.packages("lm.beta")
# install.packages("itertools")

# library(lm.beta)
library(itertools)

# Load DF
# FAdatabase <- read.csv("WA_ESSDatabase.csv", header = TRUE)

dependent_var <- "trstplc"

# Clean dependent variable
FAdatabase[dependent_var][FAdatabase[dependent_var] > 10 | FAdatabase[dependent_var] == ""] <- NA
FAdatabase <- FAdatabase[!is.na(FAdatabase[dependent_var]), ]

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
  "isco08"
)

debug_columns <- c(
  "ppltrst",
  "pplfair",
  "tporgwk",
  "ipbhprp"
)

# Create empty results array
results <- data.frame(model = character(0), r_squared = numeric(0))

# it <- ihasNext(product(a=dependent_var, b=colnames(FAdatabase)))
# while (hasNext(it)) {
#   x <- nextElem(it)
#   cat(sprintf('a = %s, b = %s\n', x$a, x$b))
# }

for (column in colnames(FAdatabase)) {
  #if (! column %in% excluded_columns) {
  if (column %in% debug_columns) {
    
    # Duplicate DF into a temporary object
    temp_database <- data.frame(FAdatabase)
    
    # Determine type of scale and clean column
    max_answer <- max(temp_database[column], na.rm = TRUE)
    if (max_answer > 9) {
      temp_database[column][temp_database[column] > 10 | temp_database[column] == ""] <- NA
    } else if (max_answer > 6) {
      temp_database[column][temp_database[column] > 6 | temp_database[column] == ""] <- NA
    }
    # Remove rows with NA values
    temp_database <- temp_database[!is.na(temp_database[column]), ]

    # Temporary model
    temp_model <- lm(
      unlist(temp_database[dependent_var])
      ~ unlist(temp_database[column])
    )
    
    temp_model_string <- paste(dependent_var, "~", column, sep = " ")
    temp_r_squared <- as.numeric(summary(temp_model)$r.squared)

    # Record results
    temp_results <- c(
      temp_model_string,
      temp_r_squared
    )
    results[nrow(results) + 1,] <- temp_results
  }
}

results <- results[order(results$r_square, decreasing = TRUE),] 
print(results)
