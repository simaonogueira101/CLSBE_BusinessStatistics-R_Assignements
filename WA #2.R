# install.packages("ggplot2")
library(ggplot2)

# Function to calculate Mode
mode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}

# Cleans trstplc column from unnecessary values
WAdatabase$trstplc[WAdatabase$trstplc > 10 | WAdatabase$trstplc == ""] <- NA

# Mean for trstplc
trstplc_mean <- round(mean(WAdatabase$trstplc, na.rm = TRUE), 2)

# Standard Deviation for trstplc
trstplc_sd <- round(sd(WAdatabase$trstplc, na.rm = TRUE), 2)

# Mode for trstplc
trstplc_mode <- mode(WAdatabase$trstplc)

# Plots trstplc for each row in the dataframe
ggplot(WAdatabase, aes(x = as.numeric(row.names(WAdatabase)), y = trstplc)) +
  geom_point(alpha = 0.008) +
  xlab("Participant") +
  ylab("Trust in the police") +
  scale_y_continuous(breaks = seq(0, 10)) +
  # Plots trstplc mean
  geom_hline(yintercept = trstplc_mean, color = "blue") +
  annotate(geom = "text", label = paste("Mean: ", trstplc_mean), colour = "blue", x = 0, y = trstplc_mean, vjust = -1, hjust = 0) +
  # Plots trstplc standard deviation
  geom_hline(yintercept = trstplc_sd, color = "red") +
  annotate(geom = "text", label = paste("Standard Deviation: ", trstplc_sd), colour = "red", x = 0, y = trstplc_sd, vjust = -1, hjust = 0) +
  # Plots trstplc mode
  geom_hline(yintercept = trstplc_mode, color = "green") +
  annotate(geom = "text", label = paste("Mode: ", trstplc_mode), colour = "green", x = 0, y = trstplc_mode, vjust = -1, hjust = 0)
