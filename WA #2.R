# install.packages("ggplot2")
library(ggplot2)

# Function to calculate Mode
mode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}

# Relevant variables for this assignment:
# 32, dmcntov - How democratic [country] is overall
# 33, lrscale - Placement on left right scale


# ---------------------
# ------ dmcntov ------
# ---------------------

# Cleans dmcntov column from unnecessary values
WAdatabase$dmcntov[WAdatabase$dmcntov > 10 | WAdatabase$dmcntov == ""] <- NA

# Mean for dmcntov
dmcntov_mean <- round(mean(WAdatabase$dmcntov, na.rm = TRUE), 2)

# Standard Deviation for dmcntov
dmcntov_sd <- round(sd(WAdatabase$dmcntov, na.rm = TRUE), 2)

# Mode for dmcntov
dmcntov_mode <- mode(WAdatabase$dmcntov)

# Plots dmcntov for each row in the dataframe
ggplot(WAdatabase, aes(x = as.numeric(row.names(WAdatabase)), y = dmcntov)) +
  geom_point(alpha = 0.008) +
  xlab("Participant") +
  ylab("How democratic [country] is overall") +
  scale_y_continuous(breaks = seq(0, 10)) +
  # Plots dmcntov mean
  geom_hline(yintercept = dmcntov_mean, color = "blue") +
  annotate(geom = "text", label = paste("Mean: ", dmcntov_mean), colour = "blue", x = 0, y = dmcntov_mean, vjust = -1, hjust = 0) +
  # Plots dmcntov standard deviation
  geom_hline(yintercept = dmcntov_sd, color = "red") +
  annotate(geom = "text", label = paste("Standard Deviation: ", dmcntov_sd), colour = "red", x = 0, y = dmcntov_sd, vjust = -1, hjust = 0) +
  # Plots dmcntov mode
  geom_hline(yintercept = dmcntov_mode, color = "green") +
  annotate(geom = "text", label = paste("Mode: ", dmcntov_mode), colour = "green", x = 0, y = dmcntov_mode, vjust = -1, hjust = 0)


# ---------------------
# ------ lrscale ------
# ---------------------

# Cleans lrscale column from unnecessary values
WAdatabase$lrscale[WAdatabase$lrscale > 10 | WAdatabase$lrscale == ""] <- NA

# Mean for lrscale
lrscale_mean <- round(mean(WAdatabase$lrscale, na.rm = TRUE), 2)

# Standard Deviation for lrscale
lrscale_sd <- round(sd(WAdatabase$lrscale, na.rm = TRUE), 2)

# Mode for lrscale
lrscale_mode <- mode(WAdatabase$lrscale)

# Plots lrscale for each row in the dataframe
ggplot(WAdatabase, aes(x = as.numeric(row.names(WAdatabase)), y = lrscale)) +
  geom_point(alpha = 0.008) +
  xlab("Participant") +
  ylab("Placement on left right scale") +
  scale_y_continuous(breaks = seq(0, 10)) +
  # Plots lrscale mean
  geom_hline(yintercept = lrscale_mean, color = "blue") +
  annotate(geom = "text", label = paste("Mean: ", lrscale_mean), colour = "blue", x = 0, y = lrscale_mean, vjust = -1, hjust = 0) +
  # Plots lrscale standard deviation
  geom_hline(yintercept = lrscale_sd, color = "red") +
  annotate(geom = "text", label = paste("Standard Deviation: ", lrscale_sd), colour = "red", x = 0, y = lrscale_sd, vjust = -1, hjust = 0) +
  # Plots lrscale mode
  geom_hline(yintercept = lrscale_mode, color = "green") +
  annotate(geom = "text", label = paste("Mode: ", lrscale_mode), colour = "green", x = 0, y = lrscale_mode, vjust = -1, hjust = 0)