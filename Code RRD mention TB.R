data_bac <- read.csv("C:/Users/arthu/OneDrive/Documents/Arthur/M2/Master Thesis/Data/Panel2007_bac.csv", header = TRUE, sep = ";")
data_brevet <- read.csv("C:/Users/arthu/OneDrive/Documents/Arthur/M2/Master Thesis/Data/Panel2007_brevet.csv", header = TRUE, sep = ";")

data_b_and_b <- merge(
  data_brevet, 
  data_bac, 
  by = "NELEV", 
  all = FALSE, # Keeps only matching observations
  suffixes = c("_BREV", "_BAC") # To handle variable naming if duplicates exist
)

data_rdd1 <- data_b_and_b[, c("NELEV", "MOY2_BREV2011", "MOY1_BAC2014")]

data_rdd1 <- na.omit(data_rdd1)

cutoff16 <- 10
data_rdd1$treatment <- ifelse(data_rdd1$MOY2_BREV2011 >= cutoff16, 1, 0)

library(ggplot2)

ggplot(data_rdd1, aes(x = MOY2_BREV2011, y = MOY1_BAC2014)) +
  geom_point(alpha = 0.5) +
  geom_vline(xintercept = cutoff16, linetype = "dashed", color = "red") +
  theme_minimal() +
  labs(title = "Bac vs Brevet Variable with Cutoff")

library(rdd)

# Create the RDD object
rdd1_object <- RDestimate(MOY1_BAC2014 ~ MOY2_BREV2011, data = data_rdd1, cutpoint = cutoff16, bw = 0.5)

# Print the results
summary(rdd1_object)

# Plot the fitted values
plot(rdd1_object)


# Define bandwidth and bin size
bandwidth <- 0.5# Adjust as needed

library(dplyr)

# Filter data within the specified bandwidth
data_rdd1_bandwidth <- data_rdd1 %>%
  filter(MOY2_BREV2011 >= (cutoff16 - bandwidth) & MOY2_BREV2011 <= (cutoff16 + bandwidth))

# Plot with binned means for each interval

ggplot(data_rdd1_bandwidth, aes(x = MOY2_BREV2011, y = MOY1_BAC2014, color = factor(treatment))) +
  stat_summary_bin(
    fun = mean,                # Calculate the mean for each bin
    geom = "point",            # Plot mean as points
    bins = 20,                 # Set the number of bins (adjust as needed)
    alpha = 0.5
  ) +
  geom_smooth(method = "lm", formula = y ~ x, se = FALSE) +
  geom_vline(xintercept = cutoff16, linetype = "dashed", color = "red") +
  theme_minimal() +
  labs(title = "RDD Plot with Automatic Binning", x = "Running Variable", y = "Mean Outcome")

#Bandwidth 0.5 bins 20
#cutoff 11 : 0.08 not stat sign
#cutoff 12 : 0.2040 not stat sign
#cutoff 13 : 0.0865 not stat sign
#cutoff 14 : 0.4819 stat sign at 1%
#cutoff 15 : -0.2676 not stat sign
#cutoff 16 : 0.007415 not stat sign
#cutoff 17 : -0.071855 not stat sign
#cutoff 18 : 0.2949 not stat sign
