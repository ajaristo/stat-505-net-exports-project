

# File: `STAT 505 - Project Code - Master.R`
# Purpose: Contains helper functions for data processing
# Authors: Aris Aristorenas, Kevin Baritua, Luqmaan Adeel, Zozo Betshwana
# Created: 2025-03-21
# Notes: This file includes R code programming for Team 1 in STAT 505 (Winter 2025)
# Related files: `helpers.R` contains the helper functions used from STAT 505


# Load all libraries
library(readr)
library(forecast)
library(tseries)
library(ggplot2)
library(tseries)
library(zoo)
library(readxl)
library(lubridate)
library(tidyr)
library(dplyr)



# clear the workspace and functions
rm(list=ls())


# Load the helper functions defined in helpers.R
source("helpers.R")


# ============================================================================================
# ------------------------------------1. DATA INGESTION---------------------------------------                            
# ============================================================================================

# a. Import raw data into data frame
raw.df <- read_excel("Statistics Canada Imports and Exports Raw Data.xlsx", sheet = "Sheet1")

# b. Check for missing data
colSums(is.na(raw.df))
anyNA(raw.df)

# ============================================================================================
# ------------------------------------2. FEATURE ENGINEERING----------------------------------                           
# ============================================================================================

# a. Construction of Net Export (NX) Target Variable
NX = raw.df$`Import, Customs, Unadjusted`/raw.df$`Export, Customs, Unadjusted`

# b. Extraction of time features (Year and Month)
Year <- year(raw.df$`Seasonal adjustment`)
Month <- month(raw.df$`Seasonal adjustment`)

# c. Construction of final modeling data frame
df <- data.frame(
  Month = as.integer(Month),
  Year = as.integer(Year),
  NX = NX
)

df <- df[-nrow(df), ] # Remove last row, January 2025

# d. Check for negative values, and check for zeros 
sum(df$NX < 0)

sum(df$NX <= 0)
df[df$NX <= 0, ]

# e. Create another data frame with factors (for exploratory data analysis)
df.factor <- df
df.factor$Year <- as.factor(df.factor$Year)
df.factor$Month <- as.factor(df.factor$Month)

# ============================================================================================
# ------------------------------------3. EXPLORATORY DATA ANALYSIS----------------------------------                           
# ============================================================================================

# a. Summary Statistics
summary(df)

# b. Variance, SD, Range, and length of NX

# c. Raw time series plot
ggplot(df, aes(x = Year + (Month - 1) / 12, y = NX)) +
  geom_line(color = "black", size = 1) +
  labs(title = "Raw Time Series Plot of NX",
       x = "Date", y = "NX (Exports / Imports)") +
  theme_minimal()

# d. Event markers with time series

df$Date <- as.Date(paste(df$Year, df$Month, "01", sep = "-"))

event_dates <- as.Date(c("2020-03-01","2001-09-01", "2008-09-01","2022-02-01", "1994-01-01","1989-10-01","1997-07-01" ))
event_labels <- c("COVID-19", "9/11", "Global Financial Crisis", "Russia-Ukraine War", "NAFTA enacted (Canada, US, Mexico)", "Canada–US Free Trade Agreement (CUSFTA)","Asian Financial Crisis")

ggplot(df, aes(x = Date, y = NX)) +
  geom_line(color = "black", size = 1) +
  # Add vertical lines at event dates
  geom_vline(xintercept = event_dates, linetype = "dashed", color = "red") +
  # Add text annotations slightly above the line
  annotate("text", x = event_dates, 
           y = rep(max(df$NX, na.rm = TRUE) * 0.95, length(event_dates)), 
           label = event_labels,
           angle = 90, vjust = -0.25, size = 3, color = "red") +
  labs(title = "NX Time Series with Key Global Events",
       x = "Date", y = "NX (Exports / Imports)") +
  theme_minimal()

# e. Seasonal and Yearly Patterns via Boxplots
ggplot(df.factor, aes(x = Month, y = NX)) +
  geom_boxplot(fill = "lightblue") +
  labs(title = "Box plot of Net Exports (by Month)",
       x = "Month",
       y = "Net Exports (NX)") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggplot(df.factor, aes(x = factor(Year), y = NX)) +
  geom_boxplot(fill = "lightblue") +
  labs(title = "Box plot of Net Exports (by Year)",
       x = "Year",
       y = "Net Exports (NX)") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

# f. Histogram of Net Exports
ggplot(df, aes(x = NX)) +
  geom_histogram(fill = "steelblue", color = "black", bins = 30) +
  labs(title = "Histogram of Net Exports (NX)",
       x = "Net Exports (NX)",
       y = "Frequency") +
  theme_minimal()



# g. Initial running mean plot
nx_ts <- ts(df$NX, start = c(df$Year[1], df$Month[1]), frequency = 12)

running_mean <- rollmean(nx_ts, k = 12, fill = NA)

plot(nx_ts, main = "Running Mean Overlay", ylab = "NX", col = "black")
lines(running_mean, col = "blue", lwd = 2)

# h. Initial running variance plot
running_var <- rollapply(ts(df$NX, start = c(df$Year[1], df$Month[1]), frequency = 12), width = 12, FUN = var, fill = NA)

plot(running_var, type = "l", col = "darkred", lwd = 2,
     main = "12-Month Running Variance of NX",
     ylab = "Variance", xlab = "Time")


# ============================================================================================
# ------------------------------------4. DATA PRE-PROCESSING----------------------------------                           
# ============================================================================================

# a. Regime Selection
df.sub <- subset(df, Year > 2010 | (Year == 2010 & Month >= 1))

# b. Creation of time series object
nx_ts <- ts(
  df.sub$NX,
  start = c(df.sub$Year[1], df.sub$Month[1]),
  end = c(df.sub$Year[nrow(df.sub)], df.sub$Month[nrow(df.sub)]),
  frequency = 12
)

plot(nx_ts, main = "NX Time Series (Jan 2010 Onward)", ylab = "NX")

# c. Train-Test-Validation Split (Time Series Cross Validation Method)
dim(df.sub)

# Total length and test split
n_total <- nrow(df.sub)
n_test <- 12

# Use all but last 12 points for training + cross-validation (ie. leave 1 year for forecast)
train_ts <- ts(df.sub$NX[1:(n_total - n_test)],
               start = c(df.sub$Year[1], df.sub$Month[1]),
               frequency = 12)

test_ts <- ts(df.sub$NX[(n_total - n_test + 1):n_total],
              start = c(df.sub$Year[n_total - n_test + 1], df.sub$Month[n_total - n_test + 1]),
              frequency = 12)


# d. Quality check of split 

# Extract date ranges from the ts objects
train_start <- time(train_ts)[1]
train_end   <- time(train_ts)[length(train_ts)]

test_start  <- time(test_ts)[1]
test_end    <- time(test_ts)[length(test_ts)]

# Convert fractional time to Year-Month
to_year_month <- function(x) {
  year <- floor(x)
  month <- round((x - year) * 12 + 1)
  paste(year, sprintf("%02d", month), sep = "-")
}

# Build summary data frame (no validation anymore)
split_summary <- data.frame(
  Set = c("Train", "Test"),
  Length = c(length(train_ts), length(test_ts)),
  Start = c(to_year_month(train_start), to_year_month(test_start)),
  End = c(to_year_month(train_end), to_year_month(test_end))
)

print(split_summary)

# e. Plot of train-test-validation split

# Define how many tail points to treat as validation
n_val_tail <- 40
n_train_main <- length(train_ts) - n_val_tail

# Split training into main training and validation portion
train_main_ts <- window(train_ts, 
                        end = time(train_ts)[n_train_main])
val_tail_ts <- window(train_ts, 
                      start = time(train_ts)[n_train_main + 1])

# Plot main training data
plot(train_main_ts, col = "blue", lwd = 2, 
     main = "NX Time Series: Train, Validation (Tail), and Test Sets", 
     ylab = "NX", xlab = "Time", 
     xlim = range(time(train_ts), time(test_ts)))

# Add validation (last 20 points of training)
lines(val_tail_ts, col = "orange", lwd = 2, lty = 3)

# Add test data
lines(test_ts, col = "red", lwd = 2, lty = 2)

# Add legend
legend("topleft", legend = c("Train", "Validation", "Test"), 
       col = c("blue", "orange", "red"), 
       lwd = 2, lty = c(1, 3, 2))


# ============================================================================================
# ------------------------------------5. STAIONARITY ANALYSIS----------------------------------                           
# ============================================================================================


# a. Preliminary ACF/PACF
par(mfrow = c(2,1))
acf(train_ts, lag = 48, main = "ACF of NX (Training set)")
pacf(train_ts, lag = 48, main = "PACF of NX (Training set)")

# b. Augmented Dickey-Fuller Test
adf.test(train_ts)

# c. Running mean plot (updated for selected regime of 2010 - 2024)
running_mean <- stats::filter(train_ts, rep(1/12, 12), sides = 2)
plot(train_ts, main = "Running Mean Overlay", ylab = "NX", xlab = "Time")
lines(running_mean, col = "blue", lwd = 2)
legend("topright", legend = c("NX", "12-Month Running Mean"),
       col = c("black", "blue"), lwd = 2, lty = 1)

# same plot on a larger range of the data to show it's close to constant
running_mean <- stats::filter(train_ts, rep(1/12, 12), sides = 2)
plot(train_ts,
     main = "Running Mean Overlay (Fixed Y-Axis)", ylab = "NX", xlab = "Time",
     ylim = c(0, 1.5)) # Adjust range as you see fit

lines(running_mean, col = "blue", lwd = 2)
legend("topright", legend = c("NX", "12-Month Running Mean"),
       col = c("black", "blue"), lwd = 2, lty = 1)

# d. Running variance plot (updated for selected regime of 2010 - 2024)
rolling_var <- rollapply(train_ts, width = 12, FUN = var, fill = NA)
plot(rolling_var, type = "l", col = "red",
     main = "12-Month Rolling Variance", ylab = "Variance", xlab = "Time")
abline(h = mean(rolling_var, na.rm = TRUE), col = "darkgray", lty = 2)
legend("topright", legend = c("Rolling Variance", "Mean Variance"),
       col = c("red", "darkgray"), lwd = 2, lty = c(1, 2))

# e. Check non-seasonal differencing (repeat a, b, c, d)
diff1 <- diff(train_ts)

par(mfrow = c(2,1))
acf(diff1, main = "ACF after Differencing (d = 1)")
pacf(diff1, main = "PACF after Differencing (d = 1)")

adf.test(diff1)

running_mean_d1 <- zoo::rollmean(diff1, k = 12, fill = NA)
plot(running_mean_d1, type = "l", main = "Running Mean (d = 1)", ylab = "Mean")

running_var_d1 <- zoo::rollapply(diff1, width = 12, FUN = var, fill = NA)
plot(running_var_d1, type = "l", main = "Running Variance (d = 1)", ylab = "Variance")

# f. Check seasonal differencing (repeat a, b, c, d)
diff_seasonal <- diff(train_ts, lag = 12)

par(mfrow = c(2,1))
acf(diff_seasonal, main = "ACF after Seasonal Differencing (D = 1)")
pacf(diff_seasonal, main = "PACF after Seasonal Differencing (D = 1)")

adf.test(diff_seasonal)

running_mean_D1 <- zoo::rollmean(diff_seasonal, k = 12, fill = NA)
plot(running_mean_D1, type = "l", main = "Running Mean (D = 1)", ylab = "Mean")

running_var_D1 <- zoo::rollapply(diff_seasonal, width = 12, FUN = var, fill = NA)
plot(running_var_D1, type = "l", main = "Running Variance (D = 1)", ylab = "Variance")

# g. Check both (d = 1) and (D = 1) (repeat a, b, c, d)
diff_both <- diff(diff(train_ts), lag = 12)

par(mfrow = c(2,1))
acf(diff_both, main = "ACF after Differencing (d=1, D=1)")
pacf(diff_both, main = "PACF after Differencing (d=1, D=1)")

adf.test(diff_both)

running_mean_both <- zoo::rollmean(diff_both, k = 12, fill = NA)
plot(running_mean_both, type = "l", main = "Running Mean (d=1, D=1)", ylab = "Mean")

running_var_both <- zoo::rollapply(diff_both, width = 12, FUN = var, fill = NA)
plot(running_var_both, type = "l", main = "Running Variance (d=1, D=1)", ylab = "Variance")



# ============================================================================================
# ------------------------------------6. VARIANCE STABILIZATION----------------------------------                           
# ============================================================================================

# Compute original rolling variance again
rolling_var_orig <- rollapply(train_ts, width = 12, FUN = var, fill = NA) 

# a. Square-Root Transformation
sqrt_train_ts <- sqrt(train_ts)

# Compute original rolling variance again
rolling_var_sqrt <- rollapply(sqrt_train_ts, width = 12, FUN = var, fill = NA)
# Plot square root variance first
plot(rolling_var_sqrt, type = "l", col = "purple",
     main = "Rolling Variance: Original vs Square Root Transformed",
     ylab = "Variance", xlab = "Time",
     ylim = c(0, 0.015))
# Overlay original variance
lines(rolling_var_orig, col = "red", lwd = 1.5, lty = 2)
# Mean lines (optional)
abline(h = mean(rolling_var_sqrt, na.rm = TRUE), col = "gray", lty = 2)
abline(h = mean(rolling_var_orig, na.rm = TRUE), col = "darkred", lty = 3)
# Add legend
legend("topright",
       legend = c("Square Root", "Original", "Mean (sqrt)", "Mean (orig)"),
       col = c("purple", "red", "gray", "darkred"),
       lwd = 2, lty = c(1, 2, 2, 3))
       
    

# b. Log Transformation
log_train_ts <- log(train_ts)
log_var <- zoo::rollapply(log_train_ts, width = 12, FUN = var, fill = NA)

plot(log_var, type = "l", col = "blue",
     main = "Rolling Variance: Original vs 12-Month Rolling Variance (Log-transformedd)",
     ylab = "Variance", xlab = "Time",
     ylim = c(0, 0.015))
# Overlay original
lines(rolling_var_orig, col = "red", lwd = 1.5, lty = 2)
# Add mean lines
abline(h = mean(log_var, na.rm = TRUE), col = "gray", lty = 2)
abline(h = mean(rolling_var_orig, na.rm = TRUE), col = "darkred", lty = 3)

# Legend
legend("topright",
       legend = c("Log-Transformed", "Original", "Mean (log)", "Mean (orig)"),
       col = c("blue", "red", "gray", "darkred"),
       lwd = 2, lty = c(1, 2, 2, 3))


# c. Box-Cox Transformation

# Find optimal lambda and apply Box-Cox
lambda_bc <- 1.5
boxcox_train_ts <- BoxCox(train_ts, lambda_bc)

rolling_var_orig <- rollapply(train_ts, width = 12, FUN = var, fill = NA)

# Compute rolling variance for Box-Cox transformed series
rolling_var_boxcox <- rollapply(boxcox_train_ts, width = 12, FUN = var, fill = NA)

# Plot Box-Cox variance first
plot(rolling_var_boxcox, type = "l", col = "darkgreen",
     main = paste("Rolling Variance: Original vs Box-Cox (Lambda =", round(lambda_bc, 3), ")"),
                  ylab = "Variance", xlab = "Time",
                  ylim = c(0, 0.015))

# Overlay original variance
lines(rolling_var_orig, col = "red", lwd = 1.5, lty = 2)

# Mean lines
abline(h = mean(rolling_var_boxcox, na.rm = TRUE), col = "gray", lty = 2)
abline(h = mean(rolling_var_orig, na.rm = TRUE), col = "darkred", lty = 3)

# Legend
legend("topright",
      legend = c("Box-Cox Transformed", "Original", "Mean (Box-Cox)", "Mean (orig)"),
      col = c("darkgreen", "red", "gray", "darkred"),
      lwd = 2, lty = c(1, 2, 2, 3))



# ============================================================================================
# ------------------------------------7. SEASONALITY ANALYSIS----------------------------------                           
# ============================================================================================


# a. Picking s using domain knowledge, literature

# b. Observing the plots from data exploration

# c. Buys-ballot table with aggregate Months, and years

# Convert train_ts to a data frame with Year and Month
train_df <- data.frame(
  Value = as.numeric(train_ts),
  Date = as.Date(time(train_ts))
)
train_df$Year <- as.numeric(format(train_df$Date, "%Y"))
train_df$Month <- factor(format(train_df$Date, "%b"), levels = month.abb)

# Calculate monthly means
monthly_means <- train_df %>%
  group_by(Month) %>%
  summarize(Mean_Month = mean(Value, na.rm = TRUE)) %>%
  # Pivot to have months as columns
  pivot_wider(names_from = Month, values_from = Mean_Month)

# Calculate yearly means
yearly_means <- train_df %>%
  group_by(Year) %>%
  summarize(Mean_Year = mean(Value, na.rm = TRUE))

# Reshape for buys-ballot table
buys_ballot_table <- pivot_wider(train_df, id_cols = Year, names_from = Month, values_from = Value)

# Merge yearly means to the buys-ballot table
buys_ballot_table <- left_join(buys_ballot_table, yearly_means, by = "Year")

# Create the monthly means row with "Mean_Month" in the Year column and NA for Mean_Year
monthly_means_row <- data.frame(
  Year = "Mean_Month",
  monthly_means[1, ],
  Mean_Year = NA  # Add a placeholder for the Mean_Year column
)

# Ensure column names match for rbind (order might matter)
colnames(monthly_means_row) <- colnames(buys_ballot_table)

# Add monthly means as a row at the bottom
buys_ballot_table <- rbind(buys_ballot_table, monthly_means_row)

# Calculate the total aggregate mean
total_aggregate_mean <- mean(train_ts, na.rm = TRUE)

# Find the column index of "Mean_Year"
mean_year_col_index <- which(colnames(buys_ballot_table) == "Mean_Year")

# Get the last row index
last_row_index <- nrow(buys_ballot_table)

# Assign the total aggregate mean to the bottom-right cell using indices
if (length(mean_year_col_index) > 0) {
  buys_ballot_table[last_row_index, mean_year_col_index] <- total_aggregate_mean
} else {
  print("Warning: 'Mean_Year' column not found in buys_ballot_table.")
}

# Print the full buys-ballot table with means
print("Buys-Ballot Table with Means:")
print(buys_ballot_table)




# d. Heat map of Buys-Ballot
# Prepare the data for ggplot2
heatmap_data <- buys_ballot_table %>%
  mutate(Year = as.character(Year)) %>% # Ensure Year is treated as a categorical variable
  pivot_longer(cols = Jan:Dec, names_to = "Month", values_to = "Ratio") %>%
  mutate(Month = factor(Month, levels = month.abb)) # Ensure months are ordered correctly

# Create the heatmap
heatmap_plot <- ggplot(heatmap_data, aes(x = Month, y = Year, fill = as.numeric(Ratio))) +
  geom_tile(color = "white") + # Add cell borders
  scale_fill_gradient2(low = "blue", mid = "white", high = "red",
                       midpoint = mean(as.numeric(heatmap_data$Ratio), na.rm = TRUE),
                       name = "Ratio") +
  labs(title = "Buys-Ballot Heatmap of Imports/Exports Ratio",
       x = "Month",
       y = "Year") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  coord_equal() # Ensure cells are square

# Print the heatmap
print(heatmap_plot)

###################

# Prepare the data for ggplot2 (excluding the "Mean_Month" row)
heatmap_data <- buys_ballot_table %>%
  filter(Year != "Mean_Month") %>%
  mutate(Year = as.character(Year)) %>% # Ensure Year is treated as a categorical variable
  pivot_longer(cols = Jan:Dec, names_to = "Month", values_to = "Ratio") %>%
  mutate(Month = factor(Month, levels = month.abb)) # Ensure months are ordered correctly

# Prepare data for the yearly means
yearly_means_data <- buys_ballot_table %>%
  filter(Year != "Mean_Month") %>%
  select(Year, Mean_Year) %>%
  mutate(Year = as.character(Year))

# Create the base heatmap plot
heatmap_plot_base <- ggplot(heatmap_data, aes(x = Month, y = Year, fill = as.numeric(Ratio))) +
  geom_tile(color = "white") + # Add cell borders
  scale_fill_gradient2(low = "blue", mid = "white", high = "red",
                       midpoint = mean(as.numeric(heatmap_data$Ratio), na.rm = TRUE),
                       name = "Ratio") +
  labs(title = "Buys-Ballot Heatmap of Imports/Exports Ratio with Yearly Means",
       x = "Month",
       y = "Year") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  coord_equal() # Ensure cells are square

# Add the text layer for yearly means
heatmap_plot_final <- heatmap_plot_base +
  geom_text(
    data = yearly_means_data,
    mapping = aes(
      x = 13.5,
      y = Year,
      label = round(Mean_Year, 2)
    ),
    hjust = 0.5,
    vjust = 0.5,
    size = 3,
    inherit.aes = FALSE # PREVENT INHERITING AESTHETICS
  ) +
  # Add a label for the yearly means column
  annotate("text", x = 13.5, y = max(as.numeric(heatmap_data$Year)) + 0.5,
           label = "Yearly Mean", hjust = 0.5, size = 3) +
  # Expand the x-axis to accommodate the yearly means label
  scale_x_discrete(limits = c(month.abb, "Yearly Mean")) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Print the heatmap
print(heatmap_plot_final)


# e. Seasonal plot
seasonplot(train_ts, col = rainbow(12), year.labels = TRUE, main = "Seasonal Plot of NX")


# f. STL decomposition with plot
plot(stl(train_ts, s.window = "periodic"))

# g. Month plot
monthplot(train_ts, main = "month plot of imports/exports ratio")

# h. Spectral Density Method

# i. ANOVA testing
#H0: There is no statistically significant difference in NX across months.
#Ha: At least 1 month has a statistically different mean of NX across months

df.sub.anova <- df.sub
df.sub.anova$Month <- factor(df.sub.anova$Month, levels = 1:12, labels = month.abb)

anova_result <- aov(NX ~ Month, data = df.sub.anova)
summary(anova_result)

TukeyHSD(anova_result)

plot(TukeyHSD(anova_result), las = 1, col = "steelblue")


# ============================================================================================
# ------------------------------------8. MODELING----------------------------------                           
# ============================================================================================

### a. Arima Auto model

# Fit auto.arima on the training set
model1.auto <- auto.arima(train_ts, seasonal = TRUE)

# View the model summary
summary(model1.auto)


# b. Manual Interpretation of (p, d, q)x(P, D, Q)_12 using STAT 505 methods from class
acf(train_ts, main = "ACF of Original Series")
pacf(train_ts, main = "PACF of Original Series")

# Try non-seasonal differencing
par(mfrow = c(2,1))
diff1 <- diff(train_ts)
acf(diff1, main = "ACF after Differencing (d = 1)")
pacf(diff1, main = "PACF after Differencing (d = 1)")

# Try seasonal differencing
par(mfrow = c(2,1))
seasonal_diff <- diff(train_ts, lag = 12)
acf(seasonal_diff, main = "ACF after Seasonal Differencing (D = 1)")
pacf(seasonal_diff, main = "PACF after Seasonal Differencing (D = 1)")

# Try both (d = 1) and (D = 1)
diff_d1 <- diff(train_ts, differences = 1)
diff_d1_D1 <- diff(diff_d1, lag = 12)
par(mfrow = c(2,1))
acf(diff_d1_D1, main = "ACF: d = 1, D = 1")
pacf(diff_d1_D1, main = "PACF: d = 1, D = 1")

# Fit the interpreted SARIMA model
model2.custom <- Arima(train_ts,
                       order = c(4, 1, 1),
                       seasonal = list(order = c(2, 0, 0), period = 12),
                       include.mean = FALSE)

# View the model summary
summary(model2.custom)

# c. BoxCox-Transformed data fit with Auto.Arima()
lambda_bc <- 1.5
model3.boxcox_auto <- auto.arima(train_ts,
                            seasonal = TRUE,
                            lambda = lambda_bc)

summary(model3.boxcox_auto)

# d. BoxCox-Transformed data fit with STAT 505 Manual Interpretation methods
new_train_ts <- (train_ts^lambda_bc - 1) / lambda_bc
model4.boxcox_custom = arima(new_train_ts, 
                             order=c(3,1,2), 
                             seasonal = list(order = c(2,0,0), period = 12))
summary(model4.boxcox_custom)

# e. Square-root Transformed data fit with Auto ARIMA
sqrt_train_ts <- sqrt(train_ts)
model5.sqrt <- auto.arima(sqrt_train_ts, seasonal = TRUE)

# View the model summary
summary(model5.sqrt)



# ============================================================================================
# ------------------------------------9. MODEL DIAGNOSTICS----------------------------------                           
# ============================================================================================


# a. Model diagnostics for Auto ARIMA model (model1.auto)
mytsplot(model1.auto$residuals)
tsdiag(model1.auto)
par(mfrow = c(2,1))
acf(model1.auto$residuals)
pacf(model1.auto$residuals)
checkresiduals(model1.auto)

# b. Model diagnostics for Interpreted ARIMA model (model2.custom)
tsdiag(model2.custom)
par(mfcol = c(2, 1))
acf(model2.custom$residuals)
pacf(model2.custom$residuals)
checkresiduals(model2.custom$residuals)
qqnorm(residuals(model2.custom)); qqline(residuals(model2.custom), col = "red")
hist(residuals(model2.custom), breaks = 20, col = "skyblue", main = "Residuals Histogram (Model 2)", xlab = "Residuals")

# c. Model diagnostics for BoxCox-Transformed data fitted with Auto ARIMA (model3.boxcox_auto)
mytsplot(model3.boxcox_auto$residuals)
tsdiag(model3.boxcox_auto)
checkresiduals(model3.boxcox_auto)

# d. Model diagnostics for BoxCox-Transformed data fitted with manual interpretation (model4.boxcox_custom)
par(mfrow = c(2,1))
acf(model4.boxcox_custom$residuals)
pacf(model4.boxcox_custom$residuals)
tsdiag(model4.boxcox_custom)
checkresiduals(model4.boxcox_custom$residuals)
qqnorm(residuals(model4.boxcox_custom)); qqline(residuals(model4.boxcox_custom), col = "red")
hist(residuals(model4.boxcox_custom), breaks = 20, col = "skyblue", main = "Residuals Histogram (Model 4)", xlab = "Residuals")


# e. Model diagnostics for Square-Root model


# ============================================================================================
# ------------------------------------10. MODEL SELECTION----------------------------------                           
# ============================================================================================


# a. AIC/BIC criteria
AIC(model1.auto, model2.custom, model3.boxcox_auto, model4.boxcox_custom, model5.sqrt)
BIC(model1.auto, model2.custom, model3.boxcox_auto, model4.boxcox_custom, model5.sqrt)

# b. MAE, RMSE, MAPE on validation set

# Helper function to compute rolling 1-step-ahead forecast errors and metrics
rolling_forecast_metrics <- function(model, data, n_val = 40, transform = NULL, inverse = NULL) {
  errors <- numeric(n_val)
  actuals <- numeric(n_val)
  forecasts <- numeric(n_val)
  
  for (i in 1:n_val) {
    idx <- length(data) - n_val + i
    history <- window(data, end = time(data)[idx - 1])
    actual <- data[idx]
    
    # Apply transformation if needed
    if (!is.null(transform)) history <- transform(history)
    
    # Forecast 1-step ahead using the fixed model
    fc <- forecast(model, h = 1)
    forecast_value <- fc$mean[1]
    
    # Back-transform forecast if needed
    if (!is.null(inverse)) {
      forecast_value <- inverse(forecast_value)
    }
    
    forecasts[i] <- forecast_value
    actuals[i] <- actual
    errors[i] <- actual - forecast_value
  }
  
  rmse <- sqrt(mean(errors^2, na.rm = TRUE))
  mae <- mean(abs(errors), na.rm = TRUE)
  mape <- mean(abs(errors / actuals), na.rm = TRUE) * 100
  
  return(c(RMSE = rmse, MAE = mae, MAPE = mape))
}

lambda = 1.5

inv_boxcox <- function(x, lambda) {
  if (lambda == 0) {
    return(exp(x))
  } else {
    return((lambda * x + 1)^(1 / lambda))
  }
}

boxcox_fwd <- function(x, lambda) {
  if (lambda == 0) {
    return(log(x))
  } else {
    return((x^lambda - 1) / lambda)
  }
}

# Run for all models
metrics1 <- rolling_forecast_metrics(model1.auto,   train_ts)
metrics2 <- rolling_forecast_metrics(model2.custom, train_ts)
metrics3 <- rolling_forecast_metrics(model3.boxcox_auto,   train_ts)
metrics4 <- rolling_forecast_metrics(
  model4.boxcox_custom,
  train_ts,
  transform = function(x) boxcox_fwd(x, lambda),
  inverse = function(x) inv_boxcox(x, lambda)
)

metrics5 <- rolling_forecast_metrics(model5.sqrt,   train_ts,
                                     transform = sqrt,
                                     inverse = function(x) x^2)





# Combine into a table
metrics_table <- rbind(
  model1.auto   = metrics1,
  model2.custom = metrics2,
  model3.sqrt   = metrics3,
  model4.log    = metrics4,
  model5.boxcox = metrics5
)

print(round(metrics_table, 5))



# c. Parsimony
count_params <- function(model) { arma <- model$arma
p1 <- arma[1]
p2 <- arma[2]
p3 <- arma[3]
p4 <- arma[4]
p6 <- arma[6]
p7 <- arma[7]
p_total <- p1 + p2 + p3 + p4 + p6 + p7
return(p_total)
}

cat("model1.auto:", count_params(model1.auto), "\n")
cat("model2.custom:  ", count_params(model2.custom), "\n")
cat("model3.boxcox_auto:  ", count_params(model3.boxcox_auto), "\n")
cat("model4.boxcox_custom:   ", count_params(model4.boxcox_custom), "\n")
cat("model5.sqrt:   ", count_params(model5.sqrt), "\n")



# d. Domain Knowledge

# e. Identification of best model



# ============================================================================================
# ------------------------------------11. FORECASTING----------------------------------                           
# ============================================================================================

# a. Point forecasts with confidence bounds for all 5 models

h <- length(test_ts)

# Forecast for Model 1: model1.auto
fcast1 <- forecast(model1.auto, h = h, level = c(80, 95))

autoplot(fcast1) +
  ggtitle("Forecast: model1.auto") +
  xlab("Time") + ylab("NX")

# Forecast for Model 2: model2.custom
fcast2 <- forecast(model2.custom, h = h, level = c(80, 95))

autoplot(fcast2) +
  ggtitle("Forecast: model2.custom") +
  xlab("Time") + ylab("NX")

# Forecast for Model 3: model3.boxcox
h <- length(test_ts)
fcast3 <- forecast(model3.boxcox_auto, h = h, level = c(80, 95))

autoplot(fcast3) +
  autolayer(test_ts, series = "Actual 2024", color = "red") +
  ggtitle("Forecast vs Actuals - Model 3 (Original Scale)") +
  xlab("Time") +
  ylab("NX (Exports / Imports)") +
  theme_minimal()

# Forecast for Model 4: model4.boxcox_custom
forecast_data <- forecast(model4.boxcox_custom, h = length(test_ts))

inv_boxcox <- function(x, lambda) {
  if (lambda == 0) {
    return(exp(x))
  } else {
    return((lambda * x + 1)^(1 / lambda))
  }
}

# Apply inverse transformation to the forecast object components
forecast_data_transformed <- forecast_data
forecast_data_transformed$mean <- inv_boxcox(forecast_data$mean, lambda_bc)
forecast_data_transformed$upper <- inv_boxcox(forecast_data$upper, lambda_bc)
forecast_data_transformed$lower <- inv_boxcox(forecast_data$lower, lambda_bc)
forecast_data_transformed$fitted <- inv_boxcox(forecast_data$fitted, lambda_bc) 

fitted_values_original_scale <- inv_boxcox(new_train_ts, lambda_bc)
fitted_ts_original_scale <- ts(fitted_values_original_scale, start = start(train_ts), 
                               frequency = frequency(train_ts))

print("Forecast Mean (Original Scale) - After Transformation:")
head(forecast_data_transformed$mean)
print("Forecast Upper (Original Scale) - After Transformation:")
head(forecast_data_transformed$upper)
print("Forecast Lower (Original Scale) - After Transformation:")
head(forecast_data_transformed$lower)

autoplot(train_ts) +  # Plot the original training data
  autolayer(fitted_ts_original_scale, series = "Fitted (Original Scale)", color = "blue") +
  autolayer(forecast_data_transformed, series = "Forecast (Original Scale)", color = "red") +
  autolayer(test_ts, series = "Actual 2024", color = "blue") + 
  ggtitle("ARIMA Forecast vs Actuals (Original Scale)") +
  ylab("NX (Exports/Imports)") +
  xlab("Year") +
  theme_minimal()


# Forecast for Model 5: model5.sqrt
forecast_model5 <- forecast(model5.sqrt, h = length(test_ts))

forecast_model5_transformed <- forecast_model5
forecast_model5_transformed$mean <- forecast_model5$mean^2
forecast_model5_transformed$upper <- forecast_model5$upper^2
forecast_model5_transformed$lower <- forecast_model5$lower^2
forecast_model5_transformed$fitted <- forecast_model5$fitted^2

fitted_ts_model5_original_scale <- ts((fitted(model5.sqrt))^2, start = start(train_ts), frequency = 12)

autoplot(fitted_ts_model5_original_scale, series = "Fitted (Original Scale)", color = "black") +
  autolayer(forecast_model5_transformed, series = "Forecast (Original Scale)", color = "blue") +
  autolayer(test_ts, series = "Actual 2024", color = "red") +
  ggtitle("Model 5 Forecast vs Actuals (Original Scale)") +
  ylab("NX (Exports/Imports)") +
  xlab("Year") +
  theme_minimal()



# b. Simulated paths for all models
set.seed(123)

plot_forecast_with_simulations <- function(model, model_name, h = length(test_ts), n_sim = 10, transform = NULL, lambda = NULL) {
  # Forecast
  fc <- forecast(model, h = h)
  
  # Simulate n paths
  simulated_paths <- replicate(n_sim, simulate(model, nsim = h, future = TRUE))
  
  # Back-transform forecast and paths if needed
  if (!is.null(transform)) {
    if (transform == "sqrt") {
      fc$mean  <- fc$mean^2
      fc$lower <- fc$lower^2
      fc$upper <- fc$upper^2
      simulated_paths <- simulated_paths^2
    } else if (transform == "log") {
      fc$mean  <- exp(fc$mean)
      fc$lower <- exp(fc$lower)
      fc$upper <- exp(fc$upper)
      simulated_paths <- exp(simulated_paths)
    } else if (transform == "boxcox") {
      # Do nothing: forecast() already back-transforms if lambda was used in fitting
    } else if (transform == "boxcox_custom"){
      fc$mean <- inv_boxcox(fc$mean, lambda_bc)
      fc$upper <- inv_boxcox(fc$upper, lambda_bc)
      fc$lower <- inv_boxcox(fc$lower, lambda_bc)
      fc$fitted <- inv_boxcox(fc$fitted, lambda_bc) 
      simulated_paths <- inv_boxcox(simulated_paths, lambda_bc)
    }
  }
  
  # Plot mean forecast
  plot(fc$mean, type = "l", col = "black", lwd = 2,
       ylim = range(simulated_paths, fc$lower, fc$upper),
       main = paste("Simulated Paths vs Forecast -", model_name),
       ylab = "NX", xlab = "Time")
  
  # Add 95% confidence bounds
  lines(fc$lower[, 2], col = "gray", lty = 2)
  lines(fc$upper[, 2], col = "gray", lty = 2)
  
  # Add simulated paths
  for (i in 1:n_sim) {
    lines(ts(simulated_paths[, i], start = end(train_ts) + c(0, 1), frequency = 12),
          col = rainbow(n_sim)[i], lty = 1)
  }
  
  # Re-add the forecast mean line
  lines(fc$mean, col = "black", lwd = 2)
}

# Call for all 5 models
plot_forecast_with_simulations(model1.auto,   "model1.auto")
plot_forecast_with_simulations(model2.custom, "model2.custom")
plot_forecast_with_simulations(model3.boxcox_auto,    "model3.boxcox_auto",  transform = "boxcox")
plot_forecast_with_simulations(model4.boxcox_custom, "model4.boxcox_custom", transform = "boxcox_custom")
plot_forecast_with_simulations(model5.sqrt,   "model5.sqrt", transform = "sqrt")



# c. Comparison on test data set for all models

library(ggplot2)
library(forecast)

# Start with autoplot using any forecast to initialize the ggplot object
p <- autoplot(fcast1$mean, series = "Model 1: Auto") +
  autolayer(test_ts, series = "Actual 2024", linetype = "dashed", size = 1) +
  autolayer(fcast2$mean, series = "Model 2: Custom") +
  autolayer(fcast3$mean, series = "Model 3: BoxCox Auto") +
  autolayer(forecast_data_transformed$mean, series = "Model 4: BoxCox Custom") +
  autolayer(forecast_model5_transformed$mean, series = "Model 5: Sqrt") +
  ggtitle("Forecast Comparison for All Models (2024)") +
  xlab("Time") + ylab("NX (Exports/Imports)") +
  theme_minimal()

# Now add manual styling for legend entries
p + scale_color_manual(
  values = c(
    "Actual 2024" = "black",
    "Model 1: Auto" = "blue",
    "Model 2: Custom" = "green",
    "Model 3: BoxCox Auto" = "orange",
    "Model 4: BoxCox Custom" = "purple",
    "Model 5: Sqrt" = "brown"
  )
) +
  scale_linetype_manual(
    values = c(
      "Actual 2024" = "dashed",
      "Model 1: Auto" = "solid",
      "Model 2: Custom" = "solid",
      "Model 3: BoxCox Auto" = "solid",
      "Model 4: BoxCox Custom" = "solid",
      "Model 5: Sqrt" = "solid"
    )
  ) +
  guides(colour = guide_legend(title = "Series"),
         linetype = guide_legend(title = "Series"))





# ============================================================================================
# ------------------------------------12. MODEL EVALUATION----------------------------------                           
# ============================================================================================

# a. Calculation of MAE, RMSE on test set

# Compute accuracy for all models on test_ts
acc3 <- accuracy(fcast3, test_ts)
acc4 <- accuracy(forecast_data_transformed, test_ts)
acc5 <- accuracy(forecast_model5_transformed, test_ts)

# b. Assessment of overfitting/underfitting

# c. Comparison of all models

# ============================================================================================
# ------------------------------------13. CONCLUDING REMARKS----------------------------------                           
# ============================================================================================

# a. Which model performed the best?











































