library(dplyr)
library(fpp3)
library(ggplot2)
library(tidyverse)
library(forecast)
library(lubridate)
library(readxl)
library(tsibble)
library(fable)
library(ranger)
library(randomForest)
library(GGally)
library(openxlsx)
library(urca)

# Reading data----------
Data_S4 <- read_excel("")
Segmented_Average <- read.csv("Segmented_Average_.csv")
tsibble_Data_S4 <- tsibble(Data_S4, index = DATE_TIME)

#Data Exploration-----------
tsibble_Data_S4 %>% autoplot(POA) + labs(title = "Time plot of POA", y = "POA")

#Seasonal Plot for POA
tsibble_Data_S4 |> gg_season(POA, period = "month") + labs(title = "Seasonal plot (Month) of POA", y = "POA")
tsibble_Data_S4 |> gg_season(POA, period = "week") + labs(title = "Seasonal plot (Week) of POA", y = "POA")
tsibble_Data_S4 |> gg_season(POA, period = "day") + labs(title = "Seasonal plot (Day) of POA", y = "POA")

#correlation
tsibble_Data_S4 |> select(-DATE_TIME) |> GGally::ggpairs(column = 2:8)

#Multiple Linear Regresssion----------
LinearModel <- tsibble_Data_S4 |> model(TSLM(POA ~ AIRTEMP + RH_AVG + DEWPT + WS + GHI + DNI + DIFF + trend()))
report(LinearModel)

ggplot(data = tsibble_Data_S4, aes(x = DATE_TIME, y = POA)) +
  geom_line(aes(color = "Data"), size = 1) + 
  geom_line(data = augment(LinearModel), aes(x = DATE_TIME, y = .fitted, color = "Fitted"), size = 1) +
  labs(x = "DATE_TIME", y = "POA", title = "POA vs. Time") +
  scale_color_manual(values = c("Data" = "blue", "Fitted" = "red"), name = "") +
  theme_minimal()

#tsibble_Data_S4 %>% autoplot(POA) + autolayer(augment(LinearModel),.fitted,col = "blue")

LM_RESI = augment(LinearModel) %>% select(.resid)
ACF(LM_RESI) %>% autoplot()

#Forecast with segmented averages
segmented_averages <- tsibble_Data_S4 %>%
  mutate(
    time_of_day = format(DATE_TIME, format = "%H:%M") # Extract hour:minute part
  ) %>%
  group_by(time_of_day) %>%
  summarise(
    avg_AIRTEMP = mean(AIRTEMP, na.rm = TRUE),
    avg_RH_AVG = mean(RH_AVG, na.rm = TRUE),
    avg_DEWPT = mean(DEWPT, na.rm = TRUE),
    avg_WS = mean(WS, na.rm = TRUE),
    avg_GHI = mean(GHI, na.rm = TRUE),
    avg_DNI = mean(DNI, na.rm = TRUE),
    avg_DIFF = mean(DIFF, na.rm = TRUE)
  ) %>%
  ungroup() 

Segmented_Average$DATE_TIME <- mdy_hm(Segmented_Average$DATE_TIME)
Segmented_Averages <- as_tsibble(Segmented_Average, index = DATE_TIME)
MLR_FORECASTS <- LinearModel |> forecast(new_data = Segmented_Averages)
MLR_FORECASTS


#ARIMAX------------
ARIMAX_model = tsibble_Data_S4 |> model(ARIMA(POA ~ AIRTEMP + RH_AVG + DEWPT + WS + GHI + DNI + DIFF)) 
report(ARIMAX_model)
augment(ARIMAX_model)

tsibble_Data_S4 %>% autoplot(POA) + autolayer(augment(ARIMAX_model),.fitted,col = "blue")
ARIMAX_RESI = augment(ARIMAX_model) %>% select(.resid)
ACF(ARIMAX_RESI) %>% autoplot()

#Forecast with segmented averages
# Assuming the last timestamp in your dataset is extracted correctly
last_timestamp <- max(tsibble_Data_S4$DATE_TIME)

# Generate the sequence of future time points, starting just after the last timestamp
future_time_points <- seq(from = last_timestamp + minutes(10), 
                          by = "10 min", 
                          length.out = 71)

Segmented_Averages_tsibble <- Segmented_Averages |> mutate(time = future_time_points) |> as_tsibble(index = time)
ARIMAX_FORECASTS <- ARIMAX_model |> forecast(new_data = Segmented_Averages_tsibble)
ARIMAX_FORECASTS

#Dynamic Regression-------------------
DR = tsibble_Data_S4 |> model(arxmod = ARIMA(POA ~ AIRTEMP + lag(AIRTEMP,1) + lag(AIRTEMP,2) + lag(AIRTEMP,3) + lag(AIRTEMP,4) + DEWPT + lag(DEWPT,1) + lag(DEWPT,2) + lag(DEWPT,3) + lag(DEWPT,4) + WS + lag(WS,1) + lag(WS,2) + lag(WS,3) + lag(WS,4) + GHI + lag(GHI,1) + lag(GHI,2) + lag(GHI,3) + lag(GHI,4) + DNI + lag(DNI,1) + lag(DNI,2) + lag(DNI,3) + lag(DNI,4) + DIFF + lag(DIFF,1) + lag(DIFF,2) + lag(DIFF,3) + lag(DIFF,4)))
report(DR)
augment(DR) |> autoplot(.resid)
augment(DR) |> ACF(.resid) |> autoplot()

#DR - forecasting with segmented averages
DR_FORECASTS <- DR |> forecast(new_data = Segmented_Averages_tsibble)
DR_FORECASTS

ggplot(data = tsibble_Data_S4, aes(x = DATE_TIME, y = POA)) +
  geom_line(aes(color = "Data"), size = 1) + 
  geom_line(data = augment(DR), aes(x = DATE_TIME, y = .fitted, color = "Fitted"), size = 1) +
  labs(x = "DATE_TIME", y = "POA", title = "POA vs. Time") +
  scale_color_manual(values = c("Data" = "blue", "Fitted" = "red"), name = "") +
  theme_minimal()

#tsibble_Data_S4 %>% autoplot(POA) + autolayer(augment(DR),.fitted,col = "blue")

DR_RESI = augment(DR) %>% select(.resid)
ACF(DR_RESI) %>% autoplot()

adf.test(DR_RESI$.resid)
ljung_box(DR_RESI$.resid)

#Random Forest---------
# Split the data into predictors (X) and target variable (Y)
X <- Data_S4[, c("AIRTEMP", "DEWPT", "WS", "RH_AVG", "GHI", "DNI", "DIFF")]
Y <- Data_S4$POA

# Split the data into training and testing sets
set.seed(123) # for reproducibility
train_data <- Data_S4 %>% filter(DATE_TIME < "2023-08-01 19:20:00")
test_data <- Data_S4 %>% filter(DATE_TIME >= "2023-08-01 19:20:00")

# Train the Random Forest model
rf_model <- randomForest(x = train_data[, c("AIRTEMP", "DEWPT", "WS", "RH_AVG", "GHI", "DNI", "DIFF")],
                         y = train_data$POA,
                         ntree = 60,     # Number of trees in the forest
                         mtry = 3)        # Number of variables randomly sampled as candidates at each split

# Predict on the test data
predictions <- predict(rf_model, newdata = test_data[, c("AIRTEMP", "DEWPT", "WS", "RH_AVG", "GHI", "DNI", "DIFF")])
# Calculate residuals
residuals <- predictions - test_data$POA
# Plot the ACF of the residuals
acf(residuals)

# Calculate Mean Squared Error (MSE)
mse <- mean(residuals^2)

# Calculate Root Mean Squared Error (RMSE)
rmse <- sqrt(mean(residuals^2))

# Calculate Mean Absolute Error (MAE)
mae <- mean(abs(residuals))

# Print the performance metrics
print(paste("Mean Squared Error:", mse))
print(paste("Root Mean Squared Error:", rmse))
print(paste("Mean Absolute Error:", mae))

# Ljung-Box Test
ljung_box_test <- Box.test(residuals, type = "Ljung-Box", lag = 20)
print(ljung_box_test)

# Create a tsibble with the forecasted values
forecast_tsibble <- Segmented_Averages_tsibble %>%
  mutate(POA_forecast = predict(rf_model, newdata = .))

# View the forecasted values
print(forecast_tsibble)

# Write data to an Excel file
write.xlsx(forecast_tsibble, file = "Forecast_RF_.xlsx")
write.xlsx(forecast_tsibble, file = ""