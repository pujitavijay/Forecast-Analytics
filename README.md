# Solar Irradiance Forecasting

## Project Overview
This forecast analytics project focuses on predicting the Plane-of-Array (POA) Irradiance using various forecasting models. The dataset was collected from a fully digitized solar testbed located in the Energy Lab at Richard Weeks Hall of Engineering. The target variable POA is forecasted using several models implemented in R.

## Dataset Description
The dataset includes the following variables:
- **DATE_TIME**: Date and time information
- **AIRTEMP**: Air temperature in Celsius
- **RH_AVG**: Humidity in percentage
- **DEWPT**: Dew point temperature in Celsius
- **WS**: Wind speed in meters per second
- **GHI**: Global Horizontal Irradiance measured in W/m² from a horizontal pyranometer mounted on a sun tracker
- **DNI**: Direct Normal Irradiance measured in W/m² from a horizontal pyranometer mounted on a sun tracker
- **DIFF**: Diffuse Irradiance measured in W/m² from a horizontal pyranometer mounted on a sun tracker
- **POA**: Plane-of-Array Irradiance measured in W/m² from a pyranometer aligned with the tilt of the solar panels

## Forecasting Models
The following models are used to forecast POA:
- **Multiple Linear Model**
- **ARIMA-X Model**
- **Dynamic Regression Model**
- **Random Forest Model**

## Language used -R
