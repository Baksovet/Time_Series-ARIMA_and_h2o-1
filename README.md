# Temperature_Forecasting
In this project, we study the daily minimum temperatures time series in Melbourne, Australia. The
data set consists of 3650 observations. It records the weather change from 1981 to 1990. We will
use two models to fit the data and test their performance:
1. Build h2o::automl(). For this task:
• prepare data using tk_augment_timeseries_signature()
• set stopping metric to “RMSE”
• set exclude_algos = c("DRF", "GBM","GLM",'XGBoost')
2. Build modeltime::arima_reg(). For this task set engine to “auto_arima”
3. Forecast temperatures for next year with model which has lower RMSE.

# You wrote "3" twice
