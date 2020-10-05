#########################
# Scripts for time series analysis:
# Trends and forecasting with ARIMA models
#
# monica.golumbeanu@unibas.ch
#########################
library(scales)
library(tidyverse)
library(tseries)
library(forecast)
library(lubridate)

# Apply a 7-day-window smoothing to the time series data
ma = function(x, n = 7){
    ma_vec = stats::filter(x, rep(1 / n, n), sides = 1)
    return(ma_vec)
}

# Apply a 7-day-window smoothing to the time series data
ma_middle = function(x, n = 7){
    ma_vec = stats::filter(x, rep(1 / n, n), sides = 2)
    return(ma_vec)
}

# Apply smoothing to specified columns in a data frame
apply_ma = function(tab_df, col_names) {
    for (col_name in col_names) {
        tab_df[, col_name] = c(ma(tab_df[, col_name]))
    }
    return(as.data.frame(tab_df))
}


#####################################
# Fit an ARIMA model to a time series
# INPUTS: 
# ts_data = data frame where the first column is the time/date (in Date format) and the second
#               column is the corresponding measure (number of cases, hospitalisations, etc.)
# forecast_days = number of days to be forecasted, default is 1
# moving_avg = specify whether a moving average transformation should be applied or not, default is TRUE
# remove_last = specify whether the last data point should be removed or not, default is TRUE
# OUTPUTS:
# data = input time series data
# arima_model = fitted arima model
# arima_fits = model fits including uncertainty
# arima_forecast = forecasted time series
#####################################
fit_arima_model_date = function(ts_data, until_date, forecast_days = 1, moving_avg = TRUE, 
                                remove_last = TRUE) {
    
    # Retain raw data
    raw_data = ts_data
    measure_name = colnames(raw_data)[2]
    
    # Remove last entry (replace it with previous value) if specified
    if (remove_last) {
        ts_data[nrow(ts_data), 2] = ts_data[(nrow(ts_data)-1), 2]
    }
    
    # Apply a moving average if specified
    if(moving_avg) {
        ts_data[,2] = ma_middle(ts_data[,2])
        # Remove end dates with NA entries due to smoothing
        ts_data = ts_data[-c((nrow(ts_data)-2):nrow(ts_data)),]
        colnames(ts_data) = c("day_reported", "smoothed_value")
    }
    
    # Extract data to fit until the specified date and log transform to additive 
    last_ts_data = ts_data[nrow(ts_data), 1]
    if(until_date < last_ts_data) {
        ext_ts_data = ts_data[which(ts_data[,1] <= until_date),]
        data_to_fit = log10(ext_ts_data[,2])
    } else {
        ext_ts_data = ts_data
        data_to_fit = log10(ext_ts_data[,2])
    }
    
    # Calculate the day of the last prediction
    last_data = ext_ts_data[nrow(ext_ts_data), 1]
    # print(last_data)
    last_prediction = last_data + forecast_days
    
    
    # Fit an ARIMA model to the time series
    data_to_fit[which(!is.finite(data_to_fit))] = 0
    arima_model = auto.arima(data_to_fit)
    # print(colnames(ts_data))
    # print(arima_model)
    fitted_all_df = cbind(ext_ts_data[,1],
                          c(10^fitted(arima_model)) * c(ext_ts_data[,2] > 0),
                          c(10^(fitted(arima_model) - 2*arima_model$sigma2))* c(ext_ts_data[,2] > 0),
                          c(10^(fitted(arima_model) + 2*arima_model$sigma2)) * c(ext_ts_data[,2] > 0)) 
    
    colnames(fitted_all_df) = c("day_reported", "Value", "Low", "High")
    
    # Forecast
    pred_arima_all = as.data.frame(forecast(arima_model, h = forecast_days))
    forecast_times = seq.Date(as.Date(last_data) + 1, as.Date(last_prediction), by = "day")
    forecast_arima = cbind(forecast_times, 10.^pred_arima_all)
    colnames(forecast_arima) = c("day_reported", "Value", "Low", "High", "Low2", "High2")
    forecast_arima = forecast_arima[, c("day_reported", "Value", "Low", "High")]
    
    # Fits and forecast
    all_data = rbind(fitted_all_df, forecast_arima)
    all_data$day_reported = c(ext_ts_data[,1], forecast_arima$day_reported)
    
    # Results data table, obtained by merging: model fits and forecasts, raw data, smoothed data
    results_tab = Reduce(function(...) merge(..., by = "day_reported", all = TRUE), 
                         list(all_data, raw_data, ts_data))
    results_tab = cbind.data.frame(results_tab, measure_name) 
    colnames(results_tab) = c("day_reported", "model_fit", "model_fit_low", 
                              "model_fit_high", "data_value", "smoothed_value", "variable")
    
    return(list(arima_model = arima_model, 
                results_table = results_tab))
}

#############################################
# Run the ARIMA analysis for each time series (column) in a data frame
# and return the models. The models are trained on data until a specified date
#############################################
run_arima_all_date = function(ts_df, last_date, n_days, ma_opt, remove_last) {
    # Run the arima analysis for each output measure and data source
    final_tab = NULL
    output_vars = unique(ts_df$variable)
    data_source_vars = unique(ts_df$data_source)
    for (o_var in output_vars) {
        for (ds_var in data_source_vars) {
            extracted_ts = ts_df[which(ts_df$variable == o_var &
                                           ts_df$data_source == ds_var), c("day_reported", "value")]
            if(nrow(extracted_ts) >0 ) {
                colnames(extracted_ts) = c("day_reported", o_var)
                arima_analysis = fit_arima_model_date(extracted_ts, until_date = last_date, 
                                                      forecast_days = n_days, moving_avg = ma_opt, 
                                                      remove_last = remove_last) 
                res_tab = arima_analysis$results_table
                res_tab$data_source = ds_var
                final_tab = rbind.data.frame(final_tab, res_tab)
            }
        }
    }
    return(final_tab)
}

