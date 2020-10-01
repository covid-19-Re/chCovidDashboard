#########################
# Plotting scripts
#
# monica.golumbeanu@unibas.ch
########################
library(plyr)

plot_simple_forecast = function(plot_df, data_df, plot_title, plot_y){
    colnames(data_df) = c("day_reported", "n")
    options(scipen=1000000)
    p = ggplot() + 
        # Data
        # geom_line(data = log_pos_cases, aes(x = eingang_dt, y = n), color = "grey") +
        geom_point(data = data_df, aes(x = day_reported, y = n), color = "grey") +
        geom_point(data = plot_df, aes(x = Time, y = Value), color = "#cc4c02", size = 0.5) +
        geom_line(data = plot_df, aes(x = Time, y = Value), color = "#cc4c02") +
        geom_ribbon(data = plot_df, aes(x = Time, ymin = Low , ymax = High),
                    fill = "#cc4c02", alpha = 0.2) +
        # Forecasts all data
        geom_vline(xintercept = as.Date("2020-03-17"), lty = 2) + theme_bw() +
        labs( x = "Time", y = plot_y, title = plot_title)  
    return(p)
}

plot_all_arimas = function(melted_tab, data_tab) {
    # Plot the data for various data sources
    plot_types = c("cases", "deaths", "hospitalisations", "ICUs")
    plot_titles = c("Reported cases", "Deaths", "Hospitalisations", "ICU") 
    p_array = vector('list', length(plot_types))
    for (i in c(1:length(plot_types))){
        plot_df = melted_tab[which(melted_tab$Measure == plot_types[i]), ]
        data_df = data_tab[, c("day_reported", plot_types[i])]
        if(length(which(plot_df$value<0))>0){
            plot_df = plot_df[-which(plot_df$value<0),]
        }
        if(length(which(data_df[,2]<0))>0){
            data_df = data_df[-which(data_df[,2]<0),]
        }
        p_array[[i]] = plot_simple_forecast(plot_df, data_df, plot_titles[i], "")
    }
    ggarrange(plotlist = p_array, ncol = 2, nrow = 2)
    # return(figure)
}

# Plotting function for the Shiny app
# Plots the input data, as well as ARIMA model fits and forecasts
render_plot_ts = function(arima_result, data_df, ts_name, smoothed, 
                          show_model, show_confidence) {
    arima_res = arima_result[[ts_name]]
    data_df = data_df[,c("day_reported", ts_name)]
    if(smoothed) {
        data_df[, ts_name] =  ma_middle(data_df[, ts_name])
        # Remove end dates with NA entries due to smoothing
        data_df = data_df[-c((nrow(data_df)-2):nrow(data_df)),]
    } 
    model_df= arima_res$all_ts
    
    # Create graphics when model is shown
    if(show_model) {
        model_df = model_df[-c(1:4),]
        # Create graphics if uncertainty is shown
        if (show_confidence) {
            disp_conf = geom_ribbon(data = model_df, aes(x = Time, ymin = Low , ymax = High),
                                    fill = "#cc4c02", alpha = 0.2) 
        } else {
            disp_conf = NULL
        }
        disp_model = list(
            geom_point(data = model_df[,], aes(x = Time, y = Value), color = "#cc4c02", size = 1),
            geom_line(data = model_df[,], aes(x = Time, y = Value), color = "#cc4c02") ,
            disp_conf)
    } else {
        disp_model = NULL
    }
    
    p = ggplot() + 
        # Data
        geom_bar(stat = "identity", data = data_df, 
                 aes_string(x = "day_reported", y = ts_name), 
                 color = "grey", fill = "white") +
        # Model
        disp_model +
        # Graphics
        geom_vline(xintercept = as.Date("2020-03-17"), lty = 2) + 
        theme_bw(base_size = 11) +
        theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
        labs( x = "Time", y = paste0("Daily reported ", ts_name))
    return(p)
}

# Plot all the selected time series given visualisation options and overall 
# result table
render_plots_ts = function(arima_result_all, o_data_source, o_ts, 
                           o_data_disp, o_model, o_conf, o_max_time){
    # Transform date entries to date format
    arima_result_all$day_reported = ymd(arima_result_all$day_reported)
    
    # Select the rows in the data table according to visualization options
    selected_data = arima_result_all[which(arima_result_all$variable %in% 
                                               C_ts_vec_names[as.double(o_ts)] &
                                         arima_result_all$data_source %in% 
                                             C_data_sources[as.double(o_data_source)]),]
    
    # Select whether displayed data is smoothed or not
    if (o_data_disp == 1){
        selected_data$data_shown = selected_data$data_value
    } else {
        selected_data$data_shown = selected_data$smoothed_value
    }
    
    # Update the names in the data table
    selected_data$variable = revalue(selected_data$variable, C_ts_vec_titles)
    
    # Define the model graphics 
    if(o_model == FALSE){
        disp_model = NULL
    } else {
        # Create graphics if uncertainty is shown
        if (o_conf) {
            disp_conf = geom_ribbon(data = selected_data[which(!is.na(selected_data$model_fit_low)),], 
                                    aes(x = day_reported, ymin = model_fit_low, 
                                        ymax = model_fit_high,
                                        fill = data_source),
                                    alpha = 0.2)
        } else {
            disp_conf = NULL
        }
        disp_model = list(
            geom_point(data = selected_data, aes(x = day_reported, y = model_fit, 
                       color = data_source), size = 1),
            geom_line(data = selected_data, aes(x = day_reported, y = model_fit, 
                      color = data_source)),
            disp_conf)
    }
    
    # Define the final graphics
    p = ggplot() + 
        # Data
        geom_bar(stat = "identity", position = "identity", data = selected_data, 
                 aes(x = day_reported, y = data_shown, 
                 fill = data_source), color = "grey", size = 0.2, alpha = 0.2) +
        # Model
        disp_model +
        # Graphics
        geom_vline(xintercept = as.Date("2020-03-17"), lty = 2) + 
        theme_bw(base_size = 11) +
        facet_wrap(~variable, scales = "free", ncol = 1) +
        theme(panel.grid.major = element_blank(), 
              panel.grid.minor = element_blank()) +
        theme(strip.background = element_rect(colour="white", fill="white")) +
        scale_color_manual(name = "data_source", values = C_col_data_sources) +
        scale_fill_manual(name = "data_source", values = C_col_data_sources) +
        labs(x = "Time", y = "") +
        geom_vline(xintercept = as.numeric(o_max_time), lty = 2, color = "grey") +
        theme(legend.position="bottom") +
        guides(fill = guide_legend(title=""), color = guide_legend(title="")) +
        theme(strip.text.x = element_text(size = 12, face = "bold")) +
        guides(color = FALSE, size = FALSE)
    
    return(p)
}