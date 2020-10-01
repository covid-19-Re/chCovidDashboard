######################
# Define constants valid throughout the app
#
# monica.golumbeanu@unibas.ch
######################

# Define the vector of time series names, should have the same length, names
# and order as the "Displayed data" checkbox
C_ts_vec_names = c("cases", "hospitalizations", "curr_hosp", "deaths")
# Define the associated labels with the various time series
C_ts_vec_titles = c("cases" = "New daily cases", 
                    "hospitalizations" = "New daily hospitalizations", 
                    "curr_hosp" = "Current hospitalizations", 
                    "deaths" = "New daily fatalities")

# Data sources
C_data_sources = c("FOPH online", "corona-data.ch")

# Colors for the data sources
C_categories_data_sources = factor(C_data_sources, levels = C_data_sources)
C_col_data_sources = c("#fb6a4a", "#42B3D5")
names(C_col_data_sources) = levels(C_categories_data_sources)

