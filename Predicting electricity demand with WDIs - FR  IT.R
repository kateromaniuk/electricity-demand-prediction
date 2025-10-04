
# ----------------------------------------------------------------------------
install.packages(c("oRaklE", "dplyr", "lubridate", "ggplot2", "WDI", "patchwork", "gtools", "broom", "car", "purrr", "tidyverse", "stringr", "tidyr"))
lapply(c("oRaklE", "dplyr", "lubridate", "ggplot2", "WDI", "patchwork", "gtools", "broom", "car", "purrr", "tidyverse", "stringr", "tidyr"), library, character.only = TRUE)

setwd("~/Desktop/R")

# ----------------------------------------------------------------------------
# ----------------------------------------------------------------------------
# STEP 1 
# LOADING IN THE DATA

# 1) FRANCE
# ----------------------------------------------------------------------------
# a) ELECTRICITY DEMAND

# At first, I load the data in its original hourly format:
France_electricity <- get_entsoE_data(2017, 2024, "France")
France_electricity <- fill_missing_data(France_electricity, data_directory=getwd())
print(head(France_electricity))

# Then I decompose it into trend, seasonality, and residuals; and return a list with hourly, daily, weekly, and yearly summaries:
France_decomposed_electricity <- decompose_load_data(France_electricity, data_directory=getwd(), verbose=TRUE) 

# Yearly average demand (averaged across hours in each year)
France_yearly_avg <- France_decomposed_electricity$longterm %>%
  select(-country) %>%
  group_by(year) %>%
  summarise(hourly_demand = avg_hourly_demand)
print(France_yearly_avg)

# I created a function to get simple demand summary tables
demand_summary <- function(data, time_period, demand_column) {
  metrics <- c("min", "median", "max")
  summary <- lapply(metrics, function(m) {
    # Extract the demand values
    demand_values <- data[[demand_column]]
    # Calculate metrics
    value <- if (m == "median") {
      median(demand_values, na.rm = TRUE)
    } else {
      do.call(m, list(demand_values, na.rm = TRUE))
    }
    # Find row closest to the metrics
    row <- data %>% slice(which.min(abs(demand_values - value)))
    # Return result
    data.frame(
      metric = m,
      group = row[[time_period]],
      hourly_demand = row[[demand_column]]
    )
  }) %>% bind_rows()
  return(summary)}

# Summary table
demand_summary(France_yearly_avg, "year", "hourly_demand")

# Monthly average demand (averaged by month across ALL years)
France_monthly_avg <- France_decomposed_electricity$midterm %>%
  select(-country) %>%
  group_by(month) %>%
  summarise(hourly_demand = mean(avg_hourly_demand))
print(France_monthly_avg)
# Summary table
demand_summary(France_monthly_avg, "month", "hourly_demand")

# Hourly average demand (averaged by hour across ALL years)
France_hourly_avg <- France_decomposed_electricity$shortterm %>%
  select(-country) %>%
  mutate(hour = lubridate::hour(date)) %>%
  group_by(hour) %>%
  summarise(demand = mean(hourly_demand))
print(France_hourly_avg)
# Summary table
demand_summary(France_hourly_avg, "hour", "demand")

# Line plots
# At first, I created a function for making line plots
plot_demand <- function(data, time_column, demand_column, country, 
                        y_breaks = NULL, color = "orange") {
  
  stopifnot(time_column %in% names(data), demand_column %in% names(data))

  x_label <- switch(
    time_column,
    "year" = "Year",
    "month" = "Month",
    "hour" = "Hour of Day",
    time_column
  )

  title <- paste0(
    "Average Hourly Electricity Demand",
    if (time_column != "hour") paste0(" by ", stringr::str_to_title(time_column)) else "",
    " (", country, ")"
  )
  # Default y-axis breaks if none provided
  if (is.null(y_breaks)) {
    y_range <- range(data[[demand_column]], na.rm = TRUE)
    step <- round((y_range[2] - y_range[1]) / 6, -2)
    y_breaks <- seq(floor(y_range[1]), ceiling(y_range[2]), by = step)
  }
  # Default x-axis breaks
  x_breaks <- if (time_column == "year") {
    seq(min(data[[time_column]]), max(data[[time_column]]), 1)
  } else if (time_column == "month") {
    1:12
  } else if (time_column == "hour") {
    0:23
  } else {
    pretty(data[[time_column]])
  }

  ggplot(data, aes(x = .data[[time_column]], y = .data[[demand_column]])) +
    geom_line(color = color, size = 1.2) +
    geom_point(color = color, size = 2) +
    theme_minimal(base_size = 14) +
    theme(panel.background = element_rect(fill = "white"),
          plot.background = element_rect(fill = "white")) +
    scale_x_continuous(breaks = x_breaks) +
    scale_y_continuous(breaks = y_breaks) +
    labs(title = title, x = x_label, y = "Average Demand (MW)")
}

# Years
plot_demand(France_yearly_avg, "year", "hourly_demand", "France", y_breaks = seq(40000, 60000, by = 1000))

# Months
plot_demand(France_monthly_avg, "month", "hourly_demand", "France", y_breaks = seq(40000, 80000, by = 4000))

# Hours
plot_demand(France_hourly_avg, "hour", "demand", "France", y_breaks = seq(43000, 58000, by = 2000))

# ----------------------------------------------------------------------------

# b) WORLD DEVELOPMENT INDICATORS

# The following function will:
# - download the world development indicators for a country
# - fill any missing values based on the growth rate between the previous two years
# - calculate new columns needed for later regression analysis
# - rename the columns
# - and finally return a data frame with WDIs

get_clean_wdi <- function(country_code, start_year = 2017, end_year = 2024) {
  indicators <- c("SP.POP.TOTL", "NY.GDP.MKTP.KD", "NY.GDP.MKTP.KD.ZG", "NY.GDP.DEFL.KD.ZG",
                  "NY.GNP.MKTP.KD", "SP.RUR.TOTL", "NV.IND.TOTL.ZS", "NV.IND.MANF.ZS", "NV.SRV.TOTL.ZS", "NE.CON.PRVT.ZS")
  data <- WDI(
    country = country_code,
    indicator = indicators,
    start = start_year,
    end = end_year)
  # Function to fill in the missing values using extrapolation based on the previous year's growth rate
  fill_missing_growth <- function(x) {
    n <- length(x)
    while (is.na(x[n]) && n >= 3) {
      growth <- (x[n-1] - x[n-2]) / x[n-2]  # Last known growth rate
      x[n] <- x[n-1] * (1 + growth)         # Extrapolate
      n <- n - 1
    }
    return(x)}
  # Filling in the missing values
  data <- data %>%
    mutate(across(where(is.numeric), fill_missing_growth))
  # Creating new variables for later regression analysis
  data <- data %>%
    mutate(
      industrial_value_added = (NV.IND.TOTL.ZS / 100) * NY.GDP.MKTP.KD,
      manufacturing_value_added = (NV.IND.MANF.ZS / 100) * NY.GDP.MKTP.KD,
      service_value_added = (NV.SRV.TOTL.ZS / 100) * NY.GDP.MKTP.KD,
      household_consumption_expenditure = (NE.CON.PRVT.ZS / 100) * NY.GDP.MKTP.KD
    ) %>%
    select(
      iso2c, country, year, SP.POP.TOTL, NY.GDP.MKTP.KD, industrial_value_added, manufacturing_value_added,
      NY.GDP.MKTP.KD.ZG, NY.GDP.DEFL.KD.ZG, service_value_added, NY.GNP.MKTP.KD,
      household_consumption_expenditure, SP.RUR.TOTL
    ) %>%
    rename(
      total_population = SP.POP.TOTL,
      GDP = NY.GDP.MKTP.KD,
      GDP_growth_rate = NY.GDP.MKTP.KD.ZG,
      GDP_deflator = NY.GDP.DEFL.KD.ZG,
      GNI = NY.GNP.MKTP.KD,
      rural_population = SP.RUR.TOTL
    ) %>%
    select(-country, -iso2c) %>%
    mutate(
      log_population = log(total_population),
      log_GDP = log(GDP),
      log_industry = log(industrial_value_added),
      log_manufacturing = log(manufacturing_value_added),
      log_services = log(service_value_added),
      log_GNI = log(GNI),
      log_consumption = log(household_consumption_expenditure),
      log_rural = log(rural_population)
    )
  return(data)}

# Applying the function on the France dataset
France_wdi <- get_clean_wdi("FR")
print(names(France_wdi))
print(summary(France_wdi))

# ----------------------------------------------------------------------------

# c) Merge the electricity and WDI data into one data frame
France_data <- merge(France_yearly_avg, France_wdi, by = "year") %>%
  mutate(log_hourly_demand = log(hourly_demand))

print(head(France_data))

# ----------------------------------------------------------------------------

# 2) ITALY
# ----------------------------------------------------------------------------
# a) ELECTRICITY DEMAND

# Loading the data
Italy_electricity <- get_entsoE_data(2017, 2024, "Italy")
Italy_electricity <- fill_missing_data(Italy_electricity, data_directory=getwd())
print(head(Italy_electricity))

Italy_decomposed_electricity <- decompose_load_data(Italy_electricity, data_directory=getwd(), verbose=TRUE)

Italy_yearly_avg <- Italy_decomposed_electricity$longterm %>%
  select(-country) %>%
  group_by(year) %>%
  summarise(hourly_demand = avg_hourly_demand)
# Summary table
demand_summary(Italy_yearly_avg, "year", "hourly_demand")

Italy_monthly_avg <- Italy_decomposed_electricity$midterm %>%
  select(-country) %>%
  group_by(month) %>%
  summarise(hourly_demand = mean(avg_hourly_demand))
# Summary table
demand_summary(Italy_monthly_avg, "month", "hourly_demand")

Italy_hourly_avg <- Italy_decomposed_electricity$shortterm %>%
  select(-country) %>%
  mutate(hour = lubridate::hour(date)) %>%
  group_by(hour) %>%
  summarise(demand = mean(hourly_demand))
# Summary table
demand_summary(Italy_hourly_avg, "hour", "demand")

# Line plots
# Years
plot_demand(Italy_yearly_avg, "year", "hourly_demand", "Italy", y_breaks = seq(25000, 35000, by = 1000))

# Months
plot_demand(Italy_monthly_avg, "month", "hourly_demand", "Italy", y_breaks = seq(29000, 38000, by = 2000))

# Hours
plot_demand(Italy_hourly_avg, "hour", "demand", "Italy", y_breaks = seq(23000, 38000, by = 2000))

# ----------------------------------------------------------------------------

# b) WORLD DEVELOPMENT INDICATORS

Italy_wdi <- get_clean_wdi("IT")
print(names(Italy_wdi))
print(summary(Italy_wdi))

# ----------------------------------------------------------------------------

# c) Merge the electricity and WDI data into one data frame
Italy_data <- merge(Italy_yearly_avg, Italy_wdi, by = "year") %>%
  mutate(log_hourly_demand = log(hourly_demand))
print(head(Italy_data))

# ----------------------------------------------------------------------------
# ----------------------------------------------------------------------------
# STEP 2
# THE PREDICTION MODEL

# 1) FRANCE
# ----------------------------------------------------------------------------
# STEP 0: Generate all predictors combinations (from size = 1 to size = length of vector)
# ----------------------------------------------------------------------------
generate_all_combinations <- function(predictors) {
  all_combs <- unlist(
    lapply(1:length(predictors), function(x) {
      combn(predictors, x, simplify = FALSE)
    }),
    recursive = FALSE
  )
  return(all_combs)
}

predictors <- c("log_population", "log_GDP", "log_industry", "log_manufacturing", "log_services",
                "log_GNI", "log_consumption", "log_rural", "GDP_growth_rate", "GDP_deflator")
combs <- generate_all_combinations(predictors)
#print(head(combs, 15)) # 1023 combinations

# ----------------------------------------------------------------------------
# STEP 1: fit all models
# ----------------------------------------------------------------------------
# Function to fit all of the models
fit_all_models <- function(combinations, data, response_var = "hourly_demand") {
  map_df(combinations, function(predictors) {
    model_name <- paste(predictors, collapse = " + ")
    formula <- as.formula(paste(response_var, "~", model_name))
    
    model <- tryCatch(lm(formula, data = data), error = function(e) NULL)
    
    if (is.null(model)) {
      return(tibble(
        model = model_name,
        adj_r2 = NA,
        r2 = NA,
        vif = NA,
        bic = NA,
        intercept = NA,
        estimators = NA
      ))
    }
    
    model_summary <- summary(model)
    bic <- BIC(model)
    
    vif_values <- if (length(predictors) > 1) {
      tryCatch(mean(vif(model)), error = function(e) NA)
    } else {
      NA
    }
    
    coefs <- coef(model)
    estimators <- if (length(coefs) > 1) {
      paste(names(coefs)[-1], round(coefs[-1], 4), collapse = "; ")
    } else {
      NA
    }
    
    tibble(
      model = model_name,
      adj_r2 = model_summary$adj.r.squared,
      r2 = model_summary$r.squared,
      vif = vif_values,
      bic = bic,
      intercept = coefs[1],
      estimators = estimators
    )
  })
}

FR_fitted_models <- fit_all_models(combs, France_data)

# ----------------------------------------------------------------------------
# STEP 2: calculate MAPE (by splitting the data into the train and test sets first)
# ----------------------------------------------------------------------------

# Function to calculate MAPE (mean absolute percentage error)
mape <- function(actual, predicted) {
  mean(abs((actual - predicted) / actual)) * 100
}

# Function to add MAPE to a data set of regression models
add_mape <- function(data, model_table, response_var, train_years, test_years) {
  # Train/test split
  train_data <- subset(data, year >= train_years[1] & year <= train_years[2])
  test_data  <- subset(data, year >= test_years[1] & year <= test_years[2])
  
  model_table$mape <- NA_real_
  for (i in seq_len(nrow(model_table))) {
    model_terms <- model_table$model[i]
    model_formula <- as.formula(paste(response_var, "~", model_terms))
    
    lm_fit <- lm(model_formula, data = train_data)
    preds <- predict(lm_fit, newdata = test_data)
    
    model_table$mape[i] <- mape(test_data[[response_var]], preds)
  }
  return(model_table)
}

# Use the function on our data
FR_fitted_models <- add_mape(
  data = France_data,
  model_table = FR_fitted_models,
  response_var = "hourly_demand",
  train_years = c(2017, 2022),
  test_years = c(2023, 2024)
)

# ----------------------------------------------------------------------------
# STEP 3: filter promising models: adj_r2 > 0.5, vif <= 5, MAPE <= 2
# ----------------------------------------------------------------------------

# Function to filter models
filter_models <- function(model_table, adj_r2_thresh = 0.5, vif_thresh = 5, mape_thresh = 2) {
  filtered <- model_table %>%
    filter(adj_r2 > adj_r2_thresh) %>%
    filter(vif <= vif_thresh) %>%
    filter(mape <= mape_thresh)
  return(filtered)
}

FR_filtered_models <- filter_models(FR_fitted_models, adj_r2_thresh = 0.5, vif_thresh = 5, mape_thresh = 2)
print(FR_filtered_models) # 20 models

# Function to select top x
get_top_models <- function(filtered_models, top_n = 5) {
  top_models <- filtered_models %>%
    arrange(mape) %>%  # Sort by ascending MAPE (best accuracy first)
    slice_head(n = top_n) %>% 
    mutate(model_name = paste0("Model_", row_number()))
  return(top_models)
}

FR_top5_models <- get_top_models(FR_filtered_models, top_n = 5)
print(FR_top5_models)

print(FR_top5_models[,c("model","adj_r2","vif","bic","mape","model_name")])

# —> BEST 5 MODELS ARE CHOSEN, now I am going to estimate the best one by plotting the actual vs. predicted values

# ----------------------------------------------------------------------------
# STEP 4: fit the models & predict the existing values (2017-2024) using all 5 models
# ----------------------------------------------------------------------------
# Function to generate predictions
generate_predictions <- function(top_models, data, response_var = "hourly_demand") {
  preds_list <- list()
  
  for (i in seq_len(nrow(top_models))) {
    model_rhs <- top_models$model[i]
    model_formula <- as.formula(paste(response_var, "~", model_rhs))
    model_label <- top_models$model_name[i]
    
    fit <- tryCatch(
      lm(model_formula, data = data, na.action = na.exclude),
      error = function(e) { 
        warning(paste("Error fitting", model_label, ":", e$message))
        return(NULL) 
      }
    )
    if (is.null(fit)) next
    
    preds_df <- data %>%
      mutate(predicted = predict(fit, newdata = data),
             model_name = model_label) %>%
      select(year, !!sym(response_var), predicted, model_name)
    
    preds_list[[model_label]] <- preds_df
  }
  
  preds_all <- bind_rows(preds_list)
  return(preds_all)
}

FR_predictions <- generate_predictions(FR_top5_models, France_data)
print(FR_predictions)


# Function for plotting predictions vs actual
plot_actual_vs_predicted <- function(preds_df, 
                                     response_var = "hourly_demand", 
                                     year_var = "year",
                                     title = "Actual vs Predicted") {
  preds_long <- preds_df %>%
    pivot_longer(cols = all_of(c(response_var, "predicted")),
                 names_to = "type",
                 values_to = "value") %>%
    mutate(type = case_when(
      type == response_var ~ "Actual",
      type == "predicted" ~ "Predicted",
      TRUE ~ type
    ))
  
  p <- ggplot(preds_long, aes_string(x = year_var, y = "value", color = "type")) +
    geom_line(aes(linetype = type), size = 1) +
    geom_point(size = 2) +
    scale_color_manual(values = c("Actual" = "blue", "Predicted" = "red")) +
    scale_linetype_manual(values = c("Actual" = "solid", "Predicted" = "dashed")) +
    geom_vline(xintercept = 2024.5, linetype = "dotted", color = "black") +
    facet_wrap(~ model_name, scales = "free_y", ncol = 1) +
    scale_x_continuous(breaks = 2017:2024) +
    labs(title = title,
         x = "Year",
         y = response_var,
         color = "Legend",
         linetype = "Legend") +
    theme_minimal() +
    theme(legend.position = "right",
          strip.text = element_text(face = "bold"))
  print(p)
}

plot_actual_vs_predicted(FR_predictions,
                         response_var = "hourly_demand",
                         year_var = "year",
                         title = "Actual vs Predicted hourly_demand (2017–2024) — Top 5 Models")

# Results analysis: Model 1 and Model 2 have the biggest deviations -> I choose to omit them

# Now let's look at the Models 3, 4, and 5
print(FR_top5_models)

# Model 5 has the highest MAPE -> I choose to discard it
# Meanwhile, the difference between Model 3 and Model 4 is tiny
# I assume that predictions of total population for 2025-2028 will be more precise than those of the rural population (as rural is a predicted % of total)
# Therefore, I choose to predict the future energy demand using Model 3

# ----------------------------------------------------------------------------
# STEP 5: predicting the future values (2025-2028)
# ----------------------------------------------------------------------------
# Fit the model manually
model_fit <- lm(hourly_demand ~ log_population + log_industry + GDP_growth_rate, data = France_data)

# Enter the predicted exogenous variables needed for the model
FR_future <- data.frame(
  year = 2025:2028,
  log_population = log(c(68606000, 68960000, 69320000, 69680000)),
  log_industry = log(c(481320000000, 488540000000, 495870000000, 503310000000)),
  GDP_growth_rate = c(1.1, 1.2, 1.2, 1.2)
)
#print(FR_future)

# Generate predictions using the model
FR_predictions <- predict(model_fit, newdata = FR_future, interval = "prediction", level = 0.95)
print(FR_predictions)

# Add the year column
FR_predictions <- cbind(FR_future,
                        predicted_hourly_demand = FR_predictions[, "fit"],
                        lower_PI = FR_predictions[, "lwr"],
                        upper_PI = FR_predictions[, "upr"])
print(FR_predictions)

# Plot 2025-2028
ggplot(FR_predictions, aes(x = year, y = predicted_hourly_demand)) +
  geom_line(color = "blue", size = 1) +
  geom_point(color = "red", size = 2) +
  labs(title = "Predicted Hourly Demand over Years",
       x = "Year", y = "Predicted Hourly Demand") +
  theme_minimal()

# Create the combined dataset
FR_actual_data <- France_data[, c("year", "hourly_demand")]
FR_plot_data <- bind_rows(
  FR_actual_data %>% rename(hourly_demand = hourly_demand) %>% mutate(type = "Actual"),
  FR_predictions %>% rename(hourly_demand = predicted_hourly_demand) %>% mutate(type = "Predicted")
)

# Plot actual (2017-2024) and future (2025-2028) predictions on one graph 
ggplot(FR_plot_data, aes(x = year, y = hourly_demand, color = type)) +
  geom_ribbon(
    data = FR_predictions,
    aes(x = year, ymin = lower_PI, ymax = upper_PI),
    fill = "pink", alpha = 0.5, inherit.aes = FALSE
  ) +
  geom_line(aes(linetype = type), size = 1) +
  geom_point(size = 2) +
  scale_color_manual(values = c("Actual" = "blue", "Predicted" = "red")) +
  scale_linetype_manual(values = c("Actual" = "solid", "Predicted" = "dashed")) +
  geom_vline(xintercept = 2024.5, linetype = "dotted", color = "black") +
  scale_y_continuous(limits = c(42000, 56000), breaks = seq(42000, 56000, by = 2000)) +
  scale_x_continuous(breaks = seq(min(FR_plot_data$year), max(FR_plot_data$year), by = 1)) +
  labs(title = "Actual and Predicted Hourly Demand in France",
       x = "Year",
       y = "Hourly Demand",
       color = "Legend",
       linetype = "Legend") +
  theme_minimal() +
  theme(legend.position = "right")

# ----------------------------------------------------------------------------


# 2) ITALY
# ----------------------------------------------------------------------------
# STEP 0: Generate all predictors combinations (from size = 1 to size = length of vector)
# ----------------------------------------------------------------------------
predictors <- c("log_population", "log_GDP", "log_industry",
                "log_manufacturing", "log_services",
                "log_GNI", "log_consumption", "log_rural",
                "GDP_growth_rate", "GDP_deflator")
combs <- generate_all_combinations(predictors)

# ----------------------------------------------------------------------------
# STEP 1: fit all models
# ----------------------------------------------------------------------------
IT_fitted_models <- fit_all_models(combs, Italy_data)

# ----------------------------------------------------------------------------
# STEP 2: calculate MAPE (by splitting the data into the train and test sets first)
# ----------------------------------------------------------------------------
IT_fitted_models <- add_mape(
  data = Italy_data,
  model_table = IT_fitted_models,
  response_var = "hourly_demand",
  train_years = c(2017, 2022),
  test_years = c(2023, 2024)
)

# ----------------------------------------------------------------------------
# STEP 3: filter promising models: adj_r2 > 0.5, vif <= 5, MAPE <= 2
# ----------------------------------------------------------------------------
IT_filtered_models <- filter_models(IT_fitted_models, adj_r2_thresh = 0.5, vif_thresh = 5, mape_thresh = 2)
print(IT_filtered_models) # 14 models

IT_top5_models <- get_top_models(IT_filtered_models, top_n = 5)
print(IT_top5_models)

print(IT_top5_models[,c("model","adj_r2","vif","bic","mape","model_name")])

# ----------------------------------------------------------------------------
# STEP 4: fit the models & predict the existing values (2017-2024) using all 5 models
# ----------------------------------------------------------------------------
IT_predictions <- generate_predictions(IT_top5_models, Italy_data)
print(IT_predictions)

plot_actual_vs_predicted(IT_predictions,
                         response_var = "hourly_demand",
                         year_var = "year",
                         title = "Actual vs Predicted hourly_demand (2017–2024) — Top 5 Models")

# It is visible on the graph, that Model 2 has the lowest deviations from the actual values
# Therefore, I choose to make the predictions on its basis

# ----------------------------------------------------------------------------
# STEP 5: predicting the future values (2025-2028)
# ----------------------------------------------------------------------------
# Fit the model manually
model_fit <- lm(hourly_demand ~ log_manufacturing + log_rural + GDP_deflator, data = Italy_data)

# Enter the predicted exogenous variables needed for the model
IT_future <- data.frame(
  year = 2025:2028,
  log_manufacturing = log(c(
    323746918215.0,  
    329250615824.65497, 
    335177126909.4988,  
    341210315193.86975  
  )),
  log_rural = log(c(
    16265221.5, 
    16137030.0, 
    15994420.0, 
    15852170.0   
  )),
  GDP_deflator = c(1.8, 1.6, 1.5, 1.5)
)
#print(IT_future)

# Generate predictions using the model
IT_predictions <- predict(model_fit, newdata = IT_future, interval = "prediction", level = 0.95)
print(IT_predictions)

IT_predictions <- cbind(IT_future, predicted_hourly_demand = IT_predictions[, "fit"],
                        lower_PI = IT_predictions[, "lwr"], upper_PI = IT_predictions[, "upr"])
print(IT_predictions)

# Plot 2025-2028
ggplot(IT_predictions, aes(x = year, y = predicted_hourly_demand)) +
  geom_line(color = "blue", size = 1) +
  geom_point(color = "red", size = 2) +  # optional points
  labs(title = "Predicted Hourly Demand over Years",
       x = "Year", y = "Predicted Hourly Demand") +
  theme_minimal()

IT_actual_data <- Italy_data[, c("year", "hourly_demand")]
IT_plot_data <- bind_rows(
  IT_actual_data %>% rename(hourly_demand = hourly_demand) %>% mutate(type = "Actual"),
  IT_predictions %>% rename(hourly_demand = predicted_hourly_demand) %>% mutate(type = "Predicted")
)

# Plot actual (2017-2024) and future (2025-2028) predictions on one graph 
ggplot(IT_plot_data, aes(x = year, y = hourly_demand, color = type)) +
  geom_ribbon(
    data = IT_predictions,
    aes(x = year, ymin = lower_PI, ymax = upper_PI),
    fill = "pink", alpha = 0.5, inherit.aes = FALSE
  ) +
  geom_line(aes(linetype = type), size = 1) +
  geom_point(size = 2) +
  scale_color_manual(values = c("Actual" = "blue", "Predicted" = "red")) +
  scale_linetype_manual(values = c("Actual" = "solid", "Predicted" = "dashed")) +
  geom_vline(xintercept = 2024.5, linetype = "dotted", color = "black") +
  scale_y_continuous(limits = c(30000, 36000), breaks = seq(30000, 36000, by = 2000)) +
  scale_x_continuous(breaks = seq(min(IT_plot_data$year), max(IT_plot_data$year), by = 1)) +
  labs(title = "Actual and Predicted Hourly Demand in Italy",
       x = "Year",
       y = "Hourly Demand",
       color = "Legend",
       linetype = "Legend") +
  theme_minimal() +
  theme(legend.position = "right")

# -----------------------------------------------------------------------------
