# Electricity Demand Prediction in R

# ⚡️ Project Background

This project investigates how **macroeconomic indicators** can be used to **forecast national electricity demand** - a necessity both for governments and individual businesses.

Here, I've focused my research on **France and Italy**, using open data from the *World Bank (World Development Indicators)* and *ENTSO-E Transparency Platform*.
Over the course of several weeks, I've built and evaluated multiple linear regression models in R to predict average hourly electricity demand (MWh) for 2025–2028.

The project was originally developed as part of a university seminar paper in *Data Analysis for Business and Economics* and later extended into a data analytics case study for my portfolio.



# 🎯 Business Context

Electricity demand forecasting plays a critical role for:

- **Energy companies** – to plan production, prevent shortages, and optimize costs.
- **Governments & regulators** – to design sustainable energy policies.
- **Consulting & tech firms** – to advise clients on efficiency, infrastructure, and future investment decisions.

*The biggest challenge in efficient electricity supply is that electricity cannot be stored in significant quantities and at a reasonable cost, unlike other goods, so supply must match demand in real time.*



# 📊 Research Question

*How can economic indicators from the World Development Indicators (WDIs) be used to predict hourly electricity demand — and are they sufficient for accurate forecasting in France and Italy?*



# 🧠 Methodology Overview

### Languages & Tools:
- R (tidyverse, dplyr, ggplot2, caret, and other libraries)
- APIs: ENTSO-E, World Bank WDI

### Steps Taken:
1. **Data Collection:** Downloaded hourly electricity demand (2017–2024) from *ENTSO-E API* and macroeconomic indicators from *World Bank WDI*.

2. **Data Cleaning & Transformation**: Filled missing values using yearly growth interpolation, then logarithmized most variables (e.g., GDP, population) to reduce skew. Also, new economic features were engineered: household consumption expenditure, industrial value added, and other variables.

<img src="https://github.com/kateromaniuk/electricity-demand-prediction/blob/main/predictors.png?raw=true" height="600" alt="Predictors">

3. **Model Building:** Generated all possible combinations of predictors (1,023 regression models) and then selected the five best-suited models applying the following criteria: 
  - Adjusted R² ≥ 0.5
  - VIF < 5 (low multicollinearity)
  - Mean Absolute Percentage Error (MAPE) < 2 after training each model on 2017-2022 and testing on 2023-2024

4. **Model Evaluation:** The best five models were compared by MAPE, BIC, and economic interpretability — this way, one best model per country was selected.

5. **Forecasting (2025–2028):** Used official predictors for economic factors and estimated electricity demand for the next years, as well as calculating 95% confidence intervals.



# 🇫🇷 Case Study 1: France
**Best model variables:**
- ln(population)
- ln(industry value added)
- GDP growth rate

**Regression equation:**
*Electricity Demand = 4,120,457.2 − 214,808.34 × ln(population) − 7,309.04 × ln(industry) + 262.03 × GDP growth rate*

<img src="https://github.com/kateromaniuk/electricity-demand-prediction/blob/main/france_forecast.png?raw=true" height="400" alt="France Forecast">

**Model results:**
- Predicts a 7.3% decline in average hourly demand by 2028
- Interpreted as reflecting energy efficiency improvements and industrial restructuring

❗️ However, this downward trend contrasts with official projections from French power system operators. Enedis expects electricity consumption to rise, largely driven by increased electricity use from electric vehicles, data centers, and broader industrial electrification. Meanwhile, residential electricity consumption is forecasted to decline, aligned with efficiency improvements and sufficiency measures.

While my model created during the project relies on economic factors from the World Development Indicators provides a statistically robust and economically
interpretable forecast, it likely underestimates the impact of emergent drivers like the electrification of transport and data infrastructure growth.


# 🇮🇹 Case Study 2: Italy
**Best model variables:**
- ln(manufacturing output)
- ln(rural population)
- GDP deflator

**Regression equation:**
*Electricity Demand = −725,600.8 + 14,600.99 × ln(manufacturing) + 22,434.15 × ln(rural population) − 329.01 × GDP deflator*

<img src="https://github.com/kateromaniuk/electricity-demand-prediction/blob/main/italy_forecast.png?raw=true" height="400" alt="Italy Forecast">

**Model results:**
- Predicts a slight increase in electricity demand by 2028
- The upward trend aligns with official forecasts from TERNA (Italy’s national grid operator)


# ⚙️ Limitations & Next Steps

The results reveal an interesting contrast: while the trend obtained from the chosen model for Italy aligns closely with official forecasts showing a gradual increase in electricity demand, the model for France predicts a decline that contradicts public projections.

The reason for that is: while regression models based on economic indicators are interpretable and statistically sound, they don’t capture time-series effects like seasonality or autocorrelation.

**Next steps planned:**
- Detrend the data to uncover underlying patterns
- Integrate economic factors with time-series methods such as SARIMAX 
- Extending the dataset to include weather, energy prices, and renewable share variables


