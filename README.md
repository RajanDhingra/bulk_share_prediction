# Bike Count Prediction

This work focusses on building single and ensemble models that predict demand for a bike sharing scheme, over a period of 2 years (2011 and 2012) for Capital bikeshare system, Washington D.C.

The objective is to ensure that enough bicycles are stocked to meet daily demand while minimising costs.

Data cleaning step involved detection and imputation of outlier’s value based on Inter-Quartile range.

7 lag variables, 3 moving average variables and one weekly trend variable were derived from the original data in the feature engineering step. 

1 target variable, which represents the trend of demand for tomorrow was also derived.

Furthermore, an ARIMA variable to factor in the effects of seasonality and trend was created which when used as an input variable boosted the model performance.

Using this data 4 individual models and 2 ensemble models were built, as shown below. The best performing model was the Ensemble XGBoost regression tree. While other models focussed on reducing the Root mean square error, the final ensemble model was trained using an objective function of maximising profit.

EDA, Preprocessing & Feature Engineering.R takes in day.csv as the input file

ModellingPart.R takes in Data_Final.csv as the input file
