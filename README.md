# Nomogram CVD

An interactive Shiny application to predict the probability of cardiovascular disease (CVD) based on dental clinical parameters. This app uses a logistic regression model and generates a nomogram to assist in identifying individuals at risk of CVD.

The nomogram is based on a logistic regression model trained on the following predictors:

Sex: Male/Female
Age: Continuous variable
Smoking Status: Never/Current/Former
Cholesterol: Yes/No
Diabetes Mellitus: Yes/No
Hypertension Treatment: Yes/No
Periodontitis Classification: None, Stage III, Stage IV, Edentulous

## Features

- **Interactive Inputs**: Adjust clinical parameters like age, smoking status, and cholesterol to calculate the probability of CVD.
- **Predictive Analysis**: Calculate and display the probability of CVD for user-specified inputs.
