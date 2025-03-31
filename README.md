# Empirical-Evaluation-of-Machine-Learning-Procedures
# Empirical Evaluation of Machine Learning Procedures

This project evaluates and compares variable selection methods from both classical statistical learning and modern machine learning approaches using SAS. It includes simulation studies under various conditions and an application to a real-world dataset (Diabetes).

## 🎯 Objective

To compare and assess the performance of different variable selection methods:
- **Statistical Learning**: Forward, Backward, Stepwise
- **Machine Learning**: Lasso, LARS, Elastic Net

The analysis considers several model selection criteria and evaluates performance across different data generation settings.

## 🧪 Data

- **Simulated data**: under controlled scenarios such as:
  - Independent variables
  - Correlated predictors (internal and external)
  - Presence of outliers
  - Structural breaks
  - Combined cases

- **Real dataset**: The **Diabetes** dataset (Efron et al.), used to test empirical performance on real observations.

## ⚙️ Methodology

- Language: **SAS**
- Procedures: `PROC IML` (data generation), `PROC GLMSELECT` (model selection)
- Selection criteria: AIC, BIC, SBC, Cp, R² adjusted, and K-fold Cross-Validation
- Performance measured over 1,000 replications according to:
  - Perfect fitting
  - Overfitting
  - Underfitting
  - Incorrect models

## 📊 Key Results

- Statistical learning methods tend to **overfit**, especially in ideal settings
- Lasso and LARS give **similar results**, with Elastic Net being more robust in the presence of correlation
- **Cross-validation** and **SBC** are generally better stopping criteria
- Structural breaks significantly degrade performance across all methods
- In real data, **Lasso with cross-validation** produced the best selection

## 📁 Project Structure


