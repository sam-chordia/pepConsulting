# predVerification.R
# -----------------------------------------------------------------------------
# Author:             Sambhav Chordia
# Date last modified: April 2023
#
# R script to predict values for the PEPRISC data set using the GBM model


# Load required packages for reading and writing from xlsx files
library(readxl)
library(writexl)

# Set file paths
excel_path <- "VerificationTemplate_Populated.xlsx" 
excel_pred_path <- "predictionValues/answers_pred.xlsx"
excel_pred_sub_path <- "predictionValues/answers_pred_sub.xlsx"

# copies over all the data models from the data folder

# Model on full dataset
fit <- readRDS("data/gbm_model.rds")
# Model on trt subsets
fit_sub <- readRDS("data/gbm_model_trt.rds") 
# Training dataset (unnormalized)
train <- readRDS("data/train_new.rds")
# Imputed and normalized training dataset
train_impute <- readRDS("data/train_impute.rds")
# Variable labels
var_names <- readRDS("data/var_names.rds")
# LIME explainer for variable importance
lime_explainer <- readRDS("data/lime_explainer.rds")


# Read in data from Excel sheet
print("Reading in data from Excel sheet...")
data <- read_excel(excel_path, .name_repair = "unique")


# Predict values
print("Using prediction models to predict values...")
pred_fit <- predict(fit, newdata = data, type = "prob")
pred_fit_sub <- predict(fit_sub, newdata = data, type = "prob")

# Append predictions to Excel sheet
print("Writing predictions to Excel sheet...")
write_xlsx(pred_fit, excel_pred_path, col_names = TRUE)
write_xlsx(pred_fit_sub, excel_pred_sub_path, col_names = TRUE)