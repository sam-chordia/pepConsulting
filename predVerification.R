# predVerification.R
# -----------------------------------------------------------------------------
# Author:             Sambhav Chordia
# Date last modified: April 2023
#
# R script to predict values for the PEPRISC data set using the GBM model


# Load required packages for reading and writing from xlsx files
library(readxl)
library(writexl)
library(caret)
library(dplyr)


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
# write_xlsx(pred_fit, excel_pred_path, col_names = TRUE)
# write_xlsx(pred_fit_sub, excel_pred_sub_path, col_names = TRUE)

# TODO - change the upper bound of loop to nrows(data)
for (i in 1:nrow(data)) {
  input <- data[i, ]
  input_dat <- data.frame(
    MRN = input$MRN,
    age_years = input$age_years,
    gender_male_1 = ifelse(input$gender_male_1 == "Male", 1, 0),
    bmi = input$bmi,
    sod = as.integer(input$sod),
    history_of_pep = as.integer(input$history_of_pep),
    hx_of_recurrent_pancreatitis = as.integer(input$history_of_pep),
    pancreatic_sphincterotomy = as.integer(input$history_of_pep),
    precut_sphincterotomy = as.integer(input$precut_sphincterotomy),
    minor_papilla_sphincterotomy = as.integer(input$minor_papilla_sphincterotomy),
    failed_cannulation = as.integer(input$failed_cannulation),
    difficult_cannulation = as.integer(input$difficult_cannulation),
    pneumatic_dilation_of_intact_biliary_sphincter = as.integer(input$pneumatic_dilation_of_intact_biliary_sphincter),
    pancreatic_duct_injection = as.integer(input$pancreatic_duct_injection),
    pancreatic_duct_injections_2 = as.integer(input$pancreatic_duct_injections_2),
    acinarization = as.integer(input$acinarization),
    trainee_involvement = as.integer(input$trainee_involvement),
    cholecystectomy = as.integer(input$cholecystectomy),
    pancreo_biliary_malignancy = as.integer(input$pancreo_biliary_malignancy),
    guidewire_cannulation = as.integer(input$guidewire_cannulation),
    guidewire_passage_into_pancreatic_duct = as.integer(input$guidewire_passage_into_pancreatic_duct),
    guidewire_passage_into_pancreatic_duct_2 = as.integer(input$guidewire_passage_into_pancreatic_duct_2),
    biliary_sphincterotomy = as.integer(input$biliary_sphincterotomy),
    aggressive_hydration = c(0, 1, 0, 0, 1, 0),
    indomethacin_nsaid_prophylaxis = c(0, 0, 1, 0, 1, 1),
    pancreatic_duct_stent_placement = c(0, 0, 0, 1, 0, 1),
    therapy = c(
      "No treatment", "Aggressive hydration only", "Indomethacin only",
      "PD stent only", "Aggressive hydration and indomethacin", "Indomethacin and PD stent"
    ),
    patient_id = 1
  )
  pre_proc_values <- preProcess(train %>% select(-c("study_id", "pep", "patient_id")), method = c("center", "scale"))
  test_impute <- predict(pre_proc_values, input_dat)
    for (trt in c("Aggressive hydration only", "Indomethacin only", "PD stent only", "Aggressive hydration and indomethacin", "Indomethacin and PD stent")) {
      # Predict on no trt
      p1 <- predict(fit_sub[[trt]], newdata = test_impute %>% filter(therapy == "No treatment"), type = "prob")[, 2]

      # Predict on trt
      p2 <- predict(fit_sub[[trt]], newdata = test_impute %>% filter(therapy == trt), type = "prob")[, 2]

      # Predict on full model
      test_sub <- test_impute %>% filter(therapy == "No treatment")
      pred <- predict(fit, newdata = test_sub, type = "prob")[, 2]

      # Adjust prediction for aggressive hydration first
      if (trt == "Aggressive hydration and indomethacin") {
        p3 <- predict(fit_sub[["Aggressive hydration only"]], newdata = test_impute %>% filter(therapy == "No treatment"), type = "prob")[, 2]
        p4 <- predict(fit_sub[["Aggressive hydration only"]], newdata = test_impute %>% filter(therapy == "Aggressive hydration only"), type = "prob")[, 2]
        shrinkage <- ifelse(p3 > 0.1, 1, p3 * 10)
        adj_factor <- p4 / p3 * shrinkage + 1 * (1 - shrinkage)
        adj_factor[is.nan(adj_factor)] <- 1
        pred <- pred * adj_factor
      }

      # Compute adjusted prediction
      shrinkage <- ifelse(p1 > 0.1, 1, p1 * 10)
      adj_factor <- p2 / p1 * shrinkage + 1 * (1 - shrinkage) # take the ratio, with shrinkage towards 1 if p1 < 0.1 # nolint
      adj_factor[is.nan(adj_factor)] <- 1 # set adjustment factor to 1 if p1 = 0
      test_patients_pred_ls[[trt]] <- tibble(
        MRN = input$MRN,
        therapy = trt,
        pred = pred * adj_factor
      )
    }
    # Combine predictions
    if (i == 1) {
      test_indomethacin_only <- test_patients_pred_ls[["Indomethacin only"]]
      test_aggressive_hydration_only <- test_patients_pred_ls[["Aggressive hydration only"]]
      test_pd_stent_only <- test_patients_pred_ls[["PD stent only"]]
      test_aggressive_hydration_and_indomethacin <- test_patients_pred_ls[["Aggressive hydration and indomethacin"]]
      test_indomethacin_and_pd_stent <- test_patients_pred_ls[["Indomethacin and PD stent"]]
    } else {
      test_indomethacin_only <- rbind(test_indomethacin_only, test_patients_pred_ls[["Indomethacin only"]])
      test_aggressive_hydration_only <- rbind(test_aggressive_hydration_only, test_patients_pred_ls[["Aggressive hydration only"]])
      test_pd_stent_only <- rbind(test_pd_stent_only, test_patients_pred_ls[["PD stent only"]])
      test_aggressive_hydration_and_indomethacin <- rbind(test_aggressive_hydration_and_indomethacin, test_patients_pred_ls[["Aggressive hydration and indomethacin"]])
      test_indomethacin_and_pd_stent <- rbind(test_indomethacin_and_pd_stent, test_patients_pred_ls[["Indomethacin and PD stent"]])
    }
  test_sub <- test_impute %>% filter(therapy == "No treatment")

  # Computes values for "no treatemnt"
  if (i == 1) {
    test_no_trt <- tibble(
      MRN = input$MRN,
      therapy = "No treatment",
      pred = predict(fit, newdata = test_sub, type = "prob")[, 2]
    )

    master_sheet <- tibble(
      MRN = input$MRN,
      indomethacin_only = test_patients_pred_ls[["Indomethacin only"]] %>% pull(pred),
      aggressive_hydration_only = test_patients_pred_ls[["Aggressive hydration only"]] %>% pull(pred),
      pd_stent_only = test_patients_pred_ls[["PD stent only"]] %>% pull(pred),
      aggressive_hydration_and_indomethacin = test_patients_pred_ls[["Aggressive hydration and indomethacin"]] %>% pull(pred),
      indomethacin_and_pd_stent = test_patients_pred_ls[["Indomethacin and PD stent"]] %>% pull(pred),
      no_treatment = pred
    )
    
  } else {
    test_no_trt <- rbind(test_no_trt, tibble(
      MRN = input$MRN,
      therapy = "No treatment",
      pred = predict(fit, newdata = test_sub, type = "prob")[, 2]
    ))
     master_sheet <- rbind(master_sheet, tibble(
        MRN = input$MRN,
        indomethacin_only = test_patients_pred_ls[["Indomethacin only"]] %>% pull(pred),
        aggressive_hydration_only = test_patients_pred_ls[["Aggressive hydration only"]] %>% pull(pred),
        pd_stent_only = test_patients_pred_ls[["PD stent only"]] %>% pull(pred),
        aggressive_hydration_and_indomethacin = test_patients_pred_ls[["Aggressive hydration and indomethacin"]] %>% pull(pred),
        indomethacin_and_pd_stent = test_patients_pred_ls[["Indomethacin and PD stent"]] %>% pull(pred),      
        no_treatment = pred
      ))
  }

}

# Writes to excel as seperate sheets with each therapy as a sheet
write_xlsx(test_no_trt, "predictionValues/answers_pred_no_treatment_path.xlsx", col_names = TRUE)
write_xlsx(test_aggressive_hydration_and_indomethacin, "predictionValues/aggressive_hydration_and_indomethacin_pred.xlsx", col_names = TRUE)
write_xlsx(test_aggressive_hydration_only, "predictionValues/aggressive_hydration_only_pred.xlsx", col_names = TRUE)
write_xlsx(test_indomethacin_and_pd_stent, "predictionValues/indomethacin_pred.xlsx", col_names = TRUE)
write_xlsx(test_pd_stent_only, "predictionValues/pd_stent_only_pred.xlsx", col_names = TRUE)
write_xlsx(test_indomethacin_only, "predictionValues/indomethacin_only_pred.xlsx", col_names = TRUE)
write_xlsx(master_sheet, "predictionValues/master_sheet.xlsx", col_names = TRUE)

