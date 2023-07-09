# predVerification.R
# -----------------------------------------------------------------------------
# Author:             Sambhav Chordia, Albert Kuo
# Date last modified: June 2023
#
# R script to predict values for the PEPRISC data set using the GBM model


# Load required packages for reading and writing from xlsx files

library(pacman)
p_load(
  shiny, shinythemes, shinyWidgets, shinycssloaders, tidyverse, ggrepel,
  janitor, caret, FNN, here, lime, readxl, writexl, caret, dplyr
)

fit <- readRDS("data/gbm_model.rds") # Model on full dataset
fit_sub <- readRDS("data/gbm_model_trt.rds") # Model on trt subsets
train <- readRDS("data/train_new.rds") # Training dataset (unnormalized)

input_data <- "Verification_FinalCohort(jun22jun23).xlsx"
output_data <- "predictionValues/pep_prediciton_value2.xlsx"

# Import data from the sheet provided
message("Reading data from input sheet")
data <- read_excel(input_data, .name_repair = "unique")

# Loop through all the rows and compute predictions
message("Computing predictions")
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
  test_patients_pred_ls <- list()
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
    adj_factor <- p2 / p1 * shrinkage + 1 * (1 - shrinkage)
    adj_factor[is.nan(adj_factor)] <- 1
    test_patients_pred_ls[[trt]] <- tibble(
      MRN = input$MRN,
      therapy = trt,
      pred = pred * adj_factor
    )
  }

  test_sub <- test_impute %>% filter(therapy == "No treatment")
  if (i == 1) {
    master_sheet <- tibble(
      MRN = input$MRN,
      indomethacin_only = test_patients_pred_ls[["Indomethacin only"]] %>% pull(pred),
      aggressive_hydration_only = test_patients_pred_ls[["Aggressive hydration only"]] %>% pull(pred),
      pd_stent_only = test_patients_pred_ls[["PD stent only"]] %>% pull(pred),
      aggressive_hydration_and_indomethacin = test_patients_pred_ls[["Aggressive hydration and indomethacin"]] %>% pull(pred),
      indomethacin_and_pd_stent = test_patients_pred_ls[["Indomethacin and PD stent"]] %>% pull(pred),
      no_treatment = predict(fit, newdata = test_sub, type = "prob")[, 2]
    )
  } else {
    master_sheet <- rbind(master_sheet, tibble(
      MRN = input$MRN,
      indomethacin_only = test_patients_pred_ls[["Indomethacin only"]] %>% pull(pred),
      aggressive_hydration_only = test_patients_pred_ls[["Aggressive hydration only"]] %>% pull(pred),
      pd_stent_only = test_patients_pred_ls[["PD stent only"]] %>% pull(pred),
      aggressive_hydration_and_indomethacin = test_patients_pred_ls[["Aggressive hydration and indomethacin"]] %>% pull(pred),
      indomethacin_and_pd_stent = test_patients_pred_ls[["Indomethacin and PD stent"]] %>% pull(pred),
      no_treatment = predict(fit, newdata = test_sub, type = "prob")[, 2]
    ))
  }
}
message("Writing to sheet")
write_xlsx(master_sheet, output_data, col_names = TRUE)
