# app.R
# -----------------------------------------------------------------------------
# Author:             Albert Kuo & Sambhav Chordia 
# Date last modified: Jul 2023

library(pacman)
library(rsconnect)
library(gbm)
library(randomForest)
library(shiny)
library(shinyWidgets)
library(shinyjs)
# downloads the required requirements for the program
p_load(
  shiny, shinythemes, shinyWidgets, shinycssloaders, tidyverse, ggrepel,
  janitor, caret, FNN, here, lime
)
#source(here("pep_risk_app/data/my_plot_lime_features.R"))

# Define UI for application
ui <- navbarPage("",
  id = "navbar",

  # Choose a theme (optional)
  # theme = shinytheme("simplex"),
  # Application title
  
  tags$head(
    tags$style(
      HTML("
        strong {
          font-size: 1.2em; /* Change the font size as needed */
        }
      ")
    )
  ),
  
  tabPanel(
    title = "Estimator", value = "Estimator",

    # ----

    fluidRow(
      column(4,
        offset = 0,
        wellPanel(
          div(
            style = "display:inline-block",
            awesomeRadio(
              inputId = "gender_male_1",
              label = "Sex",
              choices = c("Male", "Female"),
              inline = TRUE
            )
          ),
          br(),
            div(
            style = "display:inblock",
            numericInput(
              inputId = "age_years",
              label = "Age",
              value = 0
            )
          ),
          br(),
          div(
            style = "display:block",
            numericInput(
              inputId = "bmi",
              label = "BMI",
              value = 0
            )
          ),
          br(),
          div(
            column(7,
                   strong("Acinarization"),
            ),
            div(
              style = "display:inline-block",
              switchInput(
                inputId = "acinarization",
                onLabel = "Yes",
                offLabel = "No"
              )
            ),
          ),
          br(),
          div(
            column(7,
                   strong("Biliary sphincterotomy"),
            ),
            div(
              style = "display:inline-block",
              switchInput(
                inputId = "biliary_sphincterotomy",
                onLabel = "Yes",
                offLabel = "No"
              )
            ),
          ),
          br(),
          div(
            column(7,
                   strong("Cholecystectomy"),
            ),
            div(
              style = "display:inline-block",
              switchInput(
                inputId = "cholecystectomy",
                onLabel = "Yes",
                offLabel = "No"
              )
            ),
          ),
          
          br(),
          
          div(
            column(7,
                   strong("Difficult cannulation"),
            ),
            div(
              style = "display:inline-block",
              switchInput(
                inputId = "difficult_cannulation",
                onLabel = "Yes",
                offLabel = "No"
              )
            ),
          ),
          br(),
          div(
            column(7,
                   strong("Failed cannulation"),
            ),
            div(
              style = "display:inline-block",
              switchInput(
                inputId = "failed_cannulation",
                onLabel = "Yes",
                offLabel = "No"
              )
            ),
          ),
          br(),
          div(
            column(7,
                   strong("Guidewire cannulation"),
            ),
            div(
              style = "display:inline-block",
              switchInput(
                inputId = "guidewire_cannulation",
                onLabel = "Yes",
                offLabel = "No"
              )
            ),
          ),
          br(),
          div(
            column(7,
                   strong("Guidewire passage into pancreatic duct"),
            ),
            div(
              style = "display:inline-block",
              switchInput(
                inputId = "guidewire_passage_into_pancreatic_duct",
                onLabel = "Yes",
                offLabel = "No"
              )
            ),
          ),
          br(),
          div(
            column(7,
                   strong("Guidewire passage into pancreatic duct > 2"),
            ),
            div(
              style = "display:inline-block",
              switchInput(
                inputId = "guidewire_passage_into_pancreatic_duct_2",
                onLabel = "Yes",
                offLabel = "No"
              )
            ),
          ),
          br(),
          div(
            column(7,
                strong("History of PEP"),
            ),
            div(
            style = "display:inline-block",
            switchInput(
              inputId = "history_of_pep",
              onLabel = "Yes",
              offLabel = "No"
            )
            ),
          ),
          br(),
          div(
            column(7,
                   strong("History of recurrent pancreatitis"),
            ),
            div(
            style = "display:inline-block",
            switchInput(
              inputId = "hx_of_recurrent_pancreatitis",
              onLabel = "Yes",
              offLabel = "No"
            )
            ),
          ),
          br(),
          div(
            column(7,
                   strong("Minor papilla sphincterotomy"),
            ),
            div(
              style = "display:inline-block",
              switchInput(
                inputId = "minor_papilla_sphincterotomy",
                onLabel = "Yes",
                offLabel = "No"
              )
            ),
          ),
          br(),
          div(
            column(7,
                   strong("Pancreatic duct injection"),
            ),
            div(
              style = "display:inline-block",
              switchInput(
                inputId = "pancreatic_duct_injection",
                onLabel = "Yes",
                offLabel = "No"
              )
            ),
          ),
          br(),
          div(
            column(7,
                   strong("Pancreatic duct injections > 2"),
            ),
            div(
              style = "display:inline-block",
              switchInput(
                inputId = "pancreatic_duct_injections_2",
                onLabel = "Yes",
                offLabel = "No"
              )
            ),
          ),
          br(),
          div(
            column(7,
                   strong("Pancreatic sphincterotomy"),
            ),
            div(
            style = "display:inline-block",
            switchInput(
              inputId = "pancreatic_sphincterotomy",
              onLabel = "Yes",
              offLabel = "No"
            )
            ),
          ),
          br(),
          div(
            column(7,
                   strong("Pancreo biliary malignancy"),
            ),
            div(
              style = "display:inline-block",
              switchInput(
                inputId = "pancreo_biliary_malignancy",
                onLabel = "Yes",
                offLabel = "No"
              )
            ),
          ),
          br(),
          div(
            column(7,
                   strong("Pneumatic dilation of intact biliary sphincter"),
            ),
            div(
              style = "display:inline-block",
              switchInput(
                inputId = "pneumatic_dilation_of_intact_biliary_sphincter",
                onLabel = "Yes",
                offLabel = "No"
              )
            ),
          ),
          br(),
          div(
            column(7,
                   strong("Precut sphincterotomy"),
          ),
          div(
            style = "display:inline-block",
            switchInput(
              inputId = "precut_sphincterotomy",
              onLabel = "Yes",
              offLabel = "No"
            )
          ),
        ),
          br(),
        div(
          column(7,
                 strong("Sphincter of oddi dysfunction"),
          ),
          div(
            style = "display:inline-block",
            switchInput(
              inputId = "sod",
              onLabel = "Yes",
              offLabel = "No"
            )
          ),
        ),
        br(),
        div(
          column(7,
                 strong("Trainee involvement"),
          ),
          div(
            style = "display:inline-block",
            switchInput(
              inputId = "trainee_involvement",
              onLabel = "Yes",
              offLabel = "No"
            )
          ),
        ),
          br(), br(),
          actionButton("update", "Submit",
                       class = "center",
                       icon = icon("caret-right"),
                       style = "color: #fff; background-color: #337ab7"
          )
        )
      ),
      
      column(
        7,
        h1("PEPRisC: Post-ERCP Pancreatitis Risk Calculator"),
        br(),
        p(span("Calculates a patient’s risk of developing pancreatitis after endoscopic retrograde cholangiopancreatography (ERCP)", style = "color: ##6c7b89")),
        br(),
        # One-line prediction summary
        tags$head(tags$style("#pep_pred{font-size: 12pt; display: inline}")),
        uiOutput("pep_pred"),
        uiOutput("numberOutput"),
        br()
      ),
      # column(
      #   3,
      #   # Votes plot
      #   plotOutput("votesPlot") %>%
      #     withSpinner(type = 5)
      # ),
      column(
        7,
        # Votes plot
        plotOutput("votesTrtPlot") %>%
          withSpinner(type = 5),
        br(),
        br(),
        br(),
      )
    )


    #### ----
  ),
  tabPanel(
    title = "About", value = "About",
    fluidRow(
      column(
        10,
        h4("The App"),
        p("The Post ERCP Pancreatitis Risk Calculator and Decision Making Tool was designed to estimate a patient's risk of developing
                                   post ERCP pancreatitis and the effects of different treatments in reducing the risk. Its aim is to complement, rather than replace,
                                   the decision-making of physicians. The estimator was developed using data from 7,389 patients from 12 studies. The
                                   method achieved an AUC of 0.70 under 5-fold cross-validation."),
        h4("When to Use"),

        p("Primarily used to stratify the risk of developing acute pancreatitis in adult patients undergoing an ERCP procedure. "),
        p("Predicts prophylactic effects of 5 prophylactic interventions including: (a) aggressive IV hydration, (b) rectal NSAID, (c) pancreatic duct (PD) stent, (d) IV hydration AND NSAID, (e) PD stent AND NSAID. "),
        h4("Where to Use"),
        p("The PEPRisC tool uses a machine learning model to predict patient’s risk of post-ERCP pancreatitis using patient-specific demographic and procedural factors. "),
        
      )
    )
  )
)

# Define server logic required for plots
server <- function(input, output, session) {
  
  output$numberOutput <- renderUI({
    percent_value <- round(pep_percent() * 100, digits = 1)
    tags$div(
      style = "display: flex; justify-content: center;",
      tags$div(
        style = "display: inline-block; background-color: green; color: white; padding: 10px; font-size: 40px; text-align: center;",
        tags$p(
          style = "margin: 0; line-height: 1;",
          paste0("    ", percent_value, "%  ")
        ),
        tags$p(
          style = "font-size: 20px; margin: 0; line-height: 1; display: inline-block;",
          "Predicted risk of PEP without prophylaxis"
        )
    )
    )
  })
  
  
  
  
  
  # Show/hide the text box for "When to use"
  observeEvent(input$btn_when, {
    shinyjs::toggle("text_when")
  })
  
  # Show/hide the text box for "Why to use"
  observeEvent(input$btn_why, {
    shinyjs::toggle("text_why")
  })
  
  # Read in data output from pred_model.Rmd
  fit <- readRDS("data/gbm_model.rds") # Model on full dataset
  fit_sub <- readRDS("data/gbm_model_trt.rds") # Model on trt subsets
  train <- readRDS("data/train_new.rds") # Training dataset (unnormalized)
  train_impute <- readRDS("data/train_impute.rds") # Imputed and normalized training dataset
  var_names <- readRDS("data/var_names.rds") # Variable labels
  lime_explainer <- readRDS("data/lime_explainer.rds") # LIME explainer for variable importance
  # pred_dt = readRDS("data/pred_dt_gbm.rds") # Prediction scores for training set
  n_k <- 1 # Number of nearest neighbors

  # Wait for submit button before refreshing plots
  therapy_level_order <- c(
    "No treatment", "Aggressive hydration only", "Indomethacin only",
    "Aggressive hydration and indomethacin", "PD stent only", "Indomethacin and PD stent"
  )
  ref_samples_default <- tibble(
    patient_id = 1:120,
    pep = as.factor(0),
    pred = 0,
    therapy = rep(therapy_level_order, 20)
  )
  test_patient_default <- tibble(
    patient_id = 1,
    pred = 0,
    therapy = therapy_level_order
  )
  explanation_default <- readRDS("data/explanation_default.rds")
  rv <- reactiveValues(
    ref_samples = ref_samples_default,
    test_patient_pred = test_patient_default,
    explanation = explanation_default
  )
  pep_percent <- reactiveVal(0)
  
  observeEvent(input$update, {
    # Capture input and put in data frame
    input_dat <- data.frame(
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


    # Normalize valuesc
    message("Normalize values")
    pre_proc_values <- preProcess(train %>% select(-c("study_id", "pep", "patient_id")), method = c("center", "scale"))
    test_impute <- predict(pre_proc_values, input_dat)
    message('PRE PROC VALUES')

    message("TEST IMPUTE")

    message("input_dat")


    # Prediction
    # Prediction for each treatment with models on trt subsets
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
      adj_factor <- p2 / p1 * shrinkage + 1 * (1 - shrinkage) # take the ratio, with shrinkage towards 1 if p1 < 0.1
      adj_factor[is.nan(adj_factor)] <- 1 # set adjustment factor to 1 if p1 = 0
      pred_adj <- pred * adj_factor
      test_patients_pred_ls[[trt]] <- tibble(
        patient_id = test_sub$patient_id,
        therapy = trt,
        pred = pred_adj
      )
      
    }
        # Prediction for no treatment on full model
    test_sub <- test_impute %>% filter(therapy == "No treatment")
    test_no_trt <- tibble(
      patient_id = test_sub$patient_id,
      therapy = "No treatment",
      pred = predict(fit, newdata = test_sub, type = "prob")[, 2]
    )
    message("No treatment value is: ", predict(fit, newdata = test_sub, type = "prob")[, 2])
    pep_percent(predict(fit, newdata = test_sub, type = "prob")[, 2])
    
    test_patient_pred <- bind_rows(bind_rows(test_patients_pred_ls), test_no_trt)
    rv$test_patient_pred <- test_patient_pred

    # Nearest neighbors (ref_samples)
    message("Nearest neighbors")
    patient_ids <- train %>%
      filter(aggressive_hydration == 0 &
        indomethacin_nsaid_prophylaxis == 0 &
        pancreatic_duct_stent_placement == 0) %>%
      pull(patient_id)
    train_sub <- train_impute %>%
      filter(patient_id %in% patient_ids)
    notrt_values <- train_sub %>%
      select(aggressive_hydration, indomethacin_nsaid_prophylaxis, pancreatic_duct_stent_placement) %>%
      distinct()
    stopifnot(nrow(notrt_values) == 1)

    test_patient_impute <- test_impute
    k <- knn(
      train = train_sub %>% select(-pep, -patient_id, -indomethacin_nsaid_prophylaxis, -aggressive_hydration, -pancreatic_duct_stent_placement),
      test = test_patient_impute %>% filter(therapy == "No treatment") %>%
        select(-patient_id, -therapy, -indomethacin_nsaid_prophylaxis, -aggressive_hydration, -pancreatic_duct_stent_placement),
      train_sub$pep, k = n_k, algorithm = "cover_tree"
    )
    indices <- attr(k, "nn.index")[1, ]
    neighbors_1 <- train_sub %>%
      slice(indices)
    ## Get prediction
    pred <- predict(fit, newdata = neighbors_1, type = "prob")[, 2]
    neighbors_1 <- neighbors_1 %>% mutate(pred = pred)

    # Nearest neighbors among aggressive hydration only patients (n = 325)
    trt <- "Aggressive hydration only"
    patient_ids <- train %>%
      filter(aggressive_hydration == 1 &
        indomethacin_nsaid_prophylaxis == 0 &
        pancreatic_duct_stent_placement == 0) %>%
      pull(patient_id)
    train_sub <- train_impute %>%
      filter(patient_id %in% patient_ids)

    k <- knn(
      train = train_sub %>% select(-pep, -patient_id, -indomethacin_nsaid_prophylaxis, -aggressive_hydration, -pancreatic_duct_stent_placement),
      test = test_patient_impute %>% filter(therapy == trt) %>%
        select(-patient_id, -therapy, -indomethacin_nsaid_prophylaxis, -aggressive_hydration, -pancreatic_duct_stent_placement),
      train_sub$pep, k = n_k, algorithm = "cover_tree"
    )
    indices <- attr(k, "nn.index")[1, ]
    neighbors_2 <- train_sub %>%
      slice(indices)
    neighbors_2_notrt <- neighbors_2 %>% mutate(indomethacin_nsaid_prophylaxis = notrt_values$indomethacin_nsaid_prophylaxis, aggressive_hydration = notrt_values$aggressive_hydration, pancreatic_duct_stent_placement = notrt_values$pancreatic_duct_stent_placement)
    ## Get prediction
    pred <- predict(fit,
      newdata = neighbors_2_notrt,
      type = "prob"
    )[, 2]
    p1 <- predict(fit_sub[[trt]], newdata = neighbors_2_notrt, type = "prob")[, 2]
    p2 <- predict(fit_sub[[trt]], newdata = neighbors_2, type = "prob")[, 2]
    shrinkage <- ifelse(p1 > 0.1, 1, p1 * 10)
    adj_factor <- p2 / p1 * shrinkage + 1 * (1 - shrinkage) # take the ratio, with shrinkage towards 1 if p1 < 0.1
    adj_factor[is.nan(adj_factor)] <- 1 # set adjustment factor to 1 if p1 = 0
    pred_adj <- pred * adj_factor
    neighbors_2 <- neighbors_2 %>% mutate(pred = pred_adj)

    # Nearest neighbors among indomethacin only patients (n = 2955)
    trt <- "Indomethacin only"
    patient_ids <- train %>%
      filter(aggressive_hydration == 0 &
        indomethacin_nsaid_prophylaxis == 1 &
        pancreatic_duct_stent_placement == 0) %>%
      pull(patient_id)
    train_sub <- train_impute %>%
      filter(patient_id %in% patient_ids)

    k <- knn(
      train = train_sub %>% select(-pep, -patient_id, -indomethacin_nsaid_prophylaxis, -aggressive_hydration, -pancreatic_duct_stent_placement),
      test = test_patient_impute %>% filter(therapy == trt) %>%
        select(-patient_id, -therapy, -indomethacin_nsaid_prophylaxis, -aggressive_hydration, -pancreatic_duct_stent_placement),
      train_sub$pep, k = n_k, algorithm = "cover_tree"
    )
    indices <- attr(k, "nn.index")[1, ]
    neighbors_3 <- train_sub %>%
      slice(indices)
    neighbors_3_notrt <- neighbors_3 %>% mutate(indomethacin_nsaid_prophylaxis = notrt_values$indomethacin_nsaid_prophylaxis, aggressive_hydration = notrt_values$aggressive_hydration, pancreatic_duct_stent_placement = notrt_values$pancreatic_duct_stent_placement)
    ## Get prediction
    pred <- predict(fit,
      newdata = neighbors_3_notrt,
      type = "prob"
    )[, 2]
    p1 <- predict(fit_sub[[trt]], newdata = neighbors_3_notrt, type = "prob")[, 2]
    p2 <- predict(fit_sub[[trt]], newdata = neighbors_3, type = "prob")[, 2]
    shrinkage <- ifelse(p1 > 0.1, 1, p1 * 10)
    adj_factor <- p2 / p1 * shrinkage + 1 * (1 - shrinkage) # take the ratio, with shrinkage towards 1 if p1 < 0.1
    adj_factor[is.nan(adj_factor)] <- 1 # set adjustment factor to 1 if p1 = 0
    pred_adj <- pred * adj_factor
    neighbors_3 <- neighbors_3 %>% mutate(pred = pred_adj)

    # Nearest neighbors among PD stent patients (n = 363)
    trt <- "PD stent only"
    patient_ids <- train %>%
      filter(aggressive_hydration == 0 &
        indomethacin_nsaid_prophylaxis == 0 &
        pancreatic_duct_stent_placement == 1) %>%
      pull(patient_id)
    train_sub <- train_impute %>%
      filter(patient_id %in% patient_ids)

    k <- knn(
      train = train_sub %>% select(-pep, -patient_id, -indomethacin_nsaid_prophylaxis, -aggressive_hydration, -pancreatic_duct_stent_placement),
      test = test_patient_impute %>% filter(therapy == trt) %>%
        select(-patient_id, -therapy, -indomethacin_nsaid_prophylaxis, -aggressive_hydration, -pancreatic_duct_stent_placement),
      train_sub$pep, k = n_k, algorithm = "cover_tree"
    )
    indices <- attr(k, "nn.index")[1, ]
    neighbors_4 <- train_sub %>%
      slice(indices)
    neighbors_4_notrt <- neighbors_4 %>% mutate(indomethacin_nsaid_prophylaxis = notrt_values$indomethacin_nsaid_prophylaxis, aggressive_hydration = notrt_values$aggressive_hydration, pancreatic_duct_stent_placement = notrt_values$pancreatic_duct_stent_placement)
    ## Get prediction
    pred <- predict(fit,
      newdata = neighbors_4_notrt,
      type = "prob"
    )[, 2]
    p1 <- predict(fit_sub[[trt]], newdata = neighbors_4_notrt, type = "prob")[, 2]
    p2 <- predict(fit_sub[[trt]], newdata = neighbors_4, type = "prob")[, 2]
    shrinkage <- ifelse(p1 > 0.1, 1, p1 * 10)
    adj_factor <- p2 / p1 * shrinkage + 1 * (1 - shrinkage) # take the ratio, with shrinkage towards 1 if p1 < 0.1
    adj_factor[is.nan(adj_factor)] <- 1 # set adjustment factor to 1 if p1 = 0
    pred_adj <- pred * adj_factor
    neighbors_4 <- neighbors_4 %>% mutate(pred = pred_adj)

    # Nearest neighbors among aggressive hydration and indomethacin (n = 79)
    trt <- "Aggressive hydration and indomethacin"
    patient_ids <- train %>%
      filter(aggressive_hydration == 1 &
        indomethacin_nsaid_prophylaxis == 1 &
        pancreatic_duct_stent_placement == 0) %>%
      pull(patient_id)
    train_sub <- train_impute %>%
      filter(patient_id %in% patient_ids)

    k <- knn(
      train = train_sub %>% select(-pep, -patient_id, -indomethacin_nsaid_prophylaxis, -aggressive_hydration, -pancreatic_duct_stent_placement),
      test = test_patient_impute %>%
        filter(therapy == trt) %>%
        select(-patient_id, -therapy, -indomethacin_nsaid_prophylaxis, -aggressive_hydration, -pancreatic_duct_stent_placement),
      train_sub$pep, k = n_k, algorithm = "cover_tree"
    )
    indices <- attr(k, "nn.index")[1, ]
    neighbors_5 <- train_sub %>%
      slice(indices)
    neighbors_5_notrt <- neighbors_5 %>% mutate(indomethacin_nsaid_prophylaxis = notrt_values$indomethacin_nsaid_prophylaxis, aggressive_hydration = notrt_values$aggressive_hydration, pancreatic_duct_stent_placement = notrt_values$pancreatic_duct_stent_placement)

    ## Get prediction
    pred <- predict(fit,
      newdata = neighbors_5_notrt,
      type = "prob"
    )[, 2]
    ah_patient <- train %>%
      filter(aggressive_hydration == 1) %>%
      slice(1) %>%
      pull(patient_id) # pick a patient that had aggressive_hydration
    ah_value <- train_impute %>%
      filter(patient_id %in% ah_patient) %>%
      pull(aggressive_hydration)
    p3 <- predict(fit_sub[["Aggressive hydration only"]], newdata = neighbors_5_notrt, type = "prob")[, 2]
    p4 <- predict(fit_sub[["Aggressive hydration only"]], newdata = neighbors_5_notrt %>% mutate(aggressive_hydration = ah_value), type = "prob")[, 2]
    shrinkage <- ifelse(p3 > 0.1, 1, p3 * 10)
    adj_factor <- p4 / p3 * shrinkage + 1 * (1 - shrinkage)
    adj_factor[is.nan(adj_factor)] <- 1
    pred <- pred * adj_factor
    p1 <- predict(fit_sub[[trt]], newdata = neighbors_5_notrt, type = "prob")[, 2]
    p2 <- predict(fit_sub[[trt]], newdata = neighbors_5, type = "prob")[, 2]
    shrinkage <- ifelse(p1 > 0.1, 1, p1 * 10)
    adj_factor <- p2 / p1 * shrinkage + 1 * (1 - shrinkage) # take the ratio, with shrinkage towards 1 if p1 < 0.1
    adj_factor[is.nan(adj_factor)] <- 1 # set adjustment factor to 1 if p1 = 0
    pred_adj <- pred * adj_factor
    neighbors_5 <- neighbors_5 %>% mutate(pred = pred_adj)

    # Nearest neighbors among Indomethacin and PD stent patients
    trt <- "PD stent only"
    patient_ids <- train %>%
      filter(aggressive_hydration == 0 &
        indomethacin_nsaid_prophylaxis == 1 &
        pancreatic_duct_stent_placement == 1) %>%
      pull(patient_id)
    train_sub <- train_impute %>%
      filter(patient_id %in% patient_ids)

    k <- knn(
      train = train_sub %>% select(-pep, -patient_id, -indomethacin_nsaid_prophylaxis, -aggressive_hydration, -pancreatic_duct_stent_placement),
      test = test_patient_impute %>% filter(therapy == trt) %>%
        select(-patient_id, -therapy, -indomethacin_nsaid_prophylaxis, -aggressive_hydration, -pancreatic_duct_stent_placement),
      train_sub$pep, k = n_k, algorithm = "cover_tree"
    )
    indices <- attr(k, "nn.index")[1, ]
    neighbors_6 <- train_sub %>%
      slice(indices)
    neighbors_6_notrt <- neighbors_6 %>% mutate(indomethacin_nsaid_prophylaxis = notrt_values$indomethacin_nsaid_prophylaxis, aggressive_hydration = notrt_values$aggressive_hydration, pancreatic_duct_stent_placement = notrt_values$pancreatic_duct_stent_placement)
    ## Get prediction
    pred <- predict(fit,
      newdata = neighbors_6_notrt,
      type = "prob"
    )[, 2]
    p1 <- predict(fit_sub[[trt]], newdata = neighbors_6_notrt, type = "prob")[, 2]
    p2 <- predict(fit_sub[[trt]], newdata = neighbors_6, type = "prob")[, 2]
    shrinkage <- ifelse(p1 > 0.1, 1, p1 * 10)
    adj_factor <- p2 / p1 * shrinkage + 1 * (1 - shrinkage) # take the ratio, with shrinkage towards 1 if p1 < 0.1
    adj_factor[is.nan(adj_factor)] <- 1 # set adjustment factor to 1 if p1 = 0
    pred_adj <- pred * adj_factor
    neighbors_6 <- neighbors_6 %>% mutate(pred = pred_adj)

    # Store neighbors with test patient
    message("Store neighbors")
    rv$ref_samples <- bind_rows(
      neighbors_1, neighbors_2, neighbors_3,
      neighbors_4, neighbors_5, neighbors_6
    ) %>%
      select(patient_id, pred, pep) %>%
      mutate(therapy = rep(
        c(
          "No treatment", "Aggressive hydration only",
          "Indomethacin only", "PD stent only", "Aggressive hydration and indomethacin",
          "Indomethacin and PD stent"
        ),
        each = n_k
      ))

    # Store LIME explanation
    explanation <- explain(test_sub %>% select(-patient_id, -therapy),
      lime_explainer,
      n_labels = 1,
      n_features = ncol(test_sub) - 2
    )
    explanation <- explanation %>%
      right_join(., var_names, by = c("feature" = "variable")) %>%
      mutate(feature_desc = var_label)
    rv$explanation <- explanation
  })

  #Plot of votes for no treatment
  # output$votesPlot <- renderPlot({
  #   set.seed(1) # for geom_jitter to look the same every time
  #   ref_samples <- rv$ref_samples %>% filter(therapy == "No treatment")
  #   test_patient_pred <- rv$test_patient_pred %>% filter(therapy == "No treatment")
  # 
  #   input$update
  #   isolate(
  #     ggplot() +
  #       geom_jitter(
  #         data = ref_samples, width = 0.1, height = 0,
  #         aes(
  #           x = factor(therapy, levels = therapy_level_order),
  #           y = pred, size = 3, color = pep, alpha = 0.8
  #         )
  #       ) +
  #       geom_point(
  #         data = test_patient_pred,
  #         aes(
  #           x = factor(therapy, levels = therapy_level_order),
  #           y = pred
  #         ),
  #         size = 4, shape = 21, color = "black", fill = "yellow"
  #       ) +
  #       geom_label_repel(
  #         data = test_patient_pred,
  #         aes(
  #           label = paste("patient risk:", paste0(round(pred * 100, digits = 1), "%")),
  #           x = therapy, y = pred
  #         ),
  #         direction = "y", nudge_x = 0.1, fill = "yellow",
  #         color = "#333333", alpha = 0.8,
  #         segment.color = "#333333", segment.alpha = 0.8
  #       ) +
  #       scale_x_discrete(labels = str_wrap("No prophylaxis", width = 20)) +
  #       scale_y_continuous(
  #         limits = c(0, min(1, max(0.2, max(rv$ref_samples$pred), max(rv$test_patient_pred$pred)))),
  #         labels = scales::percent_format(accuracy = 1)
  #       ) +
  #       scale_color_manual(labels = c("No", "Yes"), values = c("#56B4E9", "#D55E00")) +
  #       guides(
  #         size = F,
  #         alpha = F,
  #         color = guide_legend(override.aes = list(size = 4, alpha = 0.8))
  #       ) +
  #       labs(
  #         title = "Predicted risk of PEP",
  #         x = "Treatment",
  #         y = "Probability of developing PEP",
  #         color = "Developed PEP"
  #       ) +
  #       theme_classic(base_size = 15) +
  #       theme(
  #         plot.title = element_text(hjust = 0.5),
  #         # axis.text.x = element_blank(),
  #         axis.title.x = element_blank(),
  #         # axis.ticks.x = element_blank(),
  #         legend.text = element_text(size = 15),
  #         legend.position = "top"
  #       )
  #   )
  # })
  

  # Plot of votes for other treatment options
  output$votesTrtPlot <- renderPlot({
    set.seed(1) # for geom_jitter to look the same every time
    ref_samples <- rv$ref_samples %>% filter(therapy != "No treatment")
    test_patient_pred <- rv$test_patient_pred %>% filter(therapy != "No treatment")
    pep_pred_percent <- rv$test_patient_pred %>%
      filter(therapy == "No treatment") %>%
      pull(pred)
    pep_pred_percent <- round(pep_pred_percent * 100, digits = 1)
    input$update
    
    
    pep_pred_percent <- pep_percent()
    if (pep_pred_percent == 0) {
      pep_pred_percent = NULL;
    }
    isolate(
      ggplot() +
        theme_classic(base_size = 15) +
        theme(
          axis.text.x = element_text(size = 50),  # Adjust the font size for the x-axis labels
          axis.text.y = element_text(size = 50)   # Adjust the font size for the y-axis labels
        ) + 
        guides(fill = guide_legend(title = NULL, override.aes = list(color = NA))) +
        geom_col(
          data = test_patient_pred,
          aes(
            x = factor(therapy, levels = therapy_level_order),
            y = pred,  
            fill = "#56B4E9"
          ),
          width = 0.7,
          color = "black"
        ) +
        #TODO add the red dotted line at the no treatment value
        geom_hline(
          yintercept = pep_pred_percent,  # Specify the y value where the line should be placed
          color = "red",     # Set the color of the line
          linetype = "dashed"  # Set the line type
        ) +
        geom_label(
          x = 1, y = 0.5,  # Adjust the position as needed
          label = "2",
          size = 50,
          fill = "green",
          color = "white",
          label.padding = unit(1, "lines"),
          label.size = 0,
          show.legend = FALSE
        ) +
        scale_x_discrete(
          labels = c(
            "Hydration", "NSAID",
            "Hydration and NSAID", "PD Stent",
            "NSAID and PD stent"
          )
        ) +
        
        geom_text(
          data = test_patient_pred,
          aes(
            x = factor(therapy, levels = therapy_level_order),
            y = pred,
            label = paste0(round(pred * 100, digits = 1), "%"),
            vjust = -0.5
          ),
          size = 6,
          color = "black"
        ) + 
        
        scale_y_continuous(
          limits = c(
            0, min(
              1, max(
                0.2, max(rv$ref_samples$pred), max(rv$test_patient_pred$pred)
              )
            )
          ),
          labels = scales::percent_format(accuracy = 1)
        ) +
        scale_fill_manual(labels = c("No", "Yes"), values = c("#56B4E9", "#D55E00")) +
        labs(
          title = "Predicted risk of PEP with treatment",
          x = "Treatment",
          y = "Probability of developing PEP"
        ) +
        theme_classic(base_size = 15) +
        theme(
          plot.title = element_text(hjust = 0.45),
          axis.text.x = element_text(size = 15),
          legend.text = element_text(size = 1),
          legend.position = "none"
        )
    )
  })

  observeEvent(input$link_to_about, {
    updateTabsetPanel(session, "navbar", "About")
  })

  # output$pep_pred <- renderText({
  #   pep_pred_percent <- rv$test_patient_pred %>%
  #     filter(therapy == "No treatment") %>%
  #     pull(pred)
  #   pep_pred_percent <- round(pep_pred_percent * 100, digits = 1)
  #   HTML(paste0("Based on the input risk factors for the patient, <b> the risk of developing PEP
  #               without prophylaxis is ", pep_pred_percent, "%.</b>"))
  # })

  # Info pop-ups
  observeEvent(input$show_trainee, {
    showModal(modalDialog(
      title = "Trainee involvement",
      "Whether a trainee was involved in the ERCP procedure",
      easyClose = TRUE
    ))
  })

  observeEvent(input$show_x2gw_pass_pd, {
    showModal(modalDialog(
      title = ">2 Guidewire passes",
      "More than two guidewire passes into pancreatic duct",
      easyClose = TRUE
    ))
  })
}

# Run the application
shinyApp(ui = ui, server = server)

