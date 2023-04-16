

<!-- ABOUT THE PROJECT -->
# Predicition Verifiaction

This project is meant to serve the goal of verifying the findings of model predictions regarding PEP against real world data.


## Built With

This project was built entirely with R.


<!-- GETTING STARTED -->
## Getting Started

### Prerequisites

In order to run this script your would need to have R and R studio downloaded onto your system.

### Installation

To run the program simply execute the line below in Rstudio once in the correct base directory.
   ```sh
   source("predVerification.R", encoding = "UTF-8")
   ```


Once you run the file, it should create a folder with 2 files within it.

    predictionValues
    ├── answers_pred_sub.xlsx       # Contains the predictions from the fully trained GBM model
    ├── answers_pred.xlsx           # Contains the predictions from the model on TRT subsets