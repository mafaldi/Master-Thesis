# Coming closer: a quantitative analysis of poll prediction accuracy in the 2023 Spanish general elections

> UC3M – Master in Computational Social Sciences – 2024/25. 

This repository contains the scripts and data pipeline developed for the Thesis Final Project. The project focuses on detecting and correcting pollster-party "house effects" in Spain’s 2023 general election, using historical and consensus-based debiasing strategies.

**Data sources:**

1. Official results from the Spanish Ministry of Interior, retrieved via the R package {pollspain}.
2. Polling data scraped from Wikipedia’s Opinion polling for Spanish general elections pages, harmonised into consistent datasets, retrieved via the R package {pollspaindata}.

**Repository structure**

Originally, the code was uploaded exactly the way it was used to write the Thesis Final Project paper, and can be found under the `Original` folder. However, to present a cleaner and more reproducable pipeline for easier navigation and replication, I have divided and cleaned up the scripts. While all work and code remains is the same as in the `Original` folder, the scripts now entail the different parts of the project divided by topic or task.  

The restructred scripts are designed to run sequentially, from raw data ingestion to modelling and visualisation. The cleaned code ensures that users can reproduce the full workflow by following the numbered scripts in order.

#  Instructions for replication 

As mentioned, there are two main paths for looking at the code:

* `Original` -> the original unstructured code as used with thesis version (kept for reference). 
* _Everything not inside `Original`_ -> the reproducible, stepwise pipeline (`0_...` to `5_...`).

Below you can find the content location of Final Thesis Project work both in the `Original` and the restructured scripts. Further down I explain in more detail the code done by the restructured scripts in their pipeline position. 

--- 

### Content location

The table summarizes how the restructured scripts and the `Original` scripts share content script-wise. 

| Direct access scripts | `Original` folder scripts |
|----|----|
| `0. Wikipedia scraper for polling information.qmd` | _Not uploaded before_ |
| `1. Datasets creation and feature engineering.qmd` | `House effects - Data preparation.qmd` & `House effects.qmd` |
| `2. House effects 2023 calculation + EDA.qmd` | `House effects.qmd` |
| `3. Data Historical house effects calculation.qmd` | `House effects.qmd` |
| `4. Modeling for improvement of 2023 pollsters predictions.qmd` | `House effects.qmd` |
| `5. Visualisations for Thesis Document.qmd` | `visualisations for TFM.qmd` |

--- 

### Pipeline structure description

#### `0. Wikipedia scraper for polling information.qmd`

Scrapes and harmonises polling tables from Wikipedia. This scraper is integrated in the {pollspain} package, from which we download and utilize the ultimate polling and election information directly for the Final Thesis Project. 

####  `1. Datasets creation and feature engineering.qmd`

Creation of the initial dataset by merging the election information dataset through the function `summary_election_data()` of {pollspain} and loading of the polling information Rda `historical_surveys.rda`, retrieved from {pollspaindata}. Renames target variable as `voting_results_pct` and constructs new variables (feature engineering). Preprocesses the data for further usage by fragmentating the dataset by time (2023 and historical, i.e. everything before 2023), deleting missing information and applying thresholds for party inclusion (≥2% vote share) and pollster inclusion (>10 polls). 

#### `2. House effects 2023 calculation + EDA.qmd`

Performs exploratory analysis of 2023 polling accuracy, contrasting pollster predictions against actual results. Computes party-level and pollster-level errors (MAE, RMSE, signed error), and visualises systematic under- and overestimation across firms. Produces descriptive plots of directional bias, pollster rankings, and distribution of polling errors by party to confirm the presence of measurable house effects in the 2023 contest.

#### `3. Data Historical house effects calculation.qmd`

Extends the house effects analysis to past elections to test their persistence across cycles and create parallel dataframes for the 2023 and the hisotrical data for downstream usage. Estimates pollster-party average errors historically and compares stability of biases over time. Generates summary tables and plots of pollster deviations across elections, highlighting both (in)consistent tendencies and election-specific shifts.

#### `4. Modeling for improvement of 2023 pollsters predictions.qmd` 

Implements and evaluates debiasing strategies. Introduces correction terms into predictive models under two setups: debiasing "before" modelling (cleaned estimates as inputs) and debiasing "during" modelling (raw estimates + bias term). Benchmarks performance across linear models, random forest, neural networks and k-nearest neighbours using Leave-One-Election-Out cross-validation. Compares RMSE, MAE and R² to assess gains from correction timing and model choice.

#### `5. Visualisations for Thesis Document.qmd`

Produces the final plots and tables integrated in the thesis. Includes visualisations of party-level polling errors, pollster performance rankings, consensus vs. raw predictions, and model performance comparisons. Ensures outputs are formatted consistently for export into the written document and provides a clear graphical summary of the findings.

## Running the code

Clone the repository and open the R project.

Ensure all required R packages are installed and the existence of `historical_surveys.rds` in the `data` folder. It is actually the only database you need in order to build the rest of the project on, as the rest of the files will be created as you advance through the scripts. 

Run the scripts in order (0 to 5) to fully reproduce the thesis workflow.

If you feel like only reproducing one specific script, make sure to download the entirity of the `data` folder as I have uploaded all files necessary for all scripts, so that you do not actually nee dto run everything to get all the outputs used in this project. 

Have fun! :) 

  
