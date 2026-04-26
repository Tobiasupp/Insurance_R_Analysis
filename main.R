library(tidyverse)

source("scripts/01_load_data.R")
source("scripts/02_prepare_data.R")
source("scripts/03_analysis.R")
source("scripts/04_figures.R")
source("scripts/05_regression.R")

view(cor_summary)

view(category_summary)

view(checkup_analysis)

view(age_smoker_summary)

print(plot_smoker)

print(plot_health_score)

print(model_summary)
