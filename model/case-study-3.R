library(rwebppl) # devtools::install_github("mhtess/rwebppl")
library(tidyverse)
library(aida)    # remotes::install_github("michael-franke/aida-package")
library(readr)
library(furrr)
library(feather)
library(here)

##################################################

# these options help Stan run faster
options(mc.cores = parallel::detectCores())

# use the aida-theme for plotting
theme_set(theme_aida())

# global color scheme / non-optimized
project_colors = c("#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7", "#000000")

# setting theme colors globally
scale_colour_discrete <- function(...) {
  scale_colour_manual(..., values = project_colors)
}
scale_fill_discrete <- function(...) {
  scale_fill_manual(..., values = project_colors)
} 

##################################################

# get empirical data + prior preferences

urlfile = "https://raw.githubusercontent.com/magpie-ea/magpie3-qa-overinfo-free-production/main/data%2Banalysis/data/results_e2_human_category_cleaned4modeling.csv"
empirical_responses <- read_csv(url(urlfile))

urlfile = "https://raw.githubusercontent.com/magpie-ea/magpie3-qa-overinfo-free-production/main/data%2Banalysis/data/PragmaticQA-E2-priorElicitation-sliderRating-full_120.csv"
priors <- read_csv(url(urlfile))
scenarios <- unique(empirical_responses$itemName)

full_matrix <- priors %>%
  filter(trial_type == "main") %>%
  select(submission_id, itemName, targetOption, mostSimilar,
         itemQuestion, competitor, otherCategory, sameCategory) 


#######################################
# 
# policyAlpha = c(1,3,5,7,9)
# questionerAlpha = c(1,3,5,7,9)
# R1Alpha = c(1,3,5,7,9)
# relevanceBetaR0 = c(0)
# relevanceBetaR1 = c(0.1, 0.3, 0.5, 0.7, 0.9)
# costWeight = c(0.1, 0.3, 0.5, 0.7, 0.9)
# n_sample <- c(1,2,3,4,5)
# questionCost <- c(0)
# 
# param_space <- expand_grid(policyAlpha, questionerAlpha, R1Alpha, relevanceBetaR0, relevanceBetaR1, costWeight, questionCost, scenarios, n_sample)

##############################################

run_model_tsos <- function (params, utils) {
  webPPL_data <- tibble('task' = "TSOS") %>% 
    cbind(params) %>% 
    cbind(utils)
  
  webppl(
    program_file = here("model/case-study-3.wppl"),
    packages = here("model/pragmaticQAModel"),
    data = webPPL_data,
    data_var = "RInput"
  ) -> output
  
  return(output)
}

priorSampleParams <- function() {
  params <- tibble(
    'policyAlpha'      = runif(1,min = 8.536908820191446, max = 8.536908820191446), # searched 0-10
    'questionerAlpha'  = runif(1,min = 5.892578142591768, max = 5.892578142591768), # searched 0-10
    'R1Alpha'          = runif(1,min = 2.9436908699427895, max = 2.9436908699427895), # searched 0-50
    'relevanceBetaR0'  = runif(1,min = 0, max = 0), # fixed at 0
    'relevanceBetaR1'  = runif(1,min = 0.2907849349998428, max = 0.2907849349998428), # searched 0-1
    'costWeight'       = runif(1,min = 2.3421835313724717, max = 2.3421835313724717), # searched 0-5
    'failure'          = runif(1,min = -5.936552662124935, max = -5.936552662124935), # searched -10 to 10
    'questionCost'     = runif(1,min = 0, max = 0) # fixed at 0
  )
  return(params)
}


empiricalPrior <- function(scenario) {
  these_priors <- full_matrix %>% 
    filter(itemName == scenario) %>%
    group_by(targetOption) %>%
    sample_n(1)
  
  utils <- tibble(
    'utilTarget'       = these_priors$itemQuestion/10,
    'utilCompetitor'   = these_priors$competitor/10,
    'utilSameCat'      = these_priors$sameCategory/10,
    'utilOtherCat'     = these_priors$otherCategory/10,
    'utilMostSimilar'  = these_priors$mostSimilar/10
  )
  return(utils)
}

# run samples in parallel 
samples_each = 20
scenarios_rep = rep(scenarios, samples_each)
n_samples = length(scenarios_rep)


param_search = FALSE

plan(multisession, workers = 8)

if (param_search == TRUE) {
  n_samples = nrow(param_space)
}
priorPred <- furrr::future_map_dfr(1:n_samples, function(i) {
  message('run ', i)
  if (param_search == TRUE) {
    scenario <- param_space[i,]['scenarios'] %>% pull()
    params <- param_space[i,] %>% select(-scenarios, -n_sample) %>% tibble()
  } else {
    scenario = scenarios_rep[i]
    params <- priorSampleParams()
  }
  utils  <- empiricalPrior(scenario)
  out    <- tibble('run' = i) %>%
    cbind(params) %>%
    cbind(scenario) %>%
    cbind(run_model_tsos(params, utils))
    return (out)
}, .progress = TRUE, .options = furrr_options(seed = 123))

if (param_search == TRUE) {
  write_csv(priorPred, here('data/priorpq/case_study_3/c3_parameter_search.csv'))
} else {
  write_csv(priorPred, here('data/priorpq/case_study_3/c3_model_preds_full.csv'))
}

