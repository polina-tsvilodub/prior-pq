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
priors <- read_csv(here('data/human/case_study_3/e3_prior_elicitation_human.csv'))
scenarios <- unique(empirical_responses$itemName)

full_matrix <- priors %>%
  filter(trial_type == "main") %>%
  select(submission_id, itemName, targetOption, mostSimilar,
         itemQuestion, competitor, otherCategory, sameCategory)


#######################################

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
    'relevanceBetaR1'  = runif(1,min = 0.2907849349998428, max = 0.2907849349998428), # searched 0-1
    'costWeight'       = runif(1,min = 2.3421835313724717, max = 2.3421835313724717), # searched 0-5
    'failure'          = runif(1,min = -5.936552662124935, max = -5.936552662124935), # searched to 10
    'questionCost'     = runif(1,min = 0, max = 0) # fixed at 0
  )
  return(params)
}


empiricalPrior <- function(scenario) {
  these_priors <- full_matrix %>%
    mutate(targetOption = fct_relevel(targetOption, 'itemQuestion', 'competitor', 'sameCategory', 'otherCategory', 'mostSimilar')) %>%
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

plan(multisession, workers = 8)

priorPred <- furrr::future_map_dfr(1:n_samples, function(i) {
  message('run ', i)
  scenario = scenarios_rep[i]
  params <- priorSampleParams()
  utils  <- empiricalPrior(scenario)
  print(utils)
  out    <- tibble('run' = i) %>%
    cbind(params) %>%
    cbind(scenario) %>%
    cbind(run_model_tsos(params, utils))
    return (out)
}, .progress = TRUE, .options = furrr_options(seed = 123))

write_csv(priorPred, here('data/priorpq/case_study_3/c3_model_preds_full.csv'))


