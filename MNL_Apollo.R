#  Multinomial logit model:

library("mlogit")
library('apollo')

# ################################################################# #
#### LOAD LIBRARY AND DEFINE CORE SETTINGS                       ####
# ################################################################# #

rm(list = ls())

#### Loading data from mlogit package ####
data("Heating", package = "mlogit")
data_raw <- mlogit.data(Heating, shape = "wide", choice = "depvar", varying = c(3:12))

### Initialise code
apollo_initialise()

### Set core controls
apollo_control = list(
  modelName  ="Apollo_example_12",
  modelDescr ="MDCEV model on time use data, alpha-gamma profile with outside good and socio-demographics",
  indivID    ="indivID"
)
