#  Multinomial logit model:


library('apollo')
library('mlogit')

#### Exercise 4, page 5 ####
#### a) How well do the estimated probabilities match the shares of customers choosing each alternative?####

rm(list = ls())

# ################################################################# #
#### LOAD LIBRARY AND DEFINE CORE SETTINGS                       ####
# ################################################################# #
### Initialise code
apollo_initialise()

### Set core controls
apollo_control = list(
  modelName  ="Apollo_mnl_ex4",
  modelDescr ="example",
  indivID    ="idcase",
  panelData = TRUE
)

# ################################################################# #
#### LOAD DATA AND APPLY ANY TRANSFORMATIONS                     ####
# ################################################################# #

#### Loading data from mlogit package  and preprocessing####
data("Heating", package = "mlogit")
database <- data.frame(Heating)
# A list with numeric values of alternatives:
l1 <- seq(1, length(unique(database$depvar)))
names(l1) <- unique(database$depvar)
# During transformation from mlogit data class into data.frame column depvar tranformed into factor, transform 
# this factor column into column of characters":
database$depvar <- as.character(database$depvar)
# Creating a column with a numeric version of alternatives:
database$alt_num <- l1[database$depvar]


# ################################################################# #
#### DEFINE MODEL PARAMETERS                                     ####
# ################################################################# #

### Vector of parameters, including any that are kept fixed in estimation

apollo_beta <- c(ic_coef  = 0, 
                oc_coef  = 0,
                c_ec = 0,
                c_er = 0,
                c_gc = 0,
                c_gr = 0)

### Vector with names (in quotes) of parameters to be kept fixed at their starting value in apollo_beta, use apollo_beta_fixed = c() if none

apollo_fixed = c()


# ################################################################# #
#### GROUP AND VALIDATE INPUTS                                   ####
# ################################################################# #

apollo_inputs = apollo_validateInputs()

# ################################################################# #
#### DEFINE MODEL AND LIKELIHOOD FUNCTION                        ####
# ################################################################# #

apollo_probabilities=function(apollo_beta, apollo_inputs, functionality="estimate"){
  
  ### Attach inputs and detach after function exit
  apollo_attach(apollo_beta, apollo_inputs)
  on.exit(apollo_detach(apollo_beta, apollo_inputs))
  
  ### Create list of probabilities P
  P = list()
  
  ### List of utilities: these must use the same names as in mnl_settings, order is irrelevant
  V <- list()
  
  V[['ec']]  <-  c_ec + (ic_coef * ic.ec + oc_coef * oc.ec)
  V[['er']]  <-  c_er + (ic_coef * ic.er + oc_coef * oc.er)
  V[['gc']]  <-  c_gc + (ic_coef * ic.gc + oc_coef * oc.gc)
  V[['gr']]  <-  c_gr + (ic_coef * ic.gr + oc_coef * oc.gr)
  V[['hp']]  <-  0
  
  ### Define settings for mnl:
  mnl_settings = list(
    alternatives = c(gc=1, er=2, gr=3, hp=4, ec=5),
    avail        = list(ec=1, er=1, gc=1, gr=1, hp=1),
    choiceVar   = database$alt_num,
    V            = V, 
    estimateRoutine = "maxLik"
  )
  
  
  ### Compute logit probabilities
  P[["model"]]=apollo_mnl(mnl_settings, functionality)
  
  ### Take product across observation for same individual
  P = apollo_panelProd(P, apollo_inputs, functionality)
  
  ### Prepare and return outputs of function
  P = apollo_prepareProb(P, apollo_inputs, functionality)
  return(P)
}
  
# ################################################################# #
#### MODEL ESTIMATION                                            ####
# ################################################################# #

model = apollo_estimate(apollo_beta, apollo_fixed, apollo_probabilities, apollo_inputs, 
                        estimate_settings = list(estimationRoutine = "nr", print.Level = 0))

# ----------------------------------------------------------------- #
#---- FORMATTED OUTPUT (TO SCREEN)                               ----
# ----------------------------------------------------------------- #

apollo_modelOutput(model, modelOutput_settings = list(printPVal = TRUE, printT1 = TRUE))

fitted_values <- na.omit(data.frame(apollo_prediction(model, apollo_probabilities, apollo_inputs, modelComponent = "model")))
apply(fitted_values, 2, mean)[4:ncol(fitted_values) - 1]


#### b) Calculate the wtp and discount rate r that is implied by the estimates. Are these reasonable?####

wtp <- coef(model)["oc_coef"] / coef(model)["ic_coef"]
r <- 1 / wtp
r

#### c) Update reflevel in model from 'hp' to 'gr' ####
#### Modified model apollo_probabalities: ####
apollo_beta <- c(ic_coef  = 0, 
                 oc_coef  = 0,
                 c_ec = 0,
                 c_er = 0,
                 c_gc = 0,
                 c_hp = 0)

apollo_probabilities=function(apollo_beta, apollo_inputs, functionality="estimate"){
  
  ### Attach inputs and detach after function exit
  apollo_attach(apollo_beta, apollo_inputs)
  on.exit(apollo_detach(apollo_beta, apollo_inputs))
  
  ### Create list of probabilities P
  P = list()
  
  ### List of utilities: these must use the same names as in mnl_settings, order is irrelevant
  V <- list()
  
  V[['ec']]  <-  c_ec + (ic_coef * ic + oc_coef * oc)
  V[['er']]  <-  c_er + (ic_coef * ic + oc_coef * oc)
  V[['gc']]  <-  c_gc + (ic_coef * ic + oc_coef * oc)
  V[['gr']]  <-  0
  V[['hp']]  <-  c_hp + (ic_coef * ic + oc_coef * oc)
  
  ### Define settings for mnl:
  mnl_settings = list(
    alternatives = c(ec=1, er=2, gc=3, gr=4, hp=5),
    avail        = list(ec=1, er=1, gc=1, gr=1, hp=1),
    choiceVar   = database$alt_num,
    V            = V, 
    rows = database$depvar
  )
  
  
  ### Compute logit probabilities
  P[["model"]]=apollo_mnl(mnl_settings, functionality)
  
  ### Take product across observation for same individual
  P = apollo_panelProd(P, apollo_inputs, functionality)
  
  ### Prepare and return outputs of function
  P = apollo_prepareProb(P, apollo_inputs, functionality)
  return(P)
}
#### ####
model2 = apollo_estimate(apollo_beta, apollo_fixed, apollo_probabilities, apollo_inputs, 
                        estimate_settings = list(estimationRoutine = "nr", print.Level = 0, silent = TRUE))

apollo_modelOutput(model2, modelOutput_settings = list(printPVal = TRUE, printT1 = TRUE))

