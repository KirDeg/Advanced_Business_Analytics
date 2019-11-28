#  Multinomial logit model:

library('apollo')

#### Exercise 7, page 10 ####
#### Estimate a model with installation costs, operating costs, and alternative specific constants: ####

rm(list = ls())

# ################################################################# #
#### LOAD LIBRARY AND DEFINE CORE SETTINGS                       ####
# ################################################################# #
### Initialise code
apollo_initialise()

### Set core controls
apollo_control = list(
  modelName  ="Apollo_mnl_ex7",
  modelDescr ="example",
  indivID    ="idcase"
)

# ################################################################# #
#### LOAD DATA AND APPLY ANY TRANSFORMATIONS                     ####
# ################################################################# #

#### Loading data from mlogit package  and preprocessing####
data("Heating", package = "mlogit")
database <- Heating
database <- data.frame(mlogit.data(Heating, shape = "wide", choice = "depvar", varying = c(3:12)))
# A list with numeric values of alternatives:
l1 <- seq(1, length(unique(database$alt)))
names(l1) <- unique(database$alt)
# Creating a column with a numeric version of alternatives:
database$alt_num <- l1[database$alt]


# ################################################################# #
#### DEFINE MODEL PARAMETERS                                     ####
# ################################################################# #

### Vector of parameters, including any that are kept fixed in estimation

apollo_beta <- c(ic_coef  = 0, 
                 oc_coef  = 0,
                 c_ec = 0,
                 c_er = 0,
                 c_gc = 0,
                 c_hp = 0)

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

# ################################################################# #
#### MODEL ESTIMATION                                            ####
# ################################################################# #

model = apollo_estimate(apollo_beta, apollo_fixed, apollo_probabilities, apollo_inputs, 
                        estimate_settings = list(estimationRoutine = "nr", print.Level = 0))

#### Using the estimated coefficients from the model in exercise 6, 
#calculate new probabilities and predicted shares using the new installation cost of heat pump: ####

database[database$alt == 'hp', 'ic'] <- 0.9 * database[database$alt == 'hp', 'ic']

predictions_new = na.omit(data.frame(apollo_prediction(model, apollo_probabilities, apollo_inputs)))

apply(predictions_new, 2, mean)[4:ncol(predictions_new) - 1]
