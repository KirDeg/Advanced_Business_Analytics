#  Multinomial logit model:

library('apollo')
library('mlogit')

#### Exercise 6, page 12 ####
#### Estimate a nested model logit: ####

rm(list = ls())

# ################################################################# #
#### LOAD LIBRARY AND DEFINE CORE SETTINGS                       ####
# ################################################################# #
### Initialise code
apollo_initialise()

### Set core controls
apollo_control = list(
  modelName  ="Apollo_NL_ex1",
  modelDescr ="example",
  indivID    = "chid",
  noValidation = TRUE,
  silent = TRUE)

# ################################################################# #
#### LOAD DATA AND APPLY ANY TRANSFORMATIONS                     ####
# ################################################################# #

#### Loading data from mlogit package  and preprocessing####
data("HC", package = "mlogit")
database <- data.frame(mlogit.data(HC, varying = c(2:8, 10:16), choice = "depvar", shape = "wide")) 
cooling.modes <- (database[['alt']]) %in% c('gcc', 'ecc', 'erc', 'hpc')
room.modes <- (database[['alt']]) %in% c('erc', 'er')
# installation / operating costs for cooling are constants,
# only relevant for mixed systems
database$icca[!cooling.modes] <- 0
database$occa[!cooling.modes] <- 0
# create income variables for two sets cooling and rooms
database$inc.cooling <- database$inc.room <- 0
database$inc.cooling[cooling.modes] <- database$income[cooling.modes]
database$inc.room[room.modes] <- database$income[room.modes]
# create an intercet for cooling modes
database$int.cooling <- as.numeric(cooling.modes)
# A list with numeric values of alternatives:
l1 <- seq(1, length(unique(database$alt)))
names(l1) <- unique(database$alt)
# Creating a column with a numeric version of alternatives:
database$alt_num <- l1[database$alt]



# ################################################################# #
#### DEFINE MODEL PARAMETERS                                     ####
# ################################################################# #

### Vector of parameters, including any that are kept fixed in estimation

apollo_beta <- c(ich_coef  = 0, 
                 och_coef  = 0,
                 icca_coef = 0,
                 occa_coef = 0,
                 inc.room_coef = 0,
                 inc.cooling_coef = 0,
                 int.cooling_coef = 0,
                 lambda_PT = 0.01)

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
  
  ### List of utilities: these must use the same names as in nl_settings, order is irrelevant
  V <- list()
  V[['gcc']]  <-  ich_coef * ich + och_coef * och + icca_coef * icca + occa_coef * occa + inc.room_coef * inc.room 
  + inc.cooling_coef * inc.cooling + int.cooling_coef * int.cooling
  V[['ecc']]  <-  ich_coef * ich + och_coef * och + icca_coef * icca + occa_coef * occa + inc.room_coef * inc.room 
  + inc.cooling_coef * inc.cooling + int.cooling_coef * int.cooling
  V[['erc']]  <-  ich_coef * ich + och_coef * och + icca_coef * icca + occa_coef * occa + inc.room_coef * inc.room 
  + inc.cooling_coef * inc.cooling + int.cooling_coef * int.cooling
  V[['hpc']]  <-  ich_coef * ich + och_coef * och + icca_coef * icca + occa_coef * occa + inc.room_coef * inc.room 
  + inc.cooling_coef * inc.cooling + int.cooling_coef * int.cooling
  V[['gc']]  <-  ich_coef * ich + och_coef * och + icca_coef * icca + occa_coef * occa + inc.room_coef * inc.room 
  + inc.cooling_coef * inc.cooling + int.cooling_coef * int.cooling
  V[['ec']]  <-  ich_coef * ich + och_coef * och + icca_coef * icca + occa_coef * occa + inc.room_coef * inc.room 
  + inc.cooling_coef * inc.cooling + int.cooling_coef * int.cooling
  V[['er']]  <-  ich_coef * ich + och_coef * och + icca_coef * icca + occa_coef * occa + inc.room_coef * inc.room 
  + inc.cooling_coef * inc.cooling + int.cooling_coef * int.cooling
  
  ### Specify nests for NL model
  nlNests = list(root=1, other=lambda_PT)
  
  ### Specify tree structure for NL model
  nlStructure= list()
  nlStructure[["root"]]   = c("gcc","ecc", "erc", "hpc", "other")
  nlStructure[["other"]]     = c("gc","ec","er")
  
  ### Define settings for mnl:
  nl_settings = list(
    alternatives = c(ec=1, ecc=2, er=3, erc=4, gc=5, gcc=6, hpc=7),
    avail        = list(ec=1, ecc=1, er=1, erc=1, gc=1, gcc=1, hpc=1),
    choiceVar   = database$alt_num,
    V            = V, 
    rows = database$depvar,
    nlNests      = nlNests,
    nlStructure  = nlStructure
  )
  
  
  ### Compute probabilities using NL model
  P[["model"]] = apollo_nl(nl_settings, functionality)
  
  ### Take product across observation for same individual
  P = apollo_panelProd(P, apollo_inputs, functionality)
  
  ### Prepare and return outputs of function
  P = apollo_prepareProb(P, apollo_inputs, functionality)
  return(P)
}

# ################################################################# #
#### MODEL ESTIMATION                                            ####
# ################################################################# #

model = apollo_estimate(apollo_beta, apollo_fixed, apollo_probabilities, apollo_inputs)

# ################################################################# #
#### MODEL OUTPUTS                                               ####
# ################################################################# #

# ----------------------------------------------------------------- #
#---- FORMATTED OUTPUT (TO SCREEN)                               ----
# ----------------------------------------------------------------- #

apollo_modelOutput(model)




