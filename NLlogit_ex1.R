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
  indivID    = "ID",
  panelData = TRUE)

# ################################################################# #
#### LOAD DATA AND APPLY ANY TRANSFORMATIONS                     ####
# ################################################################# #

#### Loading data from mlogit package  and preprocessing####
data("HC", package = "mlogit")
database <- data.frame(HC)
# Make id column, as is required for apollo package
database[["ID"]] <- seq(1, dim(database)[1])
# During transformation from mlogit data class into data.frame column depvar tranformed into factor, transform 
# this factor column into column of characters":
database$depvar <- as.character(database$depvar)

cooling.modes <- (database[['depvar']]) %in% c('gcc', 'ecc', 'erc', 'hpc')
room.modes <- (database[['depvar']]) %in% c('erc', 'er')
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
l1 <- seq(1, length(unique(database$depvar)))
names(l1) <- unique(database$depvar)
# Creating a column with a numeric version of alternatives:
database$alt_num <- l1[database$depvar]
# Creating new columns related to alternative specific:
x_vars <- list("icca", "occa", "inc.room", "inc.cooling", "int.cooling")
y_alt <- as.character(unique(database$depvar))
for (i in x_vars){
  for (j in y_alt){
    database[[paste(i, ".", j, sep='')]] <- (database$depvar == j) * database[[i]]
  }
}


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
                 lambda_PT = 0.95)

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
  V[['gcc']]  <-  ich_coef * ich.gcc + och_coef * och.gcc + icca_coef * icca.gcc + occa_coef * occa.gcc + inc.room_coef * inc.room.gcc 
  + inc.cooling_coef * inc.cooling.gcc + int.cooling_coef * int.cooling.gcc
  V[['ecc']]  <-  ich_coef * ich.ecc + och_coef * och.ecc + icca_coef * icca.ecc + occa_coef * occa.ecc + inc.room_coef * inc.room.ecc 
  + inc.cooling_coef * inc.cooling.ecc + int.cooling_coef * int.cooling.ecc
  V[['erc']]  <-  ich_coef * ich.erc + och_coef * och.erc + icca_coef * icca.erc + occa_coef * occa.erc + inc.room_coef * inc.room.erc 
  + inc.cooling_coef * inc.cooling.erc + int.cooling_coef * int.cooling.erc
  V[['hpc']]  <-  ich_coef * ich.hpc + och_coef * och.hpc + icca_coef * icca.hpc + occa_coef * occa.hpc + inc.room_coef * inc.room.hpc 
  + inc.cooling_coef * inc.cooling.hpc + int.cooling_coef * int.cooling.hpc
  V[['gc']]  <-  ich_coef * ich.gc + och_coef * och.gc + icca_coef * icca.gc + occa_coef * occa.gc + inc.room_coef * inc.room.gc 
  + int.cooling_coef * int.cooling.gc
  V[['ec']]  <-  ich_coef * ich.ec + och_coef * och.ec + icca_coef * icca.ec + occa_coef * occa.ec + inc.room_coef * inc.room.ec 
  + int.cooling_coef * int.cooling.ec
  V[['er']]  <-  ich_coef * ich.er + och_coef * och.er + icca_coef * icca.er + occa_coef * occa.er + inc.room_coef * inc.room.er 
  + int.cooling_coef * int.cooling.er
  
  ### Specify nests for NL model
  nlNests = list(root=1, other=lambda_PT)
  
  ### Specify tree structure for NL model
  nlStructure= list()
  nlStructure[["root"]]   = c("gcc","ecc", "erc", "hpc", "other")
  nlStructure[["other"]]     = c("gc","ec","er")
  
  ### Define settings for mnl:
  nl_settings = list(
    alternatives = c(erc=1, hpc=2, gcc=3, gc=4, er=5, ecc=6, ec=7),
    avail        = list(ec=1, ecc=1, er=1, erc=1, gc=1, gcc=1, hpc=1),
    choiceVar   = database$alt_num,
    V            = V, 
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




