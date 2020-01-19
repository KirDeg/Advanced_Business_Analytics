#  Multinomial logit model:

library('apollo')
library('mlogit')

#### Exercise 1, page 12 ####
#### Estimate a nested model logit: ####


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
database <- na.omit(data.frame(HC))
# Make id column, as is required for apollo package
database[["ID"]] <- seq(1, dim(database)[1])
# During transformation from mlogit data class into data.frame column depvar tranformed into factor, transform 
# this factor column into column of characters":
database$depvar <- as.character(database$depvar)

cooling.modes <- (database[['depvar']]) %in% c('gcc', 'ecc', 'erc', 'hpc')
room.modes <- (database[['depvar']]) %in% c('erc', 'er')
# create income variables for two sets cooling and rooms
database$inc.cooling <- database$inc.room <- 0
database$inc.cooling <- database$income
database$inc.room <- database$income
# create an intercet for cooling modes
database$int.cooling <- 1
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
      database[[paste(i, ".", j, sep='')]] <- 0
  }
}

for (i in c("icca", "occa")){
  for (j in y_alt){
    if (j %in%  c('gcc', 'ecc', 'erc', 'hpc')){
      database[[paste(i, ".", j, sep='')]] <- database[[i]]
    }
  }
}


for (j in y_alt){
  if (j %in%  c('gcc', 'ecc', 'erc', 'hpc')){
    database[[paste("inc.cooling", ".", j, sep='')]] <- database[["inc.cooling"]]
  }
}


for (j in y_alt){
  if (j %in%  c('erc', 'er')){
    database[[paste("inc.room", ".", j, sep='')]] <- database[["inc.room"]]
  }
}


for (j in y_alt){
  if (j %in%  c('gcc', 'ecc', 'erc', 'hpc')){
    database[[paste("int.cooling", ".", j, sep='')]] <- database[["int.cooling"]]
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
                 lambda = 0.5)

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
  V[['gcc']] <-  ich_coef * ich.gcc + och_coef * och.gcc + icca_coef * icca.gcc + occa_coef * occa.gcc + inc.cooling_coef * inc.cooling.gcc + int.cooling_coef * int.cooling.gcc
  V[['ecc']] <-  ich_coef * ich.ecc + och_coef * och.ecc + icca_coef * icca.ecc + occa_coef * occa.ecc+ inc.cooling_coef * inc.cooling.ecc + int.cooling_coef * int.cooling.ecc
  V[['erc']] <-  ich_coef * ich.erc + och_coef * och.erc + icca_coef * icca.erc + occa_coef * occa.erc + inc.room_coef * inc.room.erc + inc.cooling_coef * inc.cooling.erc + int.cooling_coef * int.cooling.erc
  V[['hpc']]  <-  ich_coef * ich.hpc + och_coef * och.hpc + icca_coef * icca.hpc + occa_coef * occa.hpc + inc.cooling_coef * inc.cooling.hpc + int.cooling_coef * int.cooling.hpc
  V[['gc']] <-  ich_coef * ich.gc + och_coef * och.gc
  V[['ec']] <-  ich_coef * ich.ec + och_coef * och.ec
  V[['er']] <-  ich_coef * ich.er + och_coef * och.er + inc.room_coef * inc.room.er
  
  ### Specify nests for NL model
  nlNests = list(root=1, nest1=lambda, nest2=lambda)
  
  ### Specify tree structure for NL model
  nlStructure= list()
  nlStructure[["root"]]   = c("nest1","nest2")
  nlStructure[["nest1"]]   = c("gcc","ecc", "erc", "hpc")
  nlStructure[["nest2"]]   = c("gc","ec","er")
  
  ### Define settings for nl:
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

model = apollo_estimate(apollo_beta, apollo_fixed, apollo_probabilities, apollo_inputs, estimate_settings = list(print.level = 0, estimationRoutine = "bfgs"))

# ################################################################# #
#### MODEL OUTPUTS                                               ####
# ################################################################# #

# ----------------------------------------------------------------- #
#---- FORMATTED OUTPUT (TO SCREEN)                               ----
# ----------------------------------------------------------------- #

apollo_modelOutput(model)


# b) Test the hypothesis that the log-sum coefficient is 1.0:

(coef(nl)['iv'] - 1) / sqrt(vcov(nl)['iv', 'iv'])

# The critical value of t for 95% confidence is 1.96. So we can reject the hypothesis at 95% confidence.
# We can also use a likelihood ratio test because the multinomial logit is a special case of the nested model.
# First estimate the multinomial logit model

apollo_beta <- c(ich_coef  = 0, 
                 och_coef  = 0,
                 icca_coef = 0,
                 occa_coef = 0,
                 inc.room_coef = 0,
                 inc.cooling_coef = 0,
                 int.cooling_coef = 0)

apollo_probabilities=function(apollo_beta, apollo_inputs, functionality="estimate"){
  
  ### Attach inputs and detach after function exit
  apollo_attach(apollo_beta, apollo_inputs)
  on.exit(apollo_detach(apollo_beta, apollo_inputs))
  
  ### Create list of probabilities P
  P = list()
  
  ### List of utilities: these must use the same names as in nl_settings, order is irrelevant
  V <- list()
  V[['gcc']] <-  ich_coef * ich.gcc + och_coef * och.gcc + icca_coef * icca.gcc + occa_coef * occa.gcc + inc.cooling_coef * inc.cooling.gcc + int.cooling_coef * int.cooling.gcc
  V[['ecc']] <-  ich_coef * ich.ecc + och_coef * och.ecc + icca_coef * icca.ecc + occa_coef * occa.ecc+ inc.cooling_coef * inc.cooling.ecc + int.cooling_coef * int.cooling.ecc
  V[['erc']] <-  ich_coef * ich.erc + och_coef * och.erc + icca_coef * icca.erc + occa_coef * occa.erc + inc.room_coef * inc.room.erc + inc.cooling_coef * inc.cooling.erc + int.cooling_coef * int.cooling.erc
  V[['hpc']]  <-  ich_coef * ich.hpc + och_coef * och.hpc + icca_coef * icca.hpc + occa_coef * occa.hpc + inc.cooling_coef * inc.cooling.hpc + int.cooling_coef * int.cooling.hpc
  V[['gc']] <-  ich_coef * ich.gc + och_coef * och.gc
  V[['ec']] <-  ich_coef * ich.ec + och_coef * och.ec
  V[['er']] <-  ich_coef * ich.er + och_coef * och.er + inc.room_coef * inc.room.er
  
  
  ### Define settings for nl:
  mnl_settings = list(
    alternatives = c(erc=1, hpc=2, gcc=3, gc=4, er=5, ecc=6, ec=7),
    avail        = list(ec=1, ecc=1, er=1, erc=1, gc=1, gcc=1, hpc=1),
    choiceVar   = database$alt_num,
    V            = V)
  
  
  ### Compute probabilities using NL model
  P[["model"]] = apollo_mnl(mnl_settings, functionality)
  
  ### Take product across observation for same individual
  P = apollo_panelProd(P, apollo_inputs, functionality)
  
  ### Prepare and return outputs of function
  P = apollo_prepareProb(P, apollo_inputs, functionality)
  return(P)
}

model2 = apollo_estimate(apollo_beta, apollo_fixed, apollo_probabilities, apollo_inputs, estimate_settings = list(print.level = 0, estimationRoutine = "bfgs"))

apollo_saveOutput(model2)

# LR-test:

apollo_lrTest("Apollo_NL_ex1", model)


