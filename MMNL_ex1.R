# ################################################################# #
#### LOAD LIBRARY AND DEFINE CORE SETTINGS                       ####
# ################################################################# #

### Load Apollo and mlogit libraries
library(apollo)
library("mlogit")
### Initialise code
apollo_initialise()

### Set core controls
apollo_control = list(
  modelName ="Mixed logit",
  modelDescr ="Mixed logit model1",
  indivID   ="id",  
  mixing    = TRUE,
  nCores = 4
)

# ################################################################# #
#### LOAD DATA AND APPLY ANY TRANSFORMATIONS                     ####
# ################################################################# #

data("Electricity", package = "mlogit")
database <- data.frame(Electricity)

# ################################################################# #
#### DEFINE MODEL PARAMETERS                                     ####
# ################################################################# #

### Vector of parameters, including any that are kept fixed in estimation
apollo_beta = c(mu_pf               =0,
                mu_cl               =0,
                mu_loc               =0,
                mu_wk               =0,
                mu_tod               =0,
                mu_seas               =0,
                sd_pf            = 0,
                sd_cl            = 0,
                sd_loc            = 0,
                sd_wk            = 0,
                sd_tod            = 0,
                sd_seas            = 0)

### Vector with names (in quotes) of parameters to be kept fixed at their starting value in apollo_beta, use apollo_beta_fixed = c() if none
apollo_fixed = c()

# ################################################################# #
#### DEFINE RANDOM COMPONENTS                                    ####
# ################################################################# #

### Set parameters for generating draws
apollo_draws = list(
  interDrawsType = "pmc",
  interNDraws    = 100,
  interUnifDraws = c(),
  interNormDraws = c("draws_pf","draws_cl","draws_loc","draws_wk", "draws_tod", "draws_seas"),
  intraDrawsType = "pmc",
  intraNDraws    = 0,
  intraUnifDraws = c(),
  intraNormDraws = c()
)

### Create random parameters
apollo_randCoeff = function(apollo_beta, apollo_inputs){
  randcoeff = list()
  
  randcoeff[["c_pf"]] = mu_pf + sd_pf * draws_pf
  randcoeff[["c_cl"]] = mu_cl + sd_cl * draws_cl
  randcoeff[["c_loc"]] = mu_loc + sd_loc * draws_loc
  randcoeff[["c_wk"]] = mu_wk + sd_wk * draws_wk
  randcoeff[["c_tod"]] = mu_tod + sd_tod * draws_tod
  randcoeff[["c_seas"]] = mu_seas + sd_seas * draws_seas
  
  return(randcoeff)
}

# ################################################################# #
#### GROUP AND VALIDATE INPUTS                                   ####
# ################################################################# #

apollo_inputs = apollo_validateInputs()

# ################################################################# #
#### DEFINE MODEL AND LIKELIHOOD FUNCTION                        ####
# ################################################################# #

apollo_probabilities=function(apollo_beta, apollo_inputs, functionality="estimate"){
  
  ### Function initialisation: do not change the following three commands
  ### Attach inputs and detach after function exit
  apollo_attach(apollo_beta, apollo_inputs)
  on.exit(apollo_detach(apollo_beta, apollo_inputs))
  
  ### Create list of probabilities P
  P = list()
  
  ### List of utilities: these must use the same names as in mnl_settings, order is irrelevant
  V = list()
  V[['alt1']] = c_pf * pf1 + c_cl * cl1 + c_loc * loc1 + c_wk * wk1 + c_tod * tod1 + c_seas * seas1
  V[['alt2']] = c_pf * pf2 + c_cl * cl2 + c_loc * loc2 + c_wk * wk2 + c_tod * tod2 + c_seas * seas2
  V[['alt3']] = c_pf * pf3 + c_cl * cl3 + c_loc * loc3 + c_wk * wk3 + c_tod * tod3 + c_seas * seas3
  V[['alt4']] = c_pf * pf4 + c_cl * cl4 + c_loc * loc4 + c_wk * wk4 + c_tod * tod4 + c_seas * seas4
  
  ### Define settings for MNL model component
  mnl_settings = list(
    alternatives  = c(alt1=1, alt2=2, alt3=3, alt4=4),
    avail         = list(alt1=1, alt2=1, alt3=1, alt4=1),
    choiceVar     = choice,
    V             = V
  )
  
  ### Compute probabilities using MNL model
  P[['model']] = apollo_mnl(mnl_settings, functionality)
  
  ### Take product across observation for same individual
  P = apollo_panelProd(P, apollo_inputs, functionality)
  
  ### Average across inter-individual draws
  P = apollo_avgInterDraws(P, apollo_inputs, functionality)
  
  ### Prepare and return outputs of function
  P = apollo_prepareProb(P, apollo_inputs, functionality)
  return(P)
}

# ################################################################# #
#### MODEL ESTIMATION                                            ####
# ################################################################# #

model = apollo_estimate(apollo_beta, apollo_fixed,
                        apollo_probabilities, apollo_inputs, estimate_settings=list(hessianRoutine="maxLik", estimationRoutine = "bfgs"))

# ################################################################# #
#### MODEL OUTPUTS                                               ####
# ################################################################# #

# ----------------------------------------------------------------- #
#---- FORMATTED OUTPUT (TO SCREEN)                               ----
# ----------------------------------------------------------------- #

apollo_modelOutput(model)

