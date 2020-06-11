# install.packages("here")
library(here)
 
#source(here::here("R","installer.R"))

# Indicate what files are used for thr train and the testset
ratio = 6
#subset <- list(train = 1:14,test = 15:21) 
# This can be changed, but it won't be done usually
#source(here::here("R","markstops.R"))

# Experiment name
name <- "5experiment_example"
# Script saved in ExperimentScripts
#preparation_script <- "prepareData_ex1.R"
# get the design matrices ready. For the moment no csv output is produced but it is a possibility
#source(here::here("ExperimentScripts",preparation_script))


# parameters for the model change the experiment name each time you play with them
# The sample model has low nrounds and early stop round and only 2 folds
# param <- list(max_depth=15, eta=0.005,
#               subsample=0.2,colsample_bytree =0.5, 
#               silent=1,min_child_weight=60, objective= "binary:logistic")

# The ones over are real, this is for being able to run the test
 param <- list(max_depth=10, eta=0.005,
               subsample=0.2,colsample_bytree =0.5, 
              silent=1,min_child_weight=60, objective= "binary:logistic")

 
 #Run feature engineer
 Num_dpca = 82
 source(here::here("R", "featureEngineer3.R"))
 
 # Run model
 source(here::here("R","Model.R"))
 
# The model is an XGB but it could be any other
 
 