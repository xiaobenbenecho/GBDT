
library(here)
library(base)
library(freqdom)


#######################################

# train data
trainfile <- paste0("experiment_example","train.RData")
# Load train matrix, vars etc.
load(here::here("ModelData",trainfile))

testfile <- paste0("experiment_example","test.RData")
# Load train matrix, vars etc.
load(here::here("ModelData",testfile))


X<-rbind(Xtrain, Xtest)



num_train <- as.integer((ratio/(ratio+1))*dim(X)[1])
Xtrain1 <- X[0:num_train,]
Xtest1 <- X[(num_train+1):dim(X)[1],]

# Train diagnostics
dir.create(here::here(name))

temp <- testvals$stopvalid[1:(num_train - dim(Xtrain)[1])]
ytrain <- c(ytrain, temp)
testvals <- testvals[(num_train - dim(Xtrain)[1] +1) : dim(Xtest)[1],]

save(Xtrain1, ytrain, file = here::here(name, 'traindpca.RData'))
save(Xtest1, testvals, file = here::here(name, 'testdpca.RData'))
#save(res.dpca, file = here::here(name, paste0("dpca", Num_dpca,".RData")))

######################################################################

# Xtrain <- scale(Xtrain)
# Xtrain.dpca = dpca(Xtrain, q = 60, Ndpc = dim(Xtrain)[2])
# var.dpca = (1 - sum( (Xtrain.dpca$Xhat - Xtrain)**2 ) / sum(Xtrain**2))*100
# cat("Variance explained by DPCA:\t",var.dpca,"%\n")
# Xtrain = Xtrain.dpca$scores
# save(Xtrain, ytrain, file = here::here(name, 'traindpca.RData'))
# 
# 
# 
# 
# # test data
# 
# 
# Xtest <- scale(Xtest)
# Xtest.dpca = dpca(Xtest, q = 60, Ndpc = dim(Xtest)[2])
# var.dpca = (1 - sum( (Xtest.dpca$Xhat - Xtest)**2 ) / sum(Xtest**2))*100
# cat("Variance explained by DPCA:\t",var.dpca,"%\n")
# Xtest <- Xtest.dpca$scores
# save(Xtest, testvals, file = here::here(name, 'testdpca.RData'))
# 
# 
# 
# 
# 
