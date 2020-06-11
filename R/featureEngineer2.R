
####################################


library(here)
library(base)
library(freqdom)

#######################################
#keep only 47 variables. 

#######################################

# train data
trainfile <- paste0("experiment_example","train.RData")
# Load train matrix, vars etc.
load(here::here("ModelData",trainfile))

testfile <- paste0("experiment_example","test.RData")
# Load train matrix, vars etc.
load(here::here("ModelData",testfile))


#read csv file.
cols <- c()
for (col in colnames(Xtrain)){
  if ((!grepl("\\d", col)) ||(grepl('_', substr(col, nchar(col)-1, nchar(col)))) ||(substr(col, nchar(col)-1, nchar(col)) == 'ty')){
    cols <- c(cols, col)
  }
}

#X_temp <- rbind(Xtrain, Xtest)
#X <- scale(X_temp)
#rm(X_temp)

X_temp<-rbind(Xtrain, Xtest)
X <- X_temp[,cols]

res.dpca = dpca(X, q = 60, Ndpc = dim(X)[2])
var.dpca = (1 - sum( (res.dpca$Xhat - X)**2 ) / sum(X**2))*100
cat("Variance explained by DPCA:\t",var.dpca,"%\n")
pc <- res.dpca$scores

num_train <- as.integer((ratio/(ratio+1))*dim(X)[1])
Xtrain1 <- pc[0:num_train,]
Xtest1 <- pc[(num_train+1):dim(X)[1],]

# Train diagnostics
dir.create(here::here(name))

temp <- testvals$stopvalid[1:(num_train - dim(Xtrain)[1])]
ytrain <- c(ytrain, temp)
testvals <- testvals[(num_train - dim(Xtrain)[1] +1) : dim(Xtest)[1],]

save(Xtrain1, ytrain, file = here::here(name, 'traindpca.RData'))
save(Xtest1, testvals, file = here::here(name, 'testdpca.RData'))
save(res.dpca, file = here::here(name, paste0("dpca", Num_dpca,".RData")))

rm(X)
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
