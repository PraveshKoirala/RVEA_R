# The selection function in RVEA
#function [Selection] = F_select(FunctionValue, V, theta0, refV)
# Completed!
F_select <- function(FunctionValue, V, theta0, refV){

  # disable this entire segment
#   values <- readMat("function_value.mat")
#   FunctionValue <- values$FunctionValue
#   V <- values$V
#   theta0 <- values$theta0
#   refV <- values$refV
  
NM <- size(FunctionValue)
N <- NM[1]
M <- NM[2]
VN <- size(V, 1)

# only name is Zmin, but it depends upon the optimize_func.. can be min or max
Zmin <- Min(FunctionValue ,1)

#Translation
FunctionValue <- (FunctionValue - repmat(Zmin, R(size(FunctionValue,1), 1)) )

#Solutions associattion to reference vectors

div <- repmat(sqrt(Sum(FunctionValue^2,2)), R(1, M))
uFunctionValue <- FunctionValue / div

# Matrix multiplication
cosine <- uFunctionValue %*% t(V) #calculate the cosine values between each solution and each vector
acosine <- acos(cosine)
# call max with argument to give index too..
list[maxc, maxcidx] <- Max(cosine, 2, T)

# class <- data.frame(c = rep(NA, VN)) #classification
class <- as.list(rep(NA, VN))

for (i in 1:N){
    # empty at first
#     if (is.na(class[maxcidx[[i]], 'c'])){
#       class[maxcidx[[i]], 'c'] <- R(i)
#     }
#     else { # append
#       class[maxcidx[[i]], 'c'] <- R(class[maxcidx[[i]], 'c'], i)
#     }
    if (is.na(class[maxcidx[i]])){
      class[maxcidx[i]] <- i
    }
  else {
    class[[maxcidx[i]]] <- c(class[[maxcidx[i]]], i)
  }
}

Selection <- NULL
for (k in 1:VN){
    if (!is.na(class[k])){
        sub <- class[[k]]
        subFunctionValue <- FunctionValue[sub,]

        #APD calculation
        subacosine <- acosine[sub, k]
        subacosine <- subacosine/refV[k]# angle normalization
        D1 <- sqrt(Sum(subFunctionValue^2,2))# Euclidean distance from solution to the ideal point
        D <- D1*(1 + theta0[1]*(subacosine))# APD

        list[mind, mindidx] <- Min(D, 1, T)
        Selection <- C(Selection, sub[mindidx])
    }
}
	return(Selection)
}

