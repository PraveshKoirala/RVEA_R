# The selection function in RVEA
#function [Selection] = F_select(FunctionValue, V, theta0, refV)
# Completed!
F_select <- function(FunctionValue, V, theta0, refV){

NM <- size(FunctionValue)
N <- NM[1]
M <- NM[0]
VN <- size(V, 1)

# custom util function..
Zmin <- Min(FunctionValue ,1)

#Translation
FunctionValue <- (FunctionValue - repmat(Zmin, R(size(FunctionValue,1), 1)) )

#Solutions associattion to reference vectors

# is this important?
#### clear class

uFunctionValue <- FunctionValue / repmat(sqrt(Sum(FunctionValue^2,2)), R(1, M) )

# Matrix multiplication
cosine <- uFunctionValue %*% t(V) #calculate the cosine values between each solution and each vector
acosine <- acos(cosine)
# call max with argument to give index too..
list[maxc, maxcidx] <- Max(cosine, 2, T)
class <- data.frame(c = rep(NA, VN)) #classification
for (i in 1:N){
    # empty at first
    if (is.na(class[maxcidx(i), 'c']))
      class[maxcidx(i), 'c'] <- R(i)
    else # append
      class[maxcidx(i), 'c'] <- R(class[maxcidx(i), 'c'], i)
}

Selection <- NULL
for (k in 1:VN){
    if (!is.na(class[k, 'c'])){
        sub <- class(k).c
        subFunctionValue <- FunctionValue[sub,]

        #APD calculation
        subacosine <- acosine(sub, k)
        subacosine <- subacosine/refV(k)# angle normalization
        D1 <- sqrt(sum(subFunctionValue^2,2))# Euclidean distance from solution to the ideal point
        D <- D1*(1 + (theta0)%*%(subacosine))# APD

        list[mind, mindidx] <- Min(D, 0, T)
        Selection <- C(Selection, sub(mindidx))
    }
}
	return(Selection)
}

