# The selection function in RVEA
#function [Selection] = F_select(FunctionValue, V, theta0, refV)
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

[maxc maxcidx] <- max(cosine, [], 2)
class <- struct('c', cell(1,VN)) #classification
for (i in 1:N){
    class(maxcidx(i)).c <- [class(maxcidx(i)).c, i]
}

Selection <- []
for (k in 1:VN){
    if (!isempty(class(k).c)){
        sub <- class(k).c
        subFunctionValue <- FunctionValue(sub,:)

        #APD calculation
        subacosine <- acosine(sub, k)
        subacosine <- subacosine/refV(k)# angle normalization
        D1 <- sqrt(sum(subFunctionValue.^2,2))# Euclidean distance from solution to the ideal point
        D <- D1.*(1 + (theta0)*(subacosine))# APD

        [mind mindidx] <- min(D)
        Selection <- [Selection; sub(mindidx)]
    }
}

	return(Selection)
}

