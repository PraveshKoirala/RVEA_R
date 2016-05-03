#return randomly mathced mating pool
# function [MatingPool] <- F_mating(Population)
# Completed !!
F_mating <- function(Population){
    ND <- size(Population)
    N <- ND[1]
    D <- ND[2]
    MatingPool <- zeros(N,D)
    RandList <- R(sample(N))
    MatingPool <- Population[RandList, ]
    if (N %% 2 == 1){
        MatingPool <- C(MatingPool, MatingPool[1,])
    }
    return (MatingPool)
}
