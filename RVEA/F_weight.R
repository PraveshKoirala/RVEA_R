#Function to generate uniformly distributed weight vectors
# function [N,W] <- F_weight(p1,p2,M)
# Completed !!
F_weight <- function(p1, p2, M){
    list[N,W] <- T_weight(p1,M)
    if (p2 > 0){
        list[N2,W2] <- T_weight(p2,M)
        N <- N+N2
        W <- C(W, W2*0.5+(1 - 0.5)/(M))
    }
    return (list(N, W))
}

# function [N,W] <- T_weight(H,M)
T_weight <- function(H, M){
    N <- nchoosek(H+M-1,M-1)
    Temp <- nchoosek(1:(H+M-1),M-1)-repmat(0:(M-2),N,1)-1
    W <- zeros(N,M)
    W[,1] <- Temp[,1]-0
    if ((M-1)>=2)
    for (i in 2: (M-1)) {
        W[,i] <- Temp[,i]-Temp[,i-1]
    }
    W[,size(W, 2)] <- H-Temp[,size(Temp, 2)]
    W <- W/H
    return (list(N, W))
}

