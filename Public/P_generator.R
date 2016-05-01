P_generator <- function(MatingPool,Boundary,Coding,MaxOffspring = 0){
# This function includes the SBX crossover operator and the polynomial
# mutatoion operator.

    dimension <- dim(MatingPool)
    N <- dimension[1]
    D <- dimension[2]
    if (MaxOffspring < 1 || MaxOffspring > N){
        MaxOffspring <- N
    }

    if (Coding == 'Real'){
        ProC <- 1
        ProM <- 1/D
        DisC <- 30
        DisM <- 20
        Offspring <- matrix(0, N, D)
        
        for (i in seq(1, N, 2) ) {
            beta <- rep(0, D) 
            miu  <- runif(D)
            beta(miu<=0.5) <- (2*miu(miu<=0.5))^(1/(DisC+1))
            beta(miu>0.5)  <- (2-2*miu(miu>0.5))^(-1/(DisC+1))
            beta <- beta * (-1)^round(runif(D, 0, 1))
            beta(runif(D)>ProC) <- 1
            Offspring[i,] <- (MatingPool[i,] +MatingPool[i+1,])/2 + beta * (MatingPool[i,]-MatingPool[i+1,])/2
            Offspring[i+1,] <- (MatingPool[i,]+MatingPool[i+1,])/2 - beta * (MatingPool[i,]-MatingPool[i+1,])/2
        }
        
        Offspring <- Offspring[1:MaxOffspring,]

        if (MaxOffspring == 1){
            MaxValue <- Boundary[1,]
            MinValue <- Boundary[2,]
        } else {
            # repmat defined at utils
            MaxValue = repmat(Boundary[1,], MaxOffspring,1);
            MinValue = repmat(Boundary[2,], MaxOffspring,1);
        }
        k    <- matrix(runif(MaxOffspring * D), MaxOffspring)
        miu  <- matrix(runif(MaxOffspring * D), MaxOffspring)
        Temp <- k<=ProM & miu<0.5
        
        Offspring[Temp] <- Offspring[Temp]+(MaxValue[Temp]-MinValue[Temp]) * ((2*miu[Temp]+(1-2*miu[Temp])*(1-(Offspring[Temp]-MinValue[Temp])/(MaxValue[Temp]-MinValue[Temp]))^(DisM+1))^(1/(DisM+1))-1)
        Temp <- (k<=ProM & miu>=0.5)
        Offspring[Temp] <- Offspring[Temp]+(MaxValue[Temp]-MinValue[Temp]) * (1-(2*(1-miu[Temp])+2*(miu[Temp]-0.5)*(1-(MaxValue[Temp]-Offspring[Temp])/(MaxValue[Temp]-MinValue[Temp]))^(DisM+1))^(1/(DisM+1)))

        Offspring[Offspring>MaxValue] <- MaxValue[Offspring>MaxValue]
        Offspring[Offspring<MinValue] <- MinValue[Offspring<MinValue]

    }
	return (Offspring)
}
