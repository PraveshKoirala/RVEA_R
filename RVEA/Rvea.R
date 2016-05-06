###
# Arguments
# objective: objective function.. returns a row vector.. 

P_evaluate <- function(objective, Population){
    results<-apply(Population, FUN=function(x){objective(x)}, MARGIN=1)
    results <- t(results)
    results
}

rvea <- function(objective, Generations, M, K, N, p1, p2, lbound, ubound, 
                 alpha=2, fr=0.1, FE=0){
  
  
  Evaluations <- Generations*N  # max number of fitness evaluations
  
  #reference vector initialization
  list[N,Vs] <- F_weight(p1,p2,M)
  
  
  for (i in 1:N){
    Vs[i,] <- Vs[i,]/norm(Vs[i,])
  }
  V <- Vs
  
  
  Generations <- floor(Evaluations/N)
  
  #calculat neighboring angle for angle normalization
  cosineVV <- V %*% t(V)
  list[scosineVV, neighbor] <- Sort_descend(cosineVV)
  acosVV <- acos(scosineVV[,2])
  refV <- (acosVV)
  
  # initialize population
  lbound <- R(lbound); ubound <- R(ubound)
  Boundary <- C(ubound, lbound)
  Coding <- "Real"
  D <- M+K-1
  Population <- rand(N,D)
  Population <- Population*repmat(ubound,N,1)+(1-Population)*repmat(lbound,N,1)
  FunctionValue <- P_evaluate(objective, Population)
  
  all_population <- Population
  all_functionvalues <- FunctionValue
  
  for (Gene in 0 : (Generations - 1) ){
    #random mating and reproduction
    MatingPool <- F_mating(Population)
    
    Offspring <- P_generator(MatingPool,Boundary,Coding,N);  
    
    FE <- FE + size(Offspring, 1)
    
    Population <- C(Population, Offspring)
    val <- P_evaluate(objective, Offspring)
    FunctionValue <- C(FunctionValue, val)
    
    #APD based selection
    theta0 <-  (Gene/(Generations))^alpha*(M)
    Selection <- F_select(FunctionValue,V, theta0, refV)
    Population <- Population[Selection,]
    FunctionValue <- FunctionValue[Selection,]
    
    all_population <- C(all_population, Population)
    all_functionvalues <- C(all_functionvalues, FunctionValue)
    
    #reference vector adaption
    if (Gene %% ceiling(Generations*fr) == 0){
      #update the reference vectors
      Zmin <- Min(FunctionValue,1)
      Zmax <- Max(FunctionValue,1)
      V <- Vs
      V <- V*repmat((Zmax - Zmin)*1.0,N,1)
      for (i in 1:N){
        V[i,] <- V[i,] / norm(V[i,])
      }
      #update the neighborning angle value for angle normalization
      cosineVV <- V%*%t(V)
      list[scosineVV, neighbor] <- Sort_descend(cosineVV)
      acosVV <- acos(scosineVV[,2])
      refV <- (acosVV)
    }
    
    # printf('Progress %4s',as.character(Gene/Generations*100));
    
  }
  
  FunctionValue <- P_evaluate(objective, Population)
  list[FrontValue,] <- P_sort(FunctionValue, 'first')
  NonDominated  <- (FrontValue == 1)
  
  Population    <- Population[NonDominated,]
  FunctionValue <- FunctionValue[NonDominated,]
  return (list(population=Population, functionvalue=FunctionValue, 
               num_solutions=length(NonDominated), 
               all_population=all_population, all_functionvalues=all_functionvalues) )
  
}