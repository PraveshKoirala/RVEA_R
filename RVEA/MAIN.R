#RVEA Main File
# function MAIN(Problem,M,Run)
MAIN <- function(Problem, M, Run){
  #clc;
  # format compact;tic){
  
  #basic settings
  list[Generations,N,p1,p2] <- P_settings('RVEA',Problem,M)
  Evaluations <- Generations*N  # max number of fitness evaluations
  alpha <- 2.0  # the parameter in APD, the bigger, the faster RVEA converges
  fr <- 0.1 # frequency to call reference vector
  FE <- 0 # fitness evaluation counter
  
  #reference vector initialization
  list[N,Vs] <- F_weight(p1,p2,M)
  for (i in 1:N){
      Vs[i,] <- Vs[i,]/norm(Vs[i,])
  }
  V <- Vs
  Generations <- floor(Evaluations/N)
  
  #calculat neighboring angle for angle normalization
  cosineVV <- V %*% t(V)
  list[scosineVV, neighbor] <- sort(cosineVV, 2, 'descend')
  acosVV <- acos(scosineVV[,2])
  refV <- (acosVV)
  
  #population initialization
  set.seed()
  # rand('seed', sum(100 * clock))
  
  list[Population,Boundary,Coding] <- P_objective('init',Problem,M,N)
  
  FunctionValue <- P_objective('value',Problem,M,Population)
  
  for (Gene in 0 : (Generations - 1) ){
      #random mating and reproduction
      MatingPool <- F_mating(Population)
      
      Offspring <- P_generator(MatingPool,Boundary,Coding,N);  
      FE <- FE + size(Offspring, 1)
      
      Population <- C(Population, Offspring)
      FunctionValue <- C(FunctionValue, P_objective('value',Problem,M,Offspring))
  
      #APD based selection
      theta0 <-  (Gene/(Generations))^alpha*(M)
      Selection <- F_select(FunctionValue,V, theta0, refV)
      Population <- Population[Selection,]
      FunctionValue <- FunctionValue[Selection,]
  
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
          cosineVV <- V*t(V)
          list[scosineVV, neighbor] <- sort(cosineVV, 2, 'descending')
          acosVV <- acos(scosineVV[,2])
          refV <- (acosVV)
      }
  
      printf('Progress#4s##\n',num2str(roundn(Gene/Generations*100,-1)));
  
  }
  P_output(Population,toc,'RVEA',Problem,M,Run)
}

