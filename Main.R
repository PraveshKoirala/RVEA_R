##################################################################################################
##  The source code of the reference vector guided evolutionary algorithm (RVEA) in R
##
##  See the details of RVEA in the following paper:
##
##  R. Cheng, Y. Jin, M. Olhofer and B. Sendhoff,
##  A Reference Vector Guided Evolutionary Algorithm for Many-objective Optimization,
##  IEEE Transactions on Evolutionary Computation, 2016
##
##  The source code of RVEA is implemented by Ran Cheng, this is just its port to R.
##
##  View the original code at
##  https://github.com/ranchengcn/RVEA_Matlab
##################################################################################################

source("utils.R")
sourceall("Public")
sourceall("RVEA")
# 
# Main <- function (){
#   RunNum <- 1
#   
#   #Problems = c('DTLZ1','DTLZ2','DTLZ3','DTLZ4');
#   #Problems = c('SDTLZ1','SDTLZ2','SDTLZ3','SDTLZ4');
#   Problems <- c('DTLZ2')
#   
#   for (Prob in 1:length(Problems)){
#       for (Objectives in c(3)){
#           for (Run in 1:RunNum){
#               Algorithm <- c('RVEA')
#               Problem <- Problems[Prob]
#               Start (Algorithm, Problem, as.matrix(Objectives), Run)
#           }
#       }
#   }
# }

Main <- function(){
  
  # Number of objectives
  M <- 3
  # define objective function DTLZ2 in this case.
  dtlz2 <- function(x){
    # x is a row vector
    # change as necessary
    
    FunctionValue <- zeros(1,M)
    g <- sum((Population[,M:size(x, 2)]-0.5)^2)
    for (i in 1 : M){
      if ((M-i) == 0) 
        FunctionValue[,i] <- (1+g)
      else
        FunctionValue[,i] <- (1+g)*prod(cos(0.5*pi*x[,1:(M-i)]))
      if (i > 1){
        FunctionValue[,i] <- FunctionValue[,i]*sin(0.5*pi*x[,M-i+1])
      }
    }
    FunctionValue
  }
  
  # reference vector
  p1 = 7
  p2 = 0
  lbound= R(0, 0,0)
  ubound = R(1,1,1)
  Generations <- 500
  N <- 120
  K <- 10
  
  output <- rvea(objective = dtlz2, M, K, N, p1, p2, lbound, ubound)
  
  
}