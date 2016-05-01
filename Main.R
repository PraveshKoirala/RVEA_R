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

Main <- function (){
  RunNum <- 1
  
  #Problems = c('DTLZ1','DTLZ2','DTLZ3','DTLZ4');
  #Problems = c('SDTLZ1','SDTLZ2','SDTLZ3','SDTLZ4');
  Problems <- c('DTLZ2')
  
  for (Prob in 1:length(Problems)){
      for (Objectives in c(3)){
          for (Run in 1:RunNum){
              Algorithm <- c('RVEA')
              Problem <- Problems[Prob]
              Start (Algorithm, Problem, data.matrix(Objectives), Run)
          }
      }
  }
}