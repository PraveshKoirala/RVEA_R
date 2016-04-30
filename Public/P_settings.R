P_settings <- function(Algorithm,Problem,M){
    List[Generations,N] <- set_problem(Problem,M)
    Parameter <- set_algorithm(Algorithm,Problem,M)
    varargout <- data.matrix(Parameter)
	return (list(Generations,N,varargout))
}

rfind <- function(x) seq(along=x)[x != 0]

set_problem <- function(Problem,M){
    # find last index that's not the digit.. probably can be optimized a lot.
    problem_list <- strsplit(Problem, split='')[[1]]
    k <- rfind(grepl("[[:digit:]]", problem_list))
    k <- k[length(k)]
    
    D <- as.numeric(Problem[k+1:length(Problem)])
    Problem <- Problem[1:k]
    if (Problem == "DTLZ" || Problem == "SDTLZ"){
        if (M < 2 || M > 10){
            error('Objective Number Not Supported !')
        }
        if (D < 1 || D > 7){
            stop(Problem,' Not Exist')
        }
        Generations <- c(1000, 500, 1000, 500)
        Generations <- Generations[D]
    }

    else {
        stop(Problem,' Does Not Exist')
      }
    
    N <- c(50, 105, 120, 126, 132, 112, 156, 90, 275)
    N <- N * (M-1)
	  return(list(Generations,N))
}

set_algorithm <- function(Algorithm,Problem,M){
    Parameter <- c(NaN, NaN)
    
    # find last index that's not the digit.. probably can be optimized a lot.
    problem_list <- strsplit(Problem, split='')[[1]]
    k <- rfind(grepl("[[:digit:]]", problem_list))
    k <- k[length(k)]
    
    D <- as.numeric(Problem[k+1:length(Problem)])
    Problem <- Problem[1:k]
    if (Algorithm == 'RVEA'){
            p1 <- c(49, 13,  7,  5,  4,  3,  3,  2,  3)
            p2 <- c(0,  0,  0,  0,  1,  2,  2,  2,  2)
            p1 <- p1[M-1]
            p2 <- p2[M-1]
                     
            Parameter[1] <- p1; Parameter[2] <- p2
    }
    else{
            stop(Algorithm, ' Does Not Exist')
    }
    return (Parameter)
}

