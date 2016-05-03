sourceall <- function(directory){
  # Got this nifty piece of code from http://stackoverflow.com/questions/10291520/reading-all-scripts-and-data-files-from-multiple-folders
  if (dir.exists(directory)){
    file.sources = list.files(path=directory, pattern="*.R$", full.names = T, ignore.case = T)
    data.sources = list.files(path=directory, pattern="*.rda$", full.names = T, ignore.case = T)
    sapply(data.sources,load,.GlobalEnv)
    sapply(file.sources,source,.GlobalEnv)
  }
}

printf <- function(...) invisible(print(sprintf(...)))

rfind <- function(x) seq(along=x)[x != 0]

rand <- function(row, col){
  matrix(runif(row*col), row)
}

# see this at http://haky-functions.blogspot.com/2006/11/repmat-function-matlab.html
repmat <- function(X,m,n=1){
  if (is.matrix(m)){
    n <- m[1, 2]
    m <- m[1, 1]
  }
  
  if (!is.matrix(X)){
    X <- t(X)
  }
  ##R equivalent of repmat (matlab)
  mx = dim(X)[1]
  nx = dim(X)[2]
  if (is.null(nx)) nx <-1
  matrix(t(matrix(X,mx,nx*n)),mx*m,nx*n,byrow=T)
}

# This function is repeated quite a bit in the public codes..
find_last_alphabet_index <-function(Problem){
  problem_list <- strsplit(Problem, split='')[[1]]
  k <- rfind(grepl("[[:digit:]]", problem_list))
  k <- k[1] - 1
  k
}

find_first_string <- function(Problem){
  # return DTLZ for input DTLZ05
  k <- find_last_alphabet_index(Problem)
  substr(Problem, 1, k)
}

rowProd <- function(mat){
  if (!is.matrix(mat)){
    stop("Must be matrix")
  }
  as.matrix(apply(FUN = prod, mat, MARGIN = 1))
}

colProd <- function(mat){
  if (!is.matrix(mat)){
    stop("Must be matrix")
  }
  t(apply(FUN = prod, mat, MARGIN = 2))
}

Prod <- function(mat, axis){
  if (axis == 1){
    colProd(mat)
  }
  else if (axis == 2){
    rowProd(mat)
  }
  else
    stop ("This product not supported")
}

Sum <- function(mat, axis){
  if (!is.matrix(mat)){
    stop("Must be matrix")
  }
  if (axis == 2)
    as.matrix(apply(FUN = prod, mat, MARGIN = 1))
  else if (axis == 1)
    t(apply(FUN = prod, mat, MARGIN = 2))
  else
    stop("This sum not supported")
    
}

MinMax <- function(FUNCTION, WHICH_FUNCTION, mat, axis, which){
  if (!is.matrix(mat)){
    stop("Must be matrix")
  }
  if (axis == 2){
    value <- as.matrix(apply(FUN = FUNCTION, mat, MARGIN = 1))
    index <- as.matrix(apply(FUN = WHICH_FUNCTION, mat, MARGIN=1))
  }
  else if (axis == 1){
    value <- t(apply(FUN = FUNCTION, mat, MARGIN = 2))
    index <- t(apply(FUN = WHICH_FUNCTION, mat, MARGIN = 2))
  }
  else
    stop("This operation not supported")
  if (which)
    return (list(value, index))
  return(value)
}

Min <- function(mat, axis, which=F){
  return (MinMax(FUNCTION = min, WHICH_FUNCTION = which.min, mat = mat, axis= axis, which = which))
}

Max <- function(mat, axis, which=F){
  return (MinMax(FUNCTION = max, WHICH_FUNCTION = which.max, mat = mat, axis= axis, which = which))
}

zeros <- function(M, N=0){
  if (N == 0)
    N <- M
  return (matrix(0, M, N))
}

ones <- function(M, N= 0){
  if (N == 0)
    N <- M
  return (matrix(1, M, N))
}

size <- function(mat, axis = 0){
  if (!is.matrix(mat))
    mat = as.matrix(mat)
  if (axis){
    return (dim(mat)[axis])
  }
  return (t(dim(mat)))
}

# r stands for row vector
R <- function(...){
  return (t(c(...)))
}

C <- function(...){
  return (as.matrix(c(...)))
}

norm <- function(mat){
  # check for one dimension matrix only?
  sqrt(sum(mat^2))
}
# 
# perm = function(n, x) {
#   return(factorial(n) / factorial(n-x))
# }

nchoosek = function(n, x) {
  if (is.atomic(n))
    return(factorial(n) / (factorial(x) * factorial(n-x)))
  else {
    # n is a set.. R contains a function that emulates the behaviour of nchoosek
    return (t(combn(n, x)))
  }
}

# to return multiple values, we require this hack

list <- structure(NA,class="result")
"[<-.result" <- function(x,...,value) {
  args <- as.list(match.call())
  args <- args[-c(1:2,length(args))]
  length(value) <- length(args)
  for(i in seq(along=args)) {
    a <- args[[i]]
    if(!missing(a)) eval.parent(substitute(a <- v,list(a=a,v=value[[i]])))
  }
  x
}