sourceall <- function(directory){
  # Got this nifty piece of code from http://stackoverflow.com/questions/10291520/reading-all-scripts-and-data-files-from-multiple-folders
  if (dir.exists(directory)){
    file.sources = list.files(path=directory, pattern="*.R$", full.names = T, ignore.case = T)
    data.sources = list.files(path=directory, pattern="*.rda$", full.names = T, ignore.case = T)
    sapply(data.sources,load,.GlobalEnv)
    sapply(file.sources,source,.GlobalEnv)
  }
}

rand <- function(row, col){
  matrix(runif(row*col), row)
}

# see this at http://haky-functions.blogspot.com/2006/11/repmat-function-matlab.html
repmat <- function(X,m,n=1){
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

rowProd <- function(mat, dimension){
  if (!is.matrix(mat)){
    mat <- as.matrix(mat)
  }
  apply(FUN = prod, mat, MARGIN = 1)
}

colProd <- function(mat, dimension){
  if (!is.matrix(mat)){
    mat <- as.matrix(mat)
  }
  t(apply(FUN = prod, mat, MARGIN = 2))
}
