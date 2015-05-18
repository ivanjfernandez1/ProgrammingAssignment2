makeCacheMatrix <- function(x = matrix()) {
  
  matrixinverse <- NULL                            ## set the matrix

  set <- function(y) {
    x <<- y
    matrixinverse <<- NULL 
}
  get <- function() x                              ## get the matrix
  setInv <- function(inv) matrixinverse <<- inv    ## set the inverse of the matrix
  getInv <- function() matrixinverse               ## return the inverse of the matrix
   list(set = set, get = get,
       setInv = setInv,
       getInv = getInv)
}


##########################################################Part 2################################

cacheSolve <- function(x, ...) {
  matrixinverse <- x$getInv() # get the inversed matrix from object x
  
  if(!is.null(m)) { # if inverse exists (!null = not null){
    message("Getting Cached Data")
    return(matrixinverse) # return inverted matrix
  }
  
  data <- x$get() 
  matrixinverse <- solve(data) 
  x$setInv(matrixinverse) 
  matrixinverse          ## show result
  
}
