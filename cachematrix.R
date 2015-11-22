# Function Name: makeCacheMatrix
# Matrix inversion is usually a costly computation and there may be some benefit to caching the 
# inverse of a matrix rather than compute it repeatedly.

# This function creates a special "matrix" object that can cache its inverse
# setting the "Cinv" (cashedInverse) to NULL as a placeholder for the future value.

makeCacheMatrix <- function(x = matrix()) {
  cInv <- NULL
  # defines a function to set the vector "x" to anew vector.
  set <- function(y){
    x <<- y
    #resett the CINV to NULL again.
       cInv <<- NULL
  }
  #returns the vector x
  get <- function() x
  #set the caches inverse cInv to inverse
  setInverse <- function(inverse) cInv <<- inverse
  #return the inverse cInv
  getInverse <- function() cInv
  # return the inverse matrix containing all the functions defined.
  list(set = set, 
       get = get,
       setInverse = setInverse, 
       getInverse = getInverse)
  
}


# function Name: cacheSolve
# The following function returns the inverse of a matrix created by the function makeCacheMatrix.

# this function assumes that the matrix is invertible.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  Inv <- x$getInverse()
  
  if(!is.null(cInv)) {
    message("collecting the cached data")
    return(cInv)
  }
  
  data <- x$get()
  
  cInv <- solve(data, ...)
  
  x$setInverse(cInv)
  
  cInv
}
