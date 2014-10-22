## makeCacheMatrix accepts a square matrix as input 
## and returns a "list of function names" that can
## used on the matrix

makeCacheMatrix <- function(A = matrix()) {

  ## Initially inverse is null  
  inv <- NULL
  
  ## Replace the cached matrix A by another matrix B
  set <- function(B) {
    ## Replace only if the matrices are not identical
    if(!identical(A,B)){ 
      A <<- B
      inv <<- NULL  ## Inverse is not yet calculated, therefore null
    }
  }
  ## Return the cached matrix
  get <- function() {
    return(A)
  }
  ## This function should only be used from within function 'cacheSolve'
  ## Using this function directly will corrupt the already cached inverse
  setinverse <- function(inverse) {
    inv <<- inverse
  }
  ## Return the cached inverse
  getinverse <- function() {
    return(inv)
  }
  
  list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}

## This function returns a matrix that is the inverse of 'A'
## If the inverse is already computed, it will be returned
cacheSolve <- function(A = matrix()) {
    inv <- A$getinverse()
    if(!is.null(inv)) {
      message("getting cached data of inverse")
      return(inv)
    }
  
  data <- A$get()
  ## Compute the inverse using R's function 'solve'
  inv <- solve(data)
  ## Cache the inverse
  A$setinverse(inv)
  return(inv)
}
