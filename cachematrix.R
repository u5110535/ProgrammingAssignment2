## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
  # cached inverse of the matrix
  inv <- NULL
  
  ## getter/setter for matrix
  
  set <- function(y) {
    x <<- y ## assign the input matrix y to the variable x in the
    
    inv <<- NULL 
  }
  get <- function() x ## return the matrix x
  
  ## getter/setter for matrix inverse
  getinv <- function() inv
  setinv <- function(inverse) inv <<- inverse
  ## return list of functions for matrix
  list(get=get, set=set, getinv=getinv, setinv=setinv)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
  inv <- x$getinv()
  
        ## Return a matrix that is the inverse of 'x'
  if (!is.null(inv)) {
    message("inverse is cached")
    return(inv)
  }
  # compute inverse of the matrix 
  m <- x$get()
  inv <- solve(m, ...)
  
  # cache inverse
  x$setinv(inv)
  
  # return inverse of matrix
  return(inv)
}
