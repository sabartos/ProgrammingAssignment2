## Below are two functions that are used to create a special object that 
## stores a special vector and caches its inverse.
## 
## The first function, makeVector creates a special "vector", which is really a list containing a function to
##
##  1. set the value of the vector
##  2. get the value of the vector
##  3. set the value of the inverse
##  4. get the value of the inverse 
##
## It is assumes that the matrix supplied is always invertible


makeCacheMatrix <- function(mat = matrix()) {
  
  inv <- NULL
  
  setmat <- function (setmatrix)  {
      mat   <<- setmatrix
      inv <<- NULL
  }
  
  getmat <- function() mat
  
  setinv <- function(inverse) inv <<- inverse
  
  getinv <- function() inv
  
  list(setmat = setmat, 
       getmat = getmat, 
       setinv = setinv, 
       getinv = getinv)
}


## The following computes the inverse of the matrix returned by the function above ("makeCacheMatrix"),
## If the inverse has been calculated the following function ("cacheSolve") will retrive the inverse from
## the cache, and skip the calculation. If not, it will compute the inverse, 
## and set the value of the inverse in the cache.

cacheSolve <- function(mat, ...) {
 
  inv = mat$getinv()
  
  if( !is.null(inv) ) {
      message("getting cached data")
      return(inv)
  }

  outdata <- mat$getmat()

  inv <- solve(outdata)

  mat$setinv(inv)

  inv
}
