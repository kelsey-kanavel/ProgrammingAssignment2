## Put comments here that give an overall description of what your
## functions do

## Function that creates a matrix object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
      inv <- NULL #inverse storage
      
      # can set a new matrix
      set <- function(y) { 
            x <<- y
            inv <<- NULL # removes previously stored inverse if new matrix present
      }
      
      # get the current matrix
      get <- function() {x 
      }
      
      # cache inverse
      set_inv <- function(inverse) {
            inv <<- inverse
      }
      
      # get cached inverse
      get_inv <- function() {
            inv
      }
      
      list(set = set, get = get, set_inv = set_inv, get_inv = get_inv)
}


## Function that computes the inverse of the special "matrix" returned by 
## makeCacheMatrix. If the inverse has already been calculated (and the matrix 
## has not changed), then cacheSolve should retrieve the inverse from the cache

cacheSolve <- function(x, ...) {
      inv <- x$get_inv()
      if(!is.null(inv)) {
            message("getting cached data")
            return(inv)
      }
      
      # get the matrix
      data <- x$get()
      
      # calculate inverse
      inv <- solve(data,...)
      
      # cache this inverse
      x$set_inv(inv)
      
      inv
}
