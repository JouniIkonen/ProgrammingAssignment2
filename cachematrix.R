## This file contains two functions to provide a way to cache the result of inverse matrix calculation
## i.e. second request to cacheSolve()-function will return a cached result
## This way we can avoid wasting resources to computationally heavy operations

## Example of usage
## C1 <- rbind(c(1,-1/4),c(-1/4,1))      # Create 2 by 2 matrix
## A <- makeCacheMatrix(C1)              # Create matrix object with mathods to manipulate the matrix
## cacheSolve(A)                         # Calculate and return an inverse matrix of C. If function has been called previously return 
                                         # the cached solution


## makeCacheMatrix creates an abject containing given matrix and methods (functions) for cache operations of the matrix 
makeCacheMatrix <- function(x = matrix()) {
  m <- NULL                             # variable will be emptied every call to makeCacheMatrix()
  
  set <- function(y) {                  # method for storing the given matrix to be available from other scopes (<<- assignemnt) 
    x <<- y                             # Store argument to be available from other scopes (<<- assignemnt )
    m <<- NULL                          # reset (set NULL) inverse matrix storage variable
  }
  
  get <- function() x                   # return the stored matrix
  
  setinverse <- function(inverse) m <<- inverse         # stores in cache the inverse matrix to be available later (called by cacheSolve())
  getinverse <- function() m                            # returns cached inverse matrix
  
  list(set = set, get = get,            # Accessed every time makeCacheMatrix() is called as new object is created and the object 
       setinverse = setinverse,         # has methods to interact with it.
       getinverse = getinverse)

}


## cacheSolve function returns inverse matrix which it stores in a cache memory or
## if there is not cached copy available, then inverse is calculated and cached for future use
## Usher has to store matrix first by calling makeCacheMatrix()
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getinverse()                     # Try to fetch cached inverse matrix
       
        if(!is.null(m)) {                       # if available then    
                message("getting cached data")
                return(m)                       # return cached matrix
        }
        data <- x$get()                         # get vector stored by makeCachedMatrix()- function
        m <- solve(data, ...)                   # calculate inverse matrix
        x$setinverse(m)                         # place a copy of inverse matrix to the cache
        m                                       # return calculated inverse matrix
}
