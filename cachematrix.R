## Put comments here that give an overall description of what your
## functions do

## short comment describing this function
## Matrix inversion is usually a costly computation and there may be some benefit to caching 
## the inverse of a matrix rather than computing it
## repeatedly (there are also alternatives to matrix inversion that we will not discuss here). 
## Your assignment is to write a pair of functions that cache the inverse of a matrix.

makeCacheMatrix <- function(x = matrix()) {
  m<-NULL # cached inversed matrix
  set <- function(y) {
      x <<- y
      m <<- NULL
  }
  get <- function() x
  setinverse <- function(inversedMatrix) m <<- inversedMatrix
  getinverse <- function() m
  list(set = set, get = get,
      setinverse = setinverse,
      getinverse = getinverse)
}


## cachesolve function checks the special cacheMatrix object matrix to see if its inverse is already set. 
## If so it returns the stored inverse, otherwise it computes it with solve() function and set it

cacheSolve <- function(x, ...) { ## Return a matrix that is the inverse of 'x'
  m <- x$getinverse()
  if(!is.null(m)) {  # if inverse is already set, return it from cache
    message("getting cached data")
    return(m)
  }
  data <- x$get()  # get the matrix
  m <- solve(data, ...)  # inverse it
  x$setinverse(m)  # set the cached inversed matrix in the cacheMatrix object
  m  # returns the inverse
}


## Example of use
## in this example we use a matrix that can be inversed
## we create a cachedMatrix object and initialize it and display it
## we check the inversed matrix is correct by using the %*% matrix product operator
## and obtain the identity matrix

# > m2=makeCacheMatrix(matrix(c(3,3.5,3.2,3.6),2,2))
# > m2
# $set
# function (y) 
# {
#   x <<- y
#   m <<- NULL
# }
# <environment: 0x000000000a16bbc8>
#   
#   $get
# function () 
#   x
# <environment: 0x000000000a16bbc8>
#   
#   $setinverse
# function (inversedMatrix) 
#   m <<- inversedMatrix
# <environment: 0x000000000a16bbc8>
#   
#   $getinverse
# function () 
#   m
# <environment: 0x000000000a16bbc8>
#   
#   > m2$get()
# [,1] [,2]
# [1,]  3.0  3.2
# [2,]  3.5  3.6
# > cacheSolve(m2)
# [,1] [,2]
# [1,] -9.00  8.0
# [2,]  8.75 -7.5
# > m2$get()%*%m2$getinverse()
# [,1] [,2]
# [1,]    1    0
# [2,]    0    1
# > message("Done")
# Done
## End of file