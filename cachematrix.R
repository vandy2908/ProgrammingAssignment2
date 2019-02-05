#example usage:
#> source("cachematrix.R")
#> m <- makeCacheMatrix(matrix(1:4, 2, 2))
#> m$getMatrix()
#> m$getCache() # will return NULL for the 1st time
#> cacheSolve(m)
#> m$getCache() # will return the solution
#--------------------------------
## Makes a cache matrix from a given matrix
## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {

  cacheMatrix <- NULL
  setMatrix <- function(y) 
  {
    x <<- y
    cacheMatrix <<- NULL
  }
  
  getMatrix <- function() x                                  #get the value of the Matrix
  
  setCacheInv <- function(inverse) cacheMatrix <<- inverse  #set the value of the invertible matrix
  
  getCacheInv <- function() cacheMatrix                     #get the value of the invertible matrix
  
  list(setMatrix = setMatrix,
       getMatrix = getMatrix,
       setCacheInv = setCacheInv,
       getCacheInv = getCacheInv)
}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above.
## If the inverse has already been calculated (and the matrix has not changed), then the cachesolve should retrieve the inverse from the cache.




cacheSolve <- function(x, ...) {

  cacheMatrix <- x$getCacheInv()  
  
   if(!is.null(cacheMatrix))                  #if the content is not null then return the cached matrix
  {
    message("getting cached data")
    return(cacheMatrix)
  }
  else
  {
    matrixData <- x$getMatrix()                #get the original Matrix Data 
    cacheMatrix <- solve(matrixData, ...)      #use solve function to inverse the matrix
    x$setCacheInv(cacheMatrix)
    return(cacheMatrix)
  }
}

