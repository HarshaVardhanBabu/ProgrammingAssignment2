#Author: Harsha Vardhan Babu
#Date : 20 Dec,2014


## Put comments here that give an overall description of what your
## functions do
# The following functions will try to mimic to cache the inverse of the matrix and there by
# saving computation for the future caculation

## Write a short comment describing this function
# this function will calculate the vector to set and get a matrix and to setinverse and getinverse of the matrix
makeCacheMatrix <- function(x = matrix()) 
{
  inversematrix <- NULL
  set <- function(y) 
  {
    x <<- y
    inversematrix <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) inversematrix <<- inverse
  getinverse <- function() inversematrix
  list(set = set, get = get,setinverse = setinverse,getinverse = getinverse)

}


## Write a short comment describing this function
#this function will calculate the inverse of the matrix if its not already calculated

#
cacheSolve <- function(x, ...) 
{
  Im <- x$getinverse()
  if(!is.null(Im)) {
    message("getting cached data")
    return(Im)
  }
  data <- x$get()
  Im <- solve(data,...)
  x$setinverse(Im)
  Im
        ## Return a matrix that is the inverse of 'x'
}
