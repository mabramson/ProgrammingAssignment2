## Mark Abramson 11/20/14 Programming Assignment #2 for Coursera R Programming Course
##
## This project will enable an efficient application of the expensive matrix inverse computation
## The project uses two functions, one to oversee creation of a matrix and one to access its inverse.
## When the inverse of the matrix is requested, a check is made whether the inverse has already been
## computed. The inverse is simply retrieved from cache storage if it exists. It is computed if it does
## not yet exist. The program uses the property of scoping rules in R to enable storage and access
## of the matrix inverse inside of an R object.

## makeCacheMatrix - creates a special "matrix", which will store key information internally
## and associated  functions to
## 1.  set the values of the matrix
## 2.  get the values of the matrix
## 3.  set the value of the matrix inverse
## 4.  get the value of the marix inverse
makeCacheMatrix <- function(x = matrix()) {
  m_inverse <- NULL
  set <- function(y) {
    x <<- y
    m_inverse <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) m_inverse <<- inverse
  getinverse <- function() m_inverse
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## This function returns the inverse of the input matrix.
## If the inverse has previously been computed, it returns
## the stored (cached) value. If no inverse has been computed
## the computation is made and stored for future requests.
## NOTE: This function assumes the matrix is invertible per 
## the project instructions. A more advanced version of this
## function would properly check for invertability and return
## feedback to the user gracefully if no inverse exists.
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  m_inverse <- x$getinverse()
  if(!is.null(m_inverse)) {
    message("getting cached data")
    return(m_inverse)
  }
  data <- x$get()
  
  ## Here's where an update would better check and handle a non-invertible matrix
  m_inverse <- solve(data, ...)
  x$setinverse(m_inverse)
  m_inverse
}
