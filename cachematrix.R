## Data Science Specialization - Coursera
## Course: R Programming
## Programming Assignment 2

## This function gets and sets the values of a matrix and it's inverse matrix

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  # Function to set the original matrix
  set <- function(y) {
    # When setting or changing the matrix, make the inverse NULL to clear the inverse matrix cache
    x <<- y
    i <<- NULL
  }
  # Function to get and print the original matrix
  get <- function() x
  # Function to set the inverse of the original matrix in the cache
  setinverse <- function(inverse) i <<- inverse
  # Function to get the inverse of the original matrix from the cache
  getinverse <- function() i
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## This function calculates the inverse of a matrix, using it's cache when available

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  # First get the inverse matrix from the cache
  i <- x$getinverse()
  # Test if it the cached inverse matrix is NULL
  if(!is.null(i)) {
    # If it is not NULL, it means it was previously calculated and is cached, so return it
    message("getting cached data")
    return(i)
  }
  # If it is NULL, the matrix is new or may have changed since the inverse was last calculated
  # So get the original matrix
  data <- x$get()
  # Calculate the inverse matrix
  i <- solve(data, ...)
  # Store the inverse matrix in the cache
  x$setinverse(i)
  # And finally return the calculated inverse matrix
  return(i)  
}
