## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## This function returns a list containing four functions: set, get,
## setmatrix and getmatrix


makeCacheMatrix <- function(x = matrix()) {
  i <- NULL 
  set <- function(y) { # This function sets the value of the matrix passed as a parameter and copy its value to global matrix "x"
    x <<- y
    i <<- NULL
  }
  get <- function() x # This function gets the value of the global matrix ("x")
  setmatrix <- function(solve) i <<- solve # This function inverts global matrix
  getmatrix <- function() i # This function gets the inverted matrix
  list(set = set, get = get, # This returns the list
       setmatrix = setmatrix,
       getmatrix = getmatrix)
}


## Write a short comment describing this function
## This function computes the inverse of the special "matrix" returned by makeCacheMatrix

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  i <- x$getmatrix() # Get i from x (object returned from makeCacheMatrix function)
  if(!is.null(i)) {  # If "i" is not null this means that it was calculated previously, then it is not necessary to calculate it again
    message("getting cached data")
    return(i)
  }
  matrix <- x$get() # If "i" is null, which means it never calculated before or new data is assigned to the object, get new data, and calculate mean
  i <- solve(matrix, ...)
  x$setmatrix(i) # Save inverted matrix to x
  i # Return "i" to the caller
}
