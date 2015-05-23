## Put comments here that give an overalnl description of what your
## functions do

## Write a short comment describing this function
# makeCacheMatrix: return a list of functions to:
# 1. Set the value of the matrix
# 2. Get the value of the matrix
# 3. Set the value of the inverse
# 4. Get the value of the inverse
makeCacheMatrix <- function(x = matrix()) {
  # inv will store cached inverse matrix
  inv <- NULL
  
  # Set the matrix
  set <- function(y){
    x <- y
    inv <<- NULL
  }
  # Get the matrix
  get <- function() x
  
  # Setter for the inverse
  setinv <- function(inverse) inv <<- inverse
  
  # Getter for the inverse
  getinv <- function() inv
  
  # Return the matrix with our newly defined functions
  list(set = set, 
       get = get,
       setinv = setinv,
       getinv = getinv)
}


## Write a short comment describing this function
# cacheSolve: Compute the inverse of the matrix. If the inverse is already
# calculated before, it returns the cached inverse.
cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inv <- x$getinv()

  # If the inverse is already calculated, return the value from cache
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
    data <- x$get()
  
    # Calculate inverse if not already calculated
    inv <- solve(data,...)
    
    # Cache the inverse of the matrix
    x$setinv(inv)
    
    # Return inverse of the matrix
    inv
}
