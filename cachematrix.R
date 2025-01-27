makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL  # Initialize variable to store cached inverse
  
  set <- function(y) {
    x <<- y      # Assign new matrix to x
    inv <<- NULL # Clear cached inverse
  }
  
  get <- function() x  # Return the matrix
  
  setinverse <- function(inverse) inv <<- inverse  # Store the inverse in cache
  
  getinverse <- function() inv  # Retrieve the cached inverse
  
  list(set = set, get = get, 
       setinverse = setinverse, 
       getinverse = getinverse)
}

cacheSolve <- function(x, ...) {
  inv <- x$getinverse()  # Check if inverse is already cached
  
  if(!is.null(inv)) {
    message("getting cached inverse")
    return(inv)  # Return cached inverse
  }
  
  data <- x$get()  # Get the matrix
  
  inv <- solve(data, ...)  # Compute the inverse
  
  x$setinverse(inv)  # Store inverse in cache
  
  inv  # Return the inverse
}

