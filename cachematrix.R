## Function that creates a representation of a cached matrix inverse. The following
## methods are available on the resulting object:
##  - get : Fetch the matrix
##  - set : Set the matrix
##  - getinverse : Get the cached result of the matrix inverse computation
##  - setinverse : Set the cached result of the matrix inverse computation

makeCacheMatrix <- function(matrix = matrix()) {
  cached_inverse <- NULL
  set <- function(new_matrix) {
    matrix <<- new_matrix
    cached_inv <<- NULL
  }
  get <- function() matrix
  setinverse <- function(new_inverse) cached_inverse <<- new_inverse
  getinverse <- function() cached_inverse
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## Computes the inverse of a matrix, using a cache. If the inverse was previously
## computed for the argument 'x', the cached solution is returned. Otherwise, the
## inverse is computed and the cache initialized

cacheSolve <- function(x, ...) {
  inverse <- x$getinverse() # Check if we can fetch the cached result
  if(!is.null(inverse)) {
    message("getting cached data")
    return(inverse)
  }
  matrix <- x$get() # Fetch underlying matrix to compute inverse for
  inverse <- solve(matrix) #Compute inverse
  x$setinverse(inverse) #Initialise cache
  inverse # Return result
}

## To test the code, you can execute the following function
test <- function() {
  m = matrix(rnorm(25), nrow = 5)
  cached_matrix = makeCacheMatrix(m)
  inverted <- cacheSolve(cached_matrix)
  all.equal(m %*% inverted, diag(5), tolerance = 1.0e-10)
}
