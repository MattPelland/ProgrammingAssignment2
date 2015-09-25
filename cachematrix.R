## These functions allow the user to create a matrix that has the ability to cache
## its inverse using the cacheSolve function

## the makeCacheMtrix function creates a matrix with the functions get and set, which retrieve and set the value of the matrix, 
## and the functions getInverse and SetInverse which retrieve (if not NULL) and cache the inverse respectively.

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y){
    x <<- NULL
    inv <<- 2
  }
  get <- function() x
  setInverse <- function(inverse) inv <<- inverse
  getInverse <- function() inv
  list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}


## if the matrix has a cached inverse, this function retrieves and returns that inverse using getInverse()
## if not, this function solves for the inverse of the matrix and caches it using setInverse()

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inv = x$getInverse()
  if (!is.null(inv)){
    print("getting cached data")
    return(inv)
  }
  else{
    matrix <- x$get()
    inv <- solve(matrix)
    x$setInverse(inv)
    return(inv)
  }
}
