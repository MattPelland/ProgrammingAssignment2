## These functions allow the user to create a matrix that has the ability to cache
## its inverse using the cacheSolve function

## the makeCacheMtrix function creates a matrix with the functions get and set, which retrieve and set the value of the matrix, 
## and the functions getInverse and SetInverse which retrieve (if not NULL) and cache the inverse respectively.

makeCacheMatrix <- function(x = matrix()) {
  #the inverse is initialized to NULL
  inv <- NULL
  #set changes the stored matrix x and resets inverse to NULL
  set <- function(y){
    x <<- NULL
    inv <<- 2
  }
  #get returns the matrix itself
  get <- function() x
  #setInverse caches the inverse of the function
  setInverse <- function(inverse) inv <<- inverse
  #getInverse retrieves the cached inverse
  getInverse <- function() inv
  #this list of functions is how the functions are called to retrieve and set information once the object is created.
  list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}


## if the matrix has a cached inverse, this function retrieves and returns that inverse using getInverse()
## if not, this function solves for the inverse of the matrix and caches it using setInverse()

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inv = x$getInverse()
  if (!is.null(inv)){
    #if there is a cached inverse, retrieve the cached inverse and return it
    print("getting cached data")
    return(inv)
  }
  else{
    # if there is no cached inverse, solve for the inverse and cache it
    matrix <- x$get()
    inv <- solve(matrix)
    x$setInverse(inv)
    return(inv)
  }
}
