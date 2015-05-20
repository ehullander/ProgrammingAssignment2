##makeCacheMatrix creates a list of functions stored with values super assigned
##accepts argument x which must be an invertible matrix 'x'
##Let blob<-makeCacheMatrix(x)
makeCacheMatrix <- function(x = numeric()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinv <- function(inverse) inv <<- inverse
  getinv <- function() inv
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}

##cacheSolve accepts the list of functions created by makeCacheMatrix  
##it looks to see if the inverse has already been cached, if so it returns
##the stored value, if not it calculates and pushes it into the list created by the first function
cacheSolve <- function(x, ...) {
  inv <- x$getinv()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data)
  x$setinv(inv)
  inv
}