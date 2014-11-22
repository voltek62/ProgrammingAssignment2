# makeCacheMatrix
# 1. set the value
# 2. get the value
# 3. set the value of inverse
# 4. get the value of inverse
makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinv <- function(i) inv <<- i
  getinv <- function() inv
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}


cacheSolve <- function(x, ...) {   # the input x is an object created by makeCacheMatrix
  inv <- x$getinv()               # accesses the object 'x' and gets the value of the matrix
  if(!is.null(inv)) {              # if inv was already cached (not NULL) ...
    
    message("getting cached data")  # ... send this message to the console
    return(inv)                       # ... and return the mean ... "return" ends 
    #   the function cacheinv(), note
  }
  data <- x$get()        # we reach this code only if x$getinv() returned NULL
  inv <- solve(data, ...)   # if inv was NULL then we have to calculate the inv
  x$setinv(inv)           # store the calculated inv value in x (see setinv() in makeCacheMatrix
  inv               # return the inv to the code that called this function
}