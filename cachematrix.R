## need two functions 
## 1. makeCacheMatrix : set and get the value of the matrix and the inverse
## 2. cacheSolve : return a matrix that is the inverse of 'x'


makeCacheMatrix <- function(x = matrix()) {
  invs <- NULL
  set <- function(y) {
    x <<- y
    invs <<- NULL
  }
  get <- function() x
  setinvs <- function(inverse) invs <<- inverse
  getinvs <- function() invs
  list(set = set, get = get, setinvs = setinvs, getinvs = getinvs)

}

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  invs <- x$getinvs()
  if(!is.null(invs)) {
    message("getting cached result")
    return(invs)
  }
  data <- x$get()
  invs <- solve(data, ...)
  x$setinvs(invs)
  invs
}
