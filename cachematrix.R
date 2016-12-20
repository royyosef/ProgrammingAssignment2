
makeCacheMatrix <- function(x = matrix()) {
  inverseMatrix <- NULL
  get <- function() {
    x
  }
  set <- function(newMatrix) {
    x <<- newMatrix
    inverseMatrix <<- NULL
  }
  getInverse <- function() {
    inverseMatrix
  }
  setInverse <- function(solvedMatrix) {
    inverseMatrix <<- solvedMatrix
  }
  list(get = get, set = set, getInverse = getInverse, setInverse = setInverse)
}



cacheSolve <- function(x, ...) {
  inverseMatrix <- x$getInverse()
  
  if(!is.null(inverseMatrix)) {
    message("getting cached Inverse Matrix data ....")
    return(inverseMatrix)
  }
  data <- x$get()
  inverseMatrix <- solve(data, ...)
  x$setInverse(inverseMatrix)
  return(inverseMatrix)
}
