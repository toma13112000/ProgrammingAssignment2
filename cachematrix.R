library(MASS)
makeCacheMatrix <- function (x - matrix()) {
  ## Here we create "matrix" object that caches its inverse.
  n <- NULL
  set <- function(y) {
    x <<- y
    n <<- NULL
  }
  get <- function() x
  setsolution <- function(inverse) n <<- inverse
  getsolution <- function() n
  ## Impement get/set methods to calculate inverse of the matrix created in makeCacheMatrix.
  list (
    set = set,
    get = get, 
    setsolution = setsolution,
    getsolution = getsolution)
}
## In case if inverse of the matrix has been found, then inverse will be reused from cache.
cacheSolve <- function(x, ...) {
  ## Here, inverse matrix of "x" is returned.
  n <- x$getsolution()
  if(!is.null(n)) {
    message("getting cached data")
    return(n)
  }
  m <- x$get()
  n <- solve(m, ...)
  x$setsolution(n)
  n
}
## Here is my example to check the correctness of program working
## I create a matrix with 3x3 dimensions (number of columns and rows are 3)
## In the end we get inverse of this matrix created above
x <- makeCacheMatrix(matrix(c(6, 1, 0, 3, 3, 7, 5, 2, 8), ncol=3, nrow = 3))
cacheSolve(x)
x$get()
x$getsolution() 
