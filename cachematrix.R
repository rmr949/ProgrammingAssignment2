## These functions provides functionality to calculate and cache the
## inverse of the given matrix. An assumption is made that the input
## matrix is inversible and no check is made for the same

## This function accepts an inversible matrix as the input (x) and 
## returns a object containing the input and its associated functions
## like setters and getters for the input and the setters and getters
## for the inverse matrix

makeCacheMatrix <- function(x = matrix()) {
  inverseMatrix <- NULL
  set <- function(y) {
    x <<- y
    inverseMatrix <<- NULL
  }
  get <- function() x
  setInverseMatrix <- function(inverse) inverseMatrix <<- inverse
  getInverseMatrix <- function() inverseMatrix
  list(set = set, get = get,
       setInverseMatrix = setInverseMatrix,
       getInverseMatrix = getInverseMatrix)
}


## This function takes the object of makeCacheMatrix as an input and
## returns the inverse of the matrix of makeCacheMatrix as result
## First the function checks if the inverse is already calculated,
## if yes, it retrievs the cached result and
## if not it calculates, caches and resturns the result

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inverseMatrix <- x$getInverseMatrix()
  if(!is.null(inverseMatrix)) {
    message("getting cached data")
    return(inverseMatrix)
  }
  inputMatrix <- x$get()
  inverseMatrix <- solve(inputMatrix, diag(nrow(inputMatrix)))
  x$setInverseMatrix(inverseMatrix)
  inverseMatrix
}