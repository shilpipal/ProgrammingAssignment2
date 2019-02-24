  #take the matrix as an input local changes
makeCacheMatrix <- function(x = matrix()) {
  invMatrix <- NULL
  
  #set the value of the Matrix
  setMatrix <- function(y) {
    x <<- y
    invMatrix <<- NULL
  }
  
  getMatrix <- function() x                             
  setInverse <- function(inverse) invMatrix <<- inverse 
  getInverse <- function() invMatrix                    
  list(setMatrix = setMatrix, 
       getMatrix = getMatrix, 
       setInverse = setInverse, 
       getInverse = getInverse)
}	


cacheSolve <- function(x, ...) {	cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'	

  invMatrix <- x$getInverse()
  if(!is.null(invMatrix)) {
    message("Getting Invertible Matrix From Cached")
    return(invMatrix)
  }
  

  MatrixData <- x$getMatrix()                     #get the original Matrix Data 
  invMatrix <- solve(MatrixData, ...)             #use solve function to inverse the matrix
  x$setInverse(invMatrix)                         #set the invertible matrix 
  return(invMatrix)                               #return the invertible matrix
}	}
