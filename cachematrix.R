## There are three sections in this program:
## Section 1 - Test matrices
## Section 2 - Global variable for cache result of inverse matrix and original matrix
## Section 3 - Function that calculate and returns the inverse matrix
## Section 4 - Function that decide if the result will be get from cache or will be calculated

## Section 1 ############################################################################
## Remove the comment signal from the desired test matrix
#a <- matrix(c(2), nrow = 1, ncol = 1) # 1x1 matrix
a <- matrix(c(2,1,5,3), nrow = 2, ncol = 2) # 2x2 matrix
#a <- matrix(c(2,1,2,1,1,3,1,1,2), nrow = 3, ncol = 3) # 3x3 matrix

## Section 2 ############################################################################
morigin <- matrix()
result <- matrix()

## Section 3 ############################################################################
## Function 1 - return the inverse matrix of the x
makeCacheMatrix <- function(x = matrix()) {
  result <<- solve(x)
  return(result)
}

## Section 4 ############################################################################
## Function 2 - Calculate the inverse matrix using makeCacheMatrix if the result no cached.
## If result is cached and the matrix original is equal to that passed as a parameter,
## then retrieve the result from cache.
cacheSolve <- function(x, ...) {
  if ( ((is.null(result))  ||   ((is.na(result[1,1])) & (ncol(result)*nrow(result)==1)))
    || ((morigin != x) || nrow(morigin)!=nrow(x) || ncol(morigin)!=ncol(x)) ) {
    result <- makeCacheMatrix(x)
    morigin <<- x
    result
  } else {
    message("getting cached data")
    result  
  }
}
