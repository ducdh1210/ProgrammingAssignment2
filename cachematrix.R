## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
# makeCacheMatrix acts like a "constructor" to create a matrix "object"
# INPUT: a matrix
# OUTPUT: an matrix "object" which has
#           "private" variables, which are matrices: x, inv_x
#           "methods": set --> set x, get --> get x, setinverse --> set inv_x, getinverse --> get inv_x
# Note: the actual output is a list whose elements are those above functions
makeCacheMatrix <- function(x = matrix()) {
  inv_x <- NULL
  set <- function(y) {
    x <<- y
    inv_x <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) inv_x <<- inverse
  getinverse <- function() inv_x
  list(setMatrix = set, getMatrix = get, setInverseMatrix = setinverse, getInverseMatrix = getinverse)
}


## Write a short comment describing this function
# INPUT: matrix object
# OUTPUT: matrix which is the inverse of the matrix object's matrix

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inverseMatrix <- x$getInverseMatrix()
  
  # if the inverse matrix is already computed, then return it 
  if (!is.null(inverseMatrix)) {
    return(inverseMatrix)
  }
  
  # else if the inverse is not yet computed, then compute it and then set 
  else{
    # use "solve" to compute the inverse
    inverseMatrix <- solve(x$getMatrix()) 
    # set newly found inverseMatrix to the inverse matrix variables of the passed matrix object
    # next time cacheSolve is called, a cached inverse matrix will be returned instead of needing to be recomputed
    x$setInverseMatrix(inverseMatrix)
   
    return(inverseMatrix)
  }
}

# Test
test_matrix = rbind(c(1,3),c(5,6))
matrix_obj = makeCacheMatrix(test_matrix)
cacheSolve(matrix_obj)