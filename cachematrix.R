## The purpose of these functions is to cache the inverse of a square matrix.  This will allow the inverse of the
## matrix to be 'saved' such that it can be called if it is required again rather than re-computing every time.
## For very large matricies this could save a significant amount of computation time.

## This function makeCacheMatrix creates a special "matrix" which is really a list containing a set of functions to:
##      1.) set the value of the square matrix
##      2.) get the value of the square matrix
##      3.) set the value of the inverse of the matrix
##      4.) get the value of the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {

  inv <- NULL
  
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  
  get <- function() x 
  setinv <- function(solve) inv <<- solve
  getinv <- function() inv
  list(set = set, 
       get = get,
       setinv = setinv,
       getinv = getinv)
}


## The function cacheSolve computes the inverse of the special "matrix" returned by the makeCacheMatrix function above.
## If the inverse of that matrix has already been calculated and the matrix has not changed then the function will
## return the inverse.  If the inverse has not been calculated before then the function will compute it and save it
## to the cache so that it can be called again without re-computing it.

cacheSolve <- function(x, ...) {

  ## Return a matrix that is the inverse of 'x'
  inv <- x$getinv()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setinv(inv)
  inv

}

######### TESTING THE FUNCTION - The following code lines test the functions above on sample matrices ##########

# outside of the functions just test creating and inverting a square matrix
TestMat1 <- matrix(c(4,6,10,3,6,9,10,12,14), nrow = 3, ncol = 3)
TestMat1
solve(TestMat1)

x <- matrix(c(4,6,10,3,6,9,10,12,14), nrow = 3, ncol = 3)
TestMat <- makeCacheMatrix(x) # set up the 'special' matrix
cacheSolve(TestMat) # get the inverse of the 'special' matrix for the first time (will require computing the matrix)
cacheSolve(TestMat) # get the inverse of the 'special' matrix for the second time (should be pulling from cache)

# NOTE:  The functions did pull from the cache will calculating the inverse for the second time.  The inverse
#        of the matrix was also the same as the simple test calculating the inverse outside of the functions

#####################################################################################################################
