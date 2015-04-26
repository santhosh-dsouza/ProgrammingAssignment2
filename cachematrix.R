# makeCacheMatrix(): creates a special ???matrix??? object that can cache its inverse.
# cacheSolve(): computes the inverse of the ???matrix??? returned by makeCacheMatrix(). If the inverse 
# has been previously calculated and the matrix is unchanged, it retrieves the cached inverse

makeCacheMatrix <- function(x = matrix()) {
  
  xinv <- NULL # this is where the result of inversion is stored
  set <- function(y) {
    x <<- y
    xinv <<- NULL # it also initialises xinv to null
  }
  
  get <- function() x # return the input matrix
  setInv <- function(inv) xinv <<- inv # set the inverse matrix
  getInv <- function() xinv # return the inverse matrix
# create the list
  list(set = set, get = get,
       setInv = setInv,
       getInv = getInv)
}


cacheSolve <- function(x, ...) {
  m <- x$getInv() # get the inverse matrix from object x, null if not previously calculated
  if(!is.null(m)) { # if the inversion is cached
    message("getting cached data")
    return(m) # return the calculated inversion
  }
  data <- x$get() # if not, we get the matrix object
  m <- solve(data) # we solve it
  x$setInv(m) # we then set it to the object
  m # return the solved result
}

# Test
# generate a random square, non-singular matrix
test <- matrix(runif(9,1,100),3,3)
# generate the makeCacheMatrix object with this matrix
testCached <- makeCacheMatrix(test)
# from now on calculate or retrieve calculated inversion using the cacheSolve function

testInv <- cacheSolve(testCached)
testInv <- cacheSolve(testCached)
testInv <- cacheSolve(testCached)
testInv <- cacheSolve(testCached)
testInv <- cacheSolve(testCached)
