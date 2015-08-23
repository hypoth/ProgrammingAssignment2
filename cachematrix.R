## Create matrix, inverse of which can be cached and routine to calcuate inverse.
## Write a short comment describing this function
## makeCacheMatrix - function generates matrix for which inverse can be cached.

makeCacheMatrix <- function(x = matrix()) {
  mInverse <- NULL
  set <- function(y) {
    x <<- y
    mInverse <<- NULL
  }
  get <- function() x
  setmInverse <- function(invrs)  mInverse <<- invrs
  getmInverse <- function() mInverse
  list(set = set, get = get,
       setmInverse = setmInverse,
       getmInverse = getmInverse)
}


## the function calculates matrix inverse fot the first time and afterwards returns
#cached results for matrix created by makeCacheMatrix
cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  #get the matrix inverse object
  mInverse <- x$getmInverse()
  if(!is.null(mInverse)) {
    message("getting cached data")
    #return cached if already present
    return(mInverse)
  }
  #if object not present create new one.
  data <- x$get()
  #calculate inverse
  mInverse <- solve(data)
  x$setmInverse(mInverse)
  #return inverse
  mInverse
}

#test cases
#set.seed(123)
#nmbrs <- rnorm(9)
#mat1 <- matrix(nmbrs,nrow=3,ncol=3)
#print(mat1)
#cachedMat1 <- makeCacheMatrix(mat1)
#print(cacheSolve(cachedMat1))
#print(cacheSolve(cachedMat1))

