


## This function creates a matrix filled with random numbers.
## It also stores the inverse of that matrix. This function
## is called by the cachesolve() function

makeCacheMatrix <- function(x = matrix()) {
  
  inv = NULL
  set.seed(111)
  r = rnorm(1000000)
  mat0 = matrix(r, nrow=1000, ncol=1000)
  mat1 = solve(mat0)
  
  # Special Matrix
  get <- mat0
  # Cached Inverse Matrix
  set <- mat1
  
  # External functions
  setinv = function(mat1) inv <<- mat1
  getinv = function() inv      
  
  list(set=set, get=get, setinv=setinv, getinv=getinv)
}

x <- makeCacheMatrix()
str(x)

## This function can either obtain and use the cached inverse matrix
## in makeCacheMatrix() or directly solve the inverse within this 
## function. You can directly call the makeCacheMatrix() function.


cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  mdat = x$get
  inv = x$setinv(mdat)
  
  
  if (!is.null(inv)){ 
    # get it from the cache and skips the computation.
    
    message("getting cached inverse matrix")
    inv1 = inv
  } else{
    # otherwise calculate an Inverse
    
    message("computing an inverse matrix")
    inv1 = solve(mdat)
  }
  inv1
}

cacheSolve(makeCacheMatrix())
