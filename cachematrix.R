## https://class.coursera.org/rprog-030 Assignment 2.
## Functions to create a cached object containing
## a matrix and its inverse (CacheMatrix).

## makeCacheMatrix create a new CacheMatrix object.
# return list of methods:
# $set() update the cached matrix and clear inverse
# $get() return cached matrix
# $setinv(inv) store calculated inverse
# $getinv() return stored inverse if present (use
#   cacheSolve() function instead)
makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinv <- function(newinv) inv <<- newinv
  getinv <- function() inv
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}


## cacheSolve(x,...), return the inverse of the matrix stored in
## a CacheMatrix object. Calculate a new inverse if necessary,
## otherwise return stored value. Extra arguments are not passed to
## solve().
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inv <- x$getinv()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  matdata <- x$get()
  #inv <- solve(matdata, ...) # actually, let's not...
  inv <- solve(matdata)
  x$setinv(inv)
  # The reason for not passing extra arguments is they would get used
  # only the first time and ignored when the cached result is returned.
  # It's possible to write extra checks to recalculate if they change,
  # but it would require changing some of the function arguments to
  # allow storing the options list.
  inv
}
