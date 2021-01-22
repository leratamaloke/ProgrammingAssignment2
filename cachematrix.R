## The first function creates a special matrix object that can cache its inverse,
## and the second one computes the inverse of the special matrix returned by the previous function.
## If the inverse has already been calculated then it will retrieve the inverse from the cache.

## The below function creates a special matrix object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
      inv = NULL
      set = function(y) {
            x <<- y
            inv <<- NULL
      }
      get = function() x
      setinv <- function(inverse) inv <<- inverse
      getinv <- function() inv
      list(set = set, get = get, setinv = setinv, getinv = getinv)
}


## This function computes the inverse of the special matrix returned by the previous function,
## but if the inverse has already been computed then it will retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        inv = x$getinv()
        if(!is.null(inv)) {
                message("getting cache data")
                return(inv)
        }
        mat.data = x$get()
        inv = solve(mat.data, ...)
        x$setinv(inv)
        return(inv)
}

