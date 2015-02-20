## Matrix inversion is usually a costly computation. As such this pair of functions is used to create a special
## object that stores a numeric matrix and caches its inverse so as to avoid having to compute it repeatedly.

## makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = numeric()) {
      inv <- NULL  ## cached inverse is set to NULL when new matrix is created
      
      ## set sub-function that can be used to modify contents of matrix. As should be, cached inverse is reset to NULL
      set <- function(y) {          
            x <<- as.matrix(y)
            inv <<- NULL
      }
      
      ## get sub-function returns matrix 
      get <- function() x           
      
      ## setinv sub-function is used by cacheSolve to cache the inverse after it has been computed
      setinv <- function(inverse) inv <<- inverse 
      
      ## getinv sub-function returns the cached inverse
      getinv <- function() inv
      
      ## structure of object created is a list 
      list(set = set, get = get,
           setinv = setinv,
           getinv = getinv)
}

## cacheSolve: This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), then the function will 
## retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
      ## function first checks if cached value of inverse exists and returns cached value with message if it does
      inv <- x$getinv()  
      if(!is.null(inv)) {
            message("getting cached data")
            return(inv)
      }
      
      ## if cached inverse does not exist, function uses the solve function in R to compute inverse.
      data <- x$get()
      inverse <- solve(data, ...)
      
      ## inverse computed is cached for future use and function returns computed inverse.
      x$setinv(inverse) 
      inverse
}