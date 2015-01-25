## Main aim of these two functions is to reduce the repititions in computing the inverse of a matrix. 
## The inverse of a new matrix is cached to avoid these repititions.

## Input for makeCacheMatrix is an invertible matrix. This functions creates a list containing a function to
## 1. set the value of the matrix
## 2. get the value of the matrix
## 3. set the value of the inverse
## 4. get the value of the inverse

makeCacheMatrix <- function(x = matrix()) {
	m <- NULL
         set <- function(y) {
            x <<- y
            m <<- NULL
         }
          get <- function() x
          setsolve <- function(solve) m <<- solve
          getsolve <- function() m
          list(set = set, get = get,
               setsolve = setsolve,
               getsolve = getsolve) 
}


## cacheSolve creates a new inverse for the matrix if it is not already cached else returns the cached inverse matrix.
## Input for cacheSolve would be the list created using the makeCacheMatrix function.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
	m <- x$getsolve()
        if(!is.null(m)) {
          message("getting cached data")
              return(m)
          }
          data <- x$get()
          m <- solve(data, ...)
          x$setsolve(m)
          m

}
