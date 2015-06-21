## torresmr5042 Jun 21 2015
## makeCacheMatrix basically not modified from the baseline provided expect
## for the input parameter x type definition of matrix()
## 

## makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse.  

makeCacheMatrix <- function(x = matrix()) {

        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setinverse <- function(minverse) m <<- minverse
        getinverse <- function() m
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)

}




## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated and the matrix has not changed, then the cachesolve retrieve the inverse from the cache.
## Also slightly modified from the baseline provided only including the command solve to do the matrix inverse

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'


	  ## This function compare if 2 matrices are identical
	  ## taken from Rui Barradas
	  ## https://stat.ethz.ch/pipermail/r-help/2012-June/315408.html

	  matequal <- function(w, z) {
		is.matrix(w) && is.matrix(z) && dim(w) == dim(z) && all(w == z)
	  }

	  m <- x$getinverse()
        data <- x$get()
        if(!is.null(m)) {

	          datavalidation <- solve(m, ...)

		    if (matequal(datavalidation, data)) {
				message("getting cached data")
	                  return(m)
		    }
        }
 
        m <- solve(data, ...)
        x$setinverse(m)
        m

}
