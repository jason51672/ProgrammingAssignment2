## makeCacheMatrix: This function creates a special "matrix" object
## that can cache its inverse.
##
## makeCacheMatrix creates a special "vector", which is really a
## list containing a function to:
##   set the value of the matrix
##   get the value of the matrix
##   set the value of the matrix inverse
##   get the value of the matrix inverse


makeCacheMatrix <- function(x = matrix()) {
	inv <-NULL
	
	##   set the value of the matrix
	set <- function(y) {
		x <<- y
		inv <<- NULL
	}
	
	##   get the value of the matrix
	get <- function() x
	
	##   set the value of the matrix inverse
	setInverse <- function(inverse) inv <<- inverse
	
	##   get the value of the matrix inverse
	getInverse <- function() inv
	
	list(set = set, get = get,
		 setInverse = setInverse,
		 getInverse = getInverse)
}


## cacheSolve: This function computes the inverse of the special
## "matrix" returned by makeCacheMatrix above. If the inverse has 
## already been calculated (and the matrix has not changed), then
## the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
	inv = x$getInverse()
	if(!is.null(inv)){
		message("getting cached data")
		return(inv)
	}
	
	data <- x$get()
	inv <- solve(data, ...)
	x$setInverse(inv)
	inv
}

