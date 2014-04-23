## This two functions implements the matrix cacheing problem. 
## One of them stores the matrix and the inverse, the other
## one computes, if necessary.

## makeCacheMatrix function generates a structure, which contains
## the original matrix, the inverse of original matrix (if it was
## computed before), and the functions to set and get this data.


makeCacheMatrix <- function(x = matrix()) {
	inv <- NULL
	set <- function(y) {
		x <<- y
		inv <<- NULL
	}
	get <- function() x
	setinverse <- function(inverse) inv <<-inverse
	getinverse <- function() inv
	list(set = set, get = get,
		setinverse = setinverse,
		getinverse = getinverse)
}


## cacheSolve function tries to get the inverted matrix if it was
## computed before, and computes it otherwise, then stores the result
## before returns the inverted matrix

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
	inv <- x$getinverse()
	if(!is.null(inv)) {
		message("getting cached data")
		return(inv)
	}
	orig <- x$get()
	inv <- solve(orig, ...)
	x$setinverse(inv)
	inv
}
