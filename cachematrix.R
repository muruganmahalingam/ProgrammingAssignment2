#Author Murugan Mahalingam

## makeCacheMatrix creates and returns a list of functions
## used by cacheSolve to get or set the inverted matrix in cache
makeCacheMatrix <- function(x = matrix()) {
	# stores the cached value
	# initialize to NULL
	inv <- NULL
	
	# create the matrix in the working environment
	set <- function(y) {
		x <<- y
		inv <<- NULL
	}
	# get the value of the matrix
	get <- function() x
	# invert the matrix and store in cache
	setInverse <- function(inverse) inv <<- inverse
	# get the inverted matrix from cache
	getInverse <- function() inv
	# return the created functions to the working environment
	list(set = set,
			get = get,
			setInverse = setInverse,
			getInverse = getInverse)
}

## cacheSolve calcluates the inverse of the matrix created in makeCacheMatrix
## If the inverted matrix does not exist in cache,
## it it created in the working environment and it's inverted value
## is stored in cache
cacheSolve <- function(x, ...) {
	## attempt to get the inverse of the matrix stored in cache
	inv <- x$getInverse()
	
	# return inverted matrix from cache if it exists
	# else create the matrix in working environment
	if (!is.null(inv)) {
		message("getting cached data")
		return(inv)
	}
	# create matrix since it does not exist
	mat <- x$get()
	#sets the inverse of matrix to newly created variable
	inv <- solve(mat, ...)
	x$setInverse(inv)
	#returns the inverted matrix
	inv
}