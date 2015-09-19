## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
	minv <- NULL
	set <- function(y) {
		x <<- y
		minv <<- NULL
	}
	get <- function() x
	setinverse <- function(inv) minv <<- inv
	getinverse <- function() minv
	list(set=set, get=get, setinverse=setinverse,
		getinverse=getinverse)

}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
	minv <- x$getinverse()
	if (!is.null(minv)) {
		return(minv)
	}
	matrix <- x$get()
	minv <- solve(matrix)
	x$setinverse(minv)
	minv
}
