## Put comments here that give an overall description of what your
## functions do

## makeCacheMatrix
## pass in a given matrix to initialize an object
##   that is capable of caching its inverse
##   this is the structure that you will pass to the
##   cacheSolve() function in this package
##   use get() and set() to retrieve the original matrix
##   getinverse() and setinverse() retrieve the inverse

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

## cacheSolve
## this function works in conjunction with makeCacheMatrix
##   to enable caching of a matrix's inverse as follows:
##  
##  a <- makeCacheMatrix(matrix(1:4,2,2))
##  cacheSolve(a)
##
##   now the results of solve() will be cached in 'a' and any
##   subsequent times you call this function you'll avoid the 
##   overhead of recalculation
##
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
	minv <- x$getinverse()
	if (!is.null(minv)) {
		message("hitting cache")
		return(minv)
	}
	matrix <- x$get()
	minv <- solve(matrix)
	x$setinverse(minv)
	minv
}
