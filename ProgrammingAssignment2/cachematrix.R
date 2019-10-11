
## There are two functions here 'makeCacheMatrix' and 'cacheSolve':

##	1. makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse.

##	2. cacheSolve: This function computes the inverse of the special "matrix" returned by makeCacheMatrix above.

##		If the inverse is already cached & the matrix has not changed, then cacheSolve will retrieve the inverse from the cache.




## This function creates a special "matrix" object that can cache its inverse.


makeCacheMatrix <- function(x = matrix()) {
	inversion <- NULL
	set <- function(y){
		x <<- y
		inversion <<- NULL
	}
	get <- function() x
	setinversion <- function(solvematrix) inversion <<- solvematrix
	getinversion <-function() inversion
	list (set=set, get=get, setinversion=setinversion, getinversion=getinversion)

}

## This function computes the inverse of the matrix returned by the makeCacheMatrix function above.

## If the inverse is already cached and the matrix has not changed, then cacheSolve will retrieve the inverse from the cache.



cacheSolve <- function(x, ...) {
	inversion <- x$getinversion()
	if(is.null(inversion)){
		message("getting cached data")
		return(inversion)
	}
	data <- x$get()
	inversion <- solve(data)
	x$setinversion(inversion)
	inversion
        ## Return a matrix that is the inverse of 'x'
}
