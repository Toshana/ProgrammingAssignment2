## Put comments here that give an overall description of what your
## functions do

# makeCacheMatrix is passed a matrix which creates an object of type "list" that
# stores the original matrix value and the cached value of the inverse of the matrix, originally set to NULL.
# cacheSolve accesses this object by getting the value of the matrix used to create this object and calculating 
# the inverse using 'solve' and then storing it in the call to makeCacheMatrix. If the inverse has already been calculated, it
# returns the cached value.

## Write a short comment describing this function

## makeCacheMatrix creates an object with variable "inverse" initially set to NULL. When makeCacheMatrix is called, and 
## "inverse" is set to a non-NULL value, it returns the cached value.

makeCacheMatrix <- function(x = matrix()) {
    inverse <- NULL
    set <- function(y) {
        x <<- y
        inverse <<- NULL
    }
    get <- function() x
    setinverse <-function(solve) inverse <<- solve
    getinverse <<- function() inverse
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)

}


## Write a short comment describing this function

## cacheSolve accesses the object created by makeCacheMatrix. If the value of "inverse" is not NULL, it displays the 
## message "getting cached data" and retrieves the cached value in the above function. If the value of inverse is NULL
## then it gets the original matrix, finds the inverse, and stores it in the variable "inverse".

cacheSolve <- function(x, ...) {
    inverse <- x$getinverse()
    if(!is.null(inverse)) {
        message("getting cached data")
        return(inverse)
    }
    data <- x$get()
    inverse <- solve(data, ...)
    x$setinverse(inverse)
    inverse
        ## Return a matrix that is the inverse of 'x'
}
