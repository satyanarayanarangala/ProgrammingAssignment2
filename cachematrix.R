## Starting from a normal square (and invertible) matrix, the two functions combined allow to
## build a special matrix, calculate the inverse of the matrix and check if the inverse has already
## been calculated, in this way avoiding to waste time in repeating unnecessary calculations.

##  What my function do 
## The first function takes a normal square numeric matrix and transform
## it to a special matrix provided with in-built functions that allow to:
## 1- modify the value of the matrix.
## 2- retrieve the matrix.
## 3- set the inverse of the matrix to a given matrix (it does not calculate the inverse).
## 4- retrieve the inverse of the matrix if it has already been calculated.


makeCacheMatrix <- function(x = matrix()) { 
m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setInverse <- function(solve) m <<- solve
    getInverse <- function() m
    list(set = set, get = get,
         setInverse = setInverse,
         getInverse = getInverse)

}


## The second function takes a special matrix, built using the first 
## function, and checks if its inverse has already been calculated:
## 1- if it has, the function returns the inverse without calculating it again.
## 2- if it has not, the function calculates the inverse and returns it.


cacheSolve <- function(x, ...) {
        m <- x$getInverse()
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    data <- x$get()
    m <- solve(data, ...)
    x$setInverse(m)
    m
}
