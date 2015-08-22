## The 'makeCacheMatrix' function here actually creates a list that stores 4 functions
## that eventually helps in caching the inverse of a matrix.
## The 'cacheSolve' function here is used to print inverse of a matrix. It checks if
## the inverse has been cached before. If so, it then skips all the calculation and
## prints the cached data(inverse of the matrix) or else, calculates the inverse
## and stores it as cache.

## This function is a list of 4 function ie. set, get, setinv & getinv.
## set: sets the value of the matrix
## get: gets the value of the matrix
## setinv: sets the inverse of the matrix
## getinv: gets the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setinv <- function(solve) m <<- solve
        getinv <- function() m
        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv)

}


## This function takes the list of functions from 'makeCacheMatrix' as it's input 
## argument. If 'getinv' function from the list return a value, the 'cacheSolve'
## function returns it without proceeding to calculations. Otherwise, it calls
## the get function to store the data of matrix to the variable 'data'
## and further proceeds to find the inverse of the matrix with the solve() function
## and the value is then stored inside the cache variable.

cacheSolve <- function(x, ...) {
        ## Returns a matrix that is the inverse of 'x'
        m <- x$getinv()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setinv(m)
        m
}
