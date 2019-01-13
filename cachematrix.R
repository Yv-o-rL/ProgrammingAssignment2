## Caching an inverted matrix (using 'solve()')
## to speed up repeated actions

## Creates a special 'Matrix' which is a list
## of functions
makeCacheMatrix <- function(x = matrix()) {
        im <- NULL
        set <- function(y) { #set original matrix
                x <<- y
                im <<- NULL
        }
        get <- function() x #get original matrix
        setinverse <- function(solve) im <<- solve #set inverted matrix
        getinverse <- function() im #get inverted matrix
        list(set = set, get = get,  
             setinverse = setinverse,
             getinverse = getinverse)
}

## If a makeCacheMatrix object exists, this function will
## return the inverse of the original matrix
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        im <- x$getinverse() #retrieve data from cache
        if(!is.null(im)) { #if cached data exists return that
                message("Getting cached data")
                return(im)
        }
##if there isn't cached inverted matrix, create one and return it
        data <- x$get()
        im <- solve(data, ...)
        x$setinverse(im)
        im
}

