## Programming Assignment 2
## R function that cache the inverse of a Matrix
## The first function, makeCacheMatrix creates a special "Matrix", which is really a list containing a function to

##      set the value of the Matrix
##      get the value of the Matrix
##      set the value of the inverse Matrix
##      get the value of the inverse Matrix
## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {

## Set to Null
        
        i <- NULL
## The set function will add the matrix to the list        
        set <- function(y) {
                x <<- y
                i <<- NULL
        }
## The get function will provide the initial matrix
        get <- function() x

## Sets the inverse matrix in the list        
        setinverse <- function(inverse) i <<- inverse

## Gets the inverse matrix since it is chached        
        getinverse <- function() i
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
        
        
}

## cachesolve:
## calculates the inverse of the matrix created with the above function. 
## However, it first checks to see if the inverse of the matrix has already been calculated. 
## If so, it gets the inverse from the cache and skips the computation. Otherwise
## it calculates the inverse of the data and sets the inverse in the cache via the setinverse function

cacheSolve <- function(x, ...) {
        
        ## Return a matrix that is the inverse of 'x'
        ## checks if the MAtrix has been already computed
        ## For this I sue the getmean() function 
        
        i <- x$getinverse()
        if(!is.null(i)) {
        
        ## In case it is different than null means tha has been computed before
        ## So that we take the data from the cache
        
                message("getting cached data")
                return(i)
        }

        ## If it hasn´t been inverse, get the matrix itself and calculate the inverse
        data <- x$get()
        i <- solve(data, ...)
        
        ## Set the inverse for futre calculations (cache the inverse)
        x$setinverse(i)
        
        ## return the value
        i
                
}
