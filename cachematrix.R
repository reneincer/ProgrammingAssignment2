## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
#This function creates an object with 4 methods,
#get and set for the original matrix, and getInverse setInverse to return calculated inverse
#The calculated inverse is the result from cacheSolve, where you have to pass the object
#and its get the inverse, if the inverse is null it calculates it and save it to the object
#with the setInverse, but it exists, then just return it with getInverse
#Remember for caching putting double <,  <<

#This Code is for testing
#n = 2000
#x <- matrix(runif(n^2),n)
#amatrix = makeCacheMatrix(x)
#cacheSolve(amatrix)
#amatrix$getInverse()

makeCacheMatrix <- function(x = matrix()) {
        #declare matrix variable for inverse matrix
        inverseMatrix <- NULL
        #inverseMatrix <- matrix() 
        set <- function(y) {
                #when you set up new matrix, inverse is cleaned
                x <<- y
                inverseMatrix <<- NULL
        }
        #return original matrix
        get <- function() x
        #set from CacheSolve the inverseMatrix
        setInverse <- function(inverse) inverseMatrix <<- inverse
        #get inverse matrix from cache
        getInverse <- function() inverseMatrix
        #list of subfunctions we could call
        list(set = set, get = get,
             setInverse = setInverse,
             getInverse = getInverse)
}


## Write a short comment describing this function

cacheSolve <- function(x) {
        ## Return a matrix that is the inverse of 'x'
        #getting inverse cached matrix
        iMatrix <- x$getInverse()
        #it exists return from cache,
        if(!is.null(iMatrix)) {
                message("getting cached data")
                return(iMatrix)
        }
        #else, we calculate inverse matrix and set it up to cache
        #we first get from x the original matrix, and do a solve(inverse)
        iMatrix  <- solve(x$get())
        x$setInverse(iMatrix)
        iMatrix
}
