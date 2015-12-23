#takes a matrix and makes availabe helper functions to inspect
#the original matrix
#the inverse of the original matrix
#and the necessary helper functions to set the value of the
#original matrix
#and its inverse
makeCacheMatrix <- function(x = matrix()) {
        # each instance of this function will start
        # with a null inverse
        inverseMatrix <- NULL
        
        # setter function; sets x to a new matrix and erases the inverse
        set <- function(newMatrix) {
                x <<- newMatrix
                inverseMatrix <<- NULL
        }
        
        # getter function; returns the original matrix x
        get <- function() x
        
        # setter function for the inverse of the original x matrix
        setInverse <- function(inverseOfMatrix) inverseMatrix <<- inverseOfMatrix
        
        # getter function for the inverse: returns the inverse of x, if it's been calculated
        getInverse <- function() inverseMatrix
        
        list(
                set = set 
                ,get = get 
                ,setInverse = setInverse
                ,getInverse = getInverse
        )
}


#this function takes a variable that has been assigned the makeCacheMatrix() function
#and solves for the inverse of a square matrix and cache's the inverse matrix calculation
#if the inverse has not been solved and cached previously
cacheSolve <- function(x, ...) {
        inverseMatrix <- x$getInverse()
        
        #check to see if the inverse of the matrix has been calculated
        if(!is.null(inverseMatrix)) {
                #the inverse has been found and we return the cached value
                message("getting cached data")
        }
        else {
                #the inverse of the matrix must be calcualted
                data <- x$get()                 # retrieve the matrix
                inverseMatrix <- solve(data)    # use solve to find the inverse
                x$setInverse(inverseMatrix)     # write the inverse of the matrix to the cache
        }
        
        # return either the cached or 
        # newly calcuate inverse of the matrix
        inverseMatrix                   
}
