## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

#Following function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
        #Setting the inverse as Null initially as the inverse matrix can be filled
        inverse <- NULL 
                #for getting the matrix
                get <- function() {
                        x
                }
                ##for setting the matrix
                set <- function(Matrix1) {
                        x <<- Matrix1
                        inverse <<- NULL
                }
                ##getting the inverse matrix
                Inverse_get <- function() {
                        inverse
                }
                ##setting the inverse matrix
                Inverse_set <- function(Matrix2) {
                        inverse <<- Matrix2
                }
                list(get = get, set = set, Inverse_get = Inverse_get, Inverse_set = Inverse_set)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        
        ## checking if inverse exists in cache
        inverse <- x$Inverse_get()
        ##if inverse exists returning the same
        if(!is.null(inverse)) {
                message("getting cached data")
                return(inverse)
        }
        ##if no inverse found, then get the matrix and inverse it 
        ##and set the inverse matrix and return the same
        data <- x$get()
        
        inverse <- solve(data, ...)
        
        x$Inverse_set(inverse)
        
        return(inverse)
}
