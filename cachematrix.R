## These pair of funtion pretend to cache the inverse of a matrix


## This function creates a special "matrix" object that can cache its inverse. The function pretend to

## set the value of the vector
## get the value of the vector
## set the value of the mean
## get the value of the mean

makeCacheMatrix <- function(x = matrix()) {
        Matriz_Inversa <- NULL               ## initialize inv as NULL; will hold value of matrix inverse 
        
                setMatrix <- function(y) {  ##set the value of the Matrix
                x <<- y                     ## value of matrix in upper environment
                Matriz_Inversa <<- NULL     ## If new matrix is detected, reset Matrizinversa to NULL
        ## the <<- operator is used to assign a value to an object in an environment that is different from the current environment.
                 }
        
        getMatrix <- function() x                                  #get the value of the Matrix
        setInverse <- function(inverse) Matriz_Inversa <<- inverse  #set the value of the invertible matrix
        getInverse <- function() Matriz_Inversa                     #get the value of the invertible matrix
        list(setMatrix = setMatrix, getMatrix = getMatrix,
             setInverse = setInverse, getInverse = getInverse)     ## To use the $ operator
}

## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above.
## If the inverse has already been calculated (and not changed),
## then this function (cacheSolve) will retrieve the inverse from the cache

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        Matriz_Inversa <- x$getInverse()
        if(!is.null(Matriz_Inversa)) {
                message("getting data already cached")
                return(Matriz_Inversa)
        }
        Original_data <- x$getMatrix()                     ## get the original Matrix Data 
        Matriz_Inversa <- solve(Original_data, ...)  ## use solve function to inverse the matrix
        x$setInverse(Matriz_Inversa)                 ## set the invertible matrix
        Matriz_Inversa                               ## return the invertible matrix
}
