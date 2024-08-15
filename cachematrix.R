## Function Descriptions
## makeCacheMatrix(): is a function to create a matrix that can cache its inverse
## cacheSolve(): is a function that computse the inverse of the matrix returned
## by makeCacheMatrix(). If the inverse is already calc'd then cacheSolve retrieve 
## the inverse from the cache


## makeCacheMatrix is a function that creates a matrix, which is really a list
## that contains a function to:
##      1) set the value of the matrix
##      2) get the value of the matrix
##      3) set the value of the matrix's inverse
##      4) get the value of the matrix's inverse
## This function is based on principles of makeVector() function from class example
makeCacheMatrix <- function(x = matrix()) {
        
        # establish empty matrix to fill
        inverseMatrix <- NULL
        
        # set function for matrix
        setMatrix <- function(y){
                x <<- y
                inverseMatrix <<- NULL
        }
        
        # return matrix object
        getMatrix <- function() {
                x #return matrix
        }
        
        # set inverse matrix
        setMatrixInverse <- function(invMATX) {
                inverseMatrix <<- invMATX
        }
        # return inverse matrix
                getMatrixInverse <- function(){
                    inverseMatrix    
                } 
        list(setMatrix = setMatrix, getMatrix = getMatrix,
             setMatrixInverse = setMatrixInverse,
             getMatrixInverse = getMatrixInverse)
}


## cacheSolve() should verify if inverse has been calculated and "the matrix has not changed"
## if so, grab from cache
## based on cachemean example provided
cacheSolve <- function(x, ...) {
        inverseMatrix <- x$getMatrixInverse()
        # check if inverse has been calculated
        if(!is.null(inverseMatrix)){
                message("getting cached data")
                return(inverseMatrix)
        }
        data <- x$getMatrix()
        inverseMatrix <-solve(data,...) #DOUBLE CHECK THIS PART
        x$setMatrixInverse(inverseMatrix)
        ## Return a matrix that is the inverse of 'x'
        inverseMatrix
}
