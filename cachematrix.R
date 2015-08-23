## Returns the inverse of a matrix. 
## function cacheSolve to find the inverse of a matrix and store the inverse in a cache
## function makeCacheMatrix to store the inverse of the matrix in a cache

## Store the inverse of the matrix in cache

makeCacheMatrix <- function(x = matrix()) {
        
        #Initializing for the first time load
        lastMatrix <- NULL
        
        #Function to retrieve matrix
        getMatrix <- function() x
        
        #Function to cache inverse of the matrix
        #The function accepts matrix as parameter
        #The lastMatrix variable will be updated with the present matrix
        #cacheValue will have the inverse of the matrix
        cacheMatrixInverse <- function(matrixInverse) {
                cat("CACHING INVERSE OF THE MATRIX\n")
                lastMatrix <<- x
                cachedValue <<- matrixInverse
        }
        
        #Function to retrive from the cache
        #Compares against the stored matrix and if similar retruns the cached inverse
        #The matrx is only compared if they are of equal dimensions
        #Other checks on matrix equality are not performed
        checkCacheForMatrixInverse <- function() {
                if (!is.null(lastMatrix) && lastMatrix == x) {
                        cachedValue
                } else {
                        NULL
                }
        }
        
        list(getMatrix = getMatrix, lastMatrix = NULL, cacheMatrixInverse = cacheMatrixInverse, checkCacheForMatrixInverse = checkCacheForMatrixInverse)

}


## Check if the inverse of the requested matrix in the cache
## If not the inverse is stored in the cache and retuned

cacheSolve <- function(x, ...) {
        #Get inverse of the matrix from cache
        mi <- z$checkCacheForMatrixInverse()
        
        #Check to see if inverse was present in cache
        #if inverse present in cache; then reurn the inverse
        #if inverse not present in cache set the inverse to cache
        if (!is.null(mi)) {
                cat("FOUND IN CACHE :", mi, "\n")
                mi
        } else {
                cat("NOT FOUND IN CACHE\n")
                mi <- solve(z$getMatrix())
                z$cacheMatrixInverse(mi)
                mi
        }
}
