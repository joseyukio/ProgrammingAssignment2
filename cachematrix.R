# R Progamming: Programming Assignment 2

#Matrix inversion is usually a costly computation and there may be some benefit
#to caching the inverse of a matrix rather than compute it repeatedly. 
#The functions below implement the cache functionality for inverse of matrix.

#makeCacheMatrix: This function creates a special "matrix" object that can 
#cache its inverse. It creates a list containing a function to:
# 1. set the value of the matrix
# 2. get the value of the matrix
# 3. set the value of inverse of the matrix
# 4. get the value of inverse of the matrix

#Assumtion: The matrix supplied is always invertible.

makeCacheMatrix <- function(x = matrix()) {
        #initialize the cache variable
        cachedInverse <- NULL
        
        # setMatrix will assign the received matrix (y) into (x)
        # When the matrix is set the cache variable is intialized to NULL
        setMatrix <- function(y) {
                x <<- y
                cachedInverse <<- NULL                
        }
        #Return the previously set matrix
        getMatrix <- function() x
        #Set the inverse of the matrix so it can be retrieved later on
        setInverse <- function(inverse) cachedInverse <<- inverse
        #Return the inverse of the matrix (cached)
        getCachedInverse <- function() cachedInverse
        #The return of this function makeCacheMatrix is a list of avaialable functions
        list(setMatrix = setMatrix, getMatrix = getMatrix,
             setInverse = setInverse, getCachedInverse = getCachedInverse)

}

# This function computes the inverse of the special "matrix" returned by
# makeCacheMatrix above. If the inverse has already been calculated (and
# the matrix has not changed), then the cachesolve should retrieve the 
# inverse from the cache. 

cacheSolve <- function(x, ...) {        
              
        # Check if the inverse of the matrix  exists in the cache.
        # If so, return the inverse from cache.         
        my_cachedInverse <- x$getCachedInverse()
        
        if(!is.null(my_cachedInverse)) {
                message("This matrix is from cache.")
                return(my_cachedInverse)
        }
        
        # If the value is not in the cache, get the matrix;
        data <- x$getMatrix()
        # And calculate the inverse of such matrix.
        inv <- solve(data)
        # Set the inverse of the matrix into cache so next time it can be got from cache.
        x$setInverse(inv)
        message("This matrix is NOT from cache.")        
        inv
}


# # How to test.

# # Create a matrix as example.
# > my_matrix <- matrix (c(2, 2 , 3, 5), nrow = 2)
# > my_matrix
# [,1] [,2]
# [1,]    2    3
# [2,]    2    5
# Alternatively, you can also create a bigger matrix: 
# my_matrix <- matrix(trunc(rnorm(512*512)*100), 512,512)

# # Create the "special" matric for my_matrix using the function makeCacheMatrix
# > my_special_matrix <- makeCacheMatrix(my_matrix)

# # You can check the just created special matrix by calling the getMatrix function
# > my_special_matrix$getMatrix()
# [,1] [,2]
# [1,]    2    3
# [2,]    2    5
# 
# # Run the cacheSolve. Since this is the first time it will calculate the inverse
# # of the special matrix and return it. It will also store the return value in cache.
# > cacheSolve(my_special_matrix)
# This matrix is NOT from cache.
# [,1]  [,2]
# [1,]  1.25 -0.75
# [2,] -0.50  0.50
# 
# # Run the cacheSolve. Now the inverse of the matrix comes from cache.
# > cacheSolve(my_special_matrix)
# This matrix is from cache.
# [,1]  [,2]
# [1,]  1.25 -0.75
# [2,] -0.50  0.50


# You can also check the CPU time:
# > system.time(cacheSolve(my_special_matrix))
# In order to see the difference (no cache e cache) use a big matrix and run the tests.
### End ###