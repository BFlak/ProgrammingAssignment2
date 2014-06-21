## Put comments here that give an overall description of what your
## functions do

## makeCacheMatrix is a function that returns a list of functions.
## A matrix is stored internally, its initial value can be passed as a parameter in makeCacheMatrix.
##  * set_matrix - replaces the matrix stored internally.
##  * get_matrix - retrieves the stored matrix. 
##  * set_inverse - allows the matrix inverse to be set in the cacheSolve funtion.
##  * get_inverse - stores the result of set_inverse, allowing for direct retrieval of the matrix inverse.

makeCacheMatrix <- function(x = matrix()) 
{
    inv <- NULL
    set_matrix <- function(y)
    {
        x <<- y
        inv <<- NULL
    }
    get_matrix <- function() x
    set_inverse <- function(inverse)
    {
        inv <<- inverse
    }
    get_inverse <- function() inv
    list(set_matrix = set_matrix, get_matrix = get_matrix, set_inverse = set_inverse, get_inverse = get_inverse)
}


## cacheSolve is a function that:
##  * first checks to see if the matrix inverse has already been calculated by checking 
##    if the get_inverse element of the list from makeCachematrix is null. 
##  * If it is not null, cacheSolve returns the get_inverse result. 
##  * If it is null, cacheSolve calculates the matrix inverse and inputs it into the 
##    set_inverse function from makeCacheMatrix.

cacheSolve <- function(x, ...) 
{
        ## Return a matrix that is the inverse of 'x'
    inv <- x$get_inverse()
    if(!is.null(inv))
    {
        message("getting cached data")
        return(inv)
    }
    mat <- x$get_matrix()
    inv <- solve(mat)
    x$set_inverse(inv)
    inv
}
