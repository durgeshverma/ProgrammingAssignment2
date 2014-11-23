## testMatrixCache
##      test method for matrix cache and its inverse computation
##      input parameter:
##          matrix that needs to be inverse
testMatrixCache <- function(m = matrix()) {
    if(is.matrix(m)) {
        mc <- makeCacheMatrix(m) #define cache
        inv1 <- cacheSolve(mc) #compute inverse
        print('matrix and its inverse is stored in cache, next call will return from cache and print message "getting cached data" on console')
        inv2 <- cacheSolve(mc) #compute inverse again, 
        if(matequal(inv1,inv2)) { #compare matrices
            print('both matrix inverses are same')
        }
    } else {
        print('ERROR: input is not matrix...')
    }
}

## function makeCacheMatrix
##      provides cache for matrix passed to function
##      provides getter/setter to access the matrix
##      provides getter/setter to access inverse of the given matrix
##      input param: 
##          matrix
##      returns:
##          cache (list containing function pointers of getter/setter methods)
makeCacheMatrix <- function(x = matrix()) {
    i <- NULL
    set <- function(y) {
        x <<- y
        i <<- NULL
    }
    get <- function() { x }
    setinv <- function(inv) i <<- inv
    getinv <- function() i
    list(set = set, get = get,
         setinv = setinv,
         getinv = getinv)
}

## function cacheSolve
##      computes the inverse of given matrix if not found in the cache
##      input param:
##          cache (list returned by makeCacheMatrix function)
##      returns:
##          inverse of given matrix
cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    i <- x$getinv()
    if(!is.null(i)) {
        message("getting cached data")
        return(i)
    }
    data <- x$get()
    check = try(i <- solve(data, ...), silent = TRUE)
    if(class(check) != "try-error") {
        x$setinv(i)
    } else {
        message("ERROR: matrix cannot be inversed !")
    }
    i
}

## function matequal
##      compares two matrices for equality
##      input parameters:
##          two matrices to be compared
##      returns:
##          TRUE if matrices are equal else FALSE
matequal <- function(x, y) {
    is.matrix(x) && is.matrix(y) && dim(x) == dim(y) && all(x == y)
}

