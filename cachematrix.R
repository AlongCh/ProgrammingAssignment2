
# Hi, this is what I came up after struggling through to the concept of lexical closure and scope.

makeCacheMatrix <- function(x = matrix()) {
        i <- NULL # set the unsolved inverse of the specify matrix to null.
        set <- function(y){ #set new matrix values...
                x <<- y # replace the old values with new input...
                i <<- NULL # re-set the inverse values to Null.
        }   
        get <- function() x # the function for calling the input matrix
        setInverse <- function(inverse) i <<- inverse  # a function to set inverse values of the input matrix, 
        # the inverse goes into the varirable i.
        getInverse <- function() i # a function to call the solved inverse values.
        list(set = set, get = get,
             getInverse = getInverse,
             setInverse = setInverse)
}


cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        i <- x$getInverse() # called the getInverse() function to see if the inverse of the matrix had already been solved?... 
        if(!is.null(i)) { # if there is an determinable inverse value in makeCacheMatrix...
                message("getting cached data") # it will return a message "getting cached data"...
                return(i)  # together with the inverse value.             
        }
        data <- x$get() # if the inverse of the matrix haven't yet determined.. get the original input...
        i <- solve(data,...) # and solve for the inverse then replace it into variable i...
        x$setInverse(i) # put the new inverse values into the function setInverse(i).
        i # return the solved output
}