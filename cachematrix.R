## This pair of functions allow a user to create a special matrix and compute
## its inverse. If the inverse has been previously computed and the matrix has
## not been changed ever since, the old inverse is returned, otherwise it is 
## computed from scratch.

## The first function creates the special matrix, i.e. a list of 4 elements
## each of which is a function:
## 1. set(); 
## 2. get(); 
## 3. setInverse();
## 4. getInverse().
## The environment of this function includes 2 attributes:
## 1. x, which is a proper matrix;
## 2. inv, which is the inverse of x.

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
 
  set <- function(y){ # set the matrix
    x <<- y
    # since the matrix is changing we need to reset its inverse
    # (i.e. the old one is no longer valid):
    inv <<- NULL 
  }
  
  # Simply return the matrix:
  get <- function() x 
  
  # Allow user to provide with an already computed inverse:
  setInverse <- function(inverse) inv <<- inverse
  
  # Return the inverse:
  getInverse <- function() inv

  list(set = set, get = get, 
       setInverse = setInverse,
       getInverse = getInverse)
}


## With the following function the user asks to compute the inverse of x:
## -if it has already been cached it is returned without needing to be computed;
## -otherwise it is computed using the parameters provided by the user (...).

cacheSolve <- function(x, ...) {
  inv <- x$getInverse()             # Read the stored inverse:
  if (!is.null(inv)) {              # if it isn't NULL
    message("Getting cached data")  # warn the user
    inv                             # and return it;
  }else{                            # otherwise
    message("Computing")            # warn the user
    data <- x$get()                 # get the matrix
    inv <- solve(data, ...)         # compute the inverse
    x$setInverse(inv)               # store it
    inv                             # and return it.
  }
}
