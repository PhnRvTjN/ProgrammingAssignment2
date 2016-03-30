## 2 Functions, respectively to Cache a Matrix, & Solve it.

## This Function creates a special "matrix" object that can Cache its Inverse.

makeCacheMatrix <- function(x = matrix())
{
   inv <- matrix()
   set <- function(y = matrix())
   {
      x <<- y
      inv <<- matrix()
   }
   get <- function()
      x
   setinv <- function(solve)
      inv <<- solve
   getinv <- function()
      inv
   list(set = set, get = get, setinv = setinv, getinv = getinv)
}

## This Function computes the Inverse of the special "matrix" returned by `makeCacheMatrix` above.
## If the Inverse has already been calculated (and the matrix has not changed),
## then `cacheSolve` should retrieve the Inverse from the Cache.

cacheSolve <- function(x, ...)
{
   inv <- x$getinv()
   if (!is.null(inv))
   {
      message("Getting Cached Data ... ")
      return(inv)
   }
   data <- x$get()
   inv <- solve(data, ...) ## Calculate the Matrix that is the Inverse of 'x'
   x$setmean(inv)
   inv ## Return the Matrix that is the Inverse of 'x'
}
