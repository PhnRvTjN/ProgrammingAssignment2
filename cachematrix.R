## 2 Functions, respectively to Cache a Matrix, & Solve it.

## This Function creates a special "matrix" object that can Cache its Inverse.
makeCacheMatrix <- function(x = matrix())
{
   inv <- matrix()

   ## Setting the Value of the Matrix.
   set <- function(y = matrix())
   {
      x <<- y
      inv <<- matrix()
   }

   ## Getting the Value of the Matrix.
   get <- function()
      x

   ## Setting the Value of the Inverse of the Matrix.
   setinv <- function(solve)
      inv <<- solve

   ## Getting the Value of the Inverse of the Matrix.
   getinv <- function()
      inv
   list(set = set, get = get, setinv = setinv, getinv = getinv)
}

## This Function computes the Inverse of the special "matrix" returned by `makeCacheMatrix` above.
## If the Inverse has already been calculated (and the matrix has not changed),
## then `cacheSolve` should retrieve the Inverse from the Cache.
cacheSolve <- function(x, ...)
{
   ## Return a Matrix that is the Inverse of 'x'.
   inv <- x$getinv()

   ## Return the Inverse if it's already Set.
   if (!is.null(inv))
   {
      message("Getting Cached Data ... ")
      return(inv)
   }

   ## Get the Matrix from our Object.
   data <- x$get()

   ## Calculate the Matrix that is the Inverse of 'x'.
   inv <- solve(data, ...)

   ## Set the Inverse to the Object.
   x$setinv(inv)
   ## Return the Matrix that is the Inverse of 'x'
   inv
}
