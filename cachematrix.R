makeCacheMatrix = function(x = matrix()){
  k = NULL
  set = function(y){
    x <<- y
    k <<- NULL
  }
  get = function() x
  setinv = function(inverse) k <<- inverse
  getinv = function() k
  list(set = set, get = get, 
       setinv = setinv, getinv = getinv)
}

cacheSolve = function(x,...){
  k = x$getinv()
  if (!is.null(k)){
    message("getting cached data")
    return(k)
  }
  data = x$get()
  k = solve(data,...)
  x$setinv(k)
  k
}
