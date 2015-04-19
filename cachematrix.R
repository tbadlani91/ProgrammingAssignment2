+## Creates matrix object that can cache its inverse
+makeCacheMatrix <- function( m = matrix() ) {
+
+	## Initialize the property
+    i <- NULL
+
+    ## setmatrix
+    set <- function( matrix ) {
+            m <<- matrix
+            i <<- NULL
+    }
+
+    ## getmatrix
+    get <- function() {
+    	## Return the matrix
+    	m
+    }
+
+    ##inversematrix
+    setInverse <- function(inverse) {
+        i <<- inverse
+    }
+
+    ##getinversematrix
+    getInverse <- function() {
+        ## Return the inverse property
+        i
+    }
+
+    ##returnmethods
+    list(set = set, get = get,
+         setInverse = setInverse,
+         getInverse = getInverse)
+}
+
+
+## Compute the inverse matrix returned by "makeCacheMatrix" above.
+## If the inverse has already been calculated (and the matrix has not
+## changed), then the "cachesolve" should retrieve the inverse from the cache.
+cacheSolve <- function(x, ...) {
+
+    ##returnmatrixofx
+    m <- x$getInverse()
+
+    ##returnsetinverse
+    if( !is.null(m) ) {
+            message("getting cached data")
+            return(m)
+    }
+
+    ##getmatrix
+    data <- x$get()
+
+    ##computeinverse
+    m <- solve(data) %*% data
+
+    ##inverseobject
+    x$setInverse(m)
+
+    ##returnmatrix
+    m
+}
