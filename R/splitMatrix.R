
#' splitMatrices
#'
#' splitMatrix splits a matrix or a splitMatrix into a matrix of matrices along rows or columns based on a number or factor.
#' as.splitMatrix attempts to turn its argument into a matrix
#' is.splitMatrix tests if its argument is a splitMatrix
#'
#' @param mat A matrix to split or a splitMatrix to re-split or a structure what can be coerced into a matrix
#' @param rows A factor indicating how to (re)split `mat` into row matrices, or a single number indicating the number of row matrices `mat` should be split.
#' @param cols A factor indicating how to (re)split `mat` into column matrices, or a single number indicating the number of column matrices `mat` should be split.
#' @param transpose creates the sub matrices transpose
#'
#' @details
#' If `rows` is a factor, it should have the length equal to `nrow(mat)` if `mat` is a matrix or equal to `sum(sapply(mat[,1], nrow))` if `mat` is a `splitMatrix`. if `rows` is a number, the value should be > 1 and <= `nrow(mat)` in case of a pure matrix or <= `sum(sapply(mat[,1], nrow))` if `mat` is a `splitMatrix`.
#'
#' If `cols` is a factor, it should have the length equal to `ncol(mat)` if `mat` is a matrix or equal to `sum(sapply(mat[1,], nrow))` if `mat` is a `splitMatrix`. if `cols` is a number, the value should be > 1 and <= `nrow(mat)` in case of a pure matrix or <= `sum(sapply(mat[,1], nrow))` if `mat` is a `splitMatrix`.
#'
#' If `transpose` is TRUE, then the matrix is first transposed before it is split
#'
#' `splitMatrix` returns a matrix of matrices where `sapply(mat[i,], function(x) { dim(x)[2]})` are equal for all `i` and `sapply(mat[,j], function(x) { dim(x)[1]} )` are equal for all `j`
#'
#' `is.splitMatrix` returns TRUE if the argument is a `splitMatrix`, else it returns FALSE
#'
#' 'as.splitMatrix' tries to coerce the argument in a `splitMatrix`. The method and additional arguments are derived from `splitMatrix`
#'
#' The `print` method, prints the individual dimensions as `name: (row x col)` in a rectangular form.
#'
#' `as.matrix` and `as.data.frame` will will restore the original data matrix
#'
#' @export
#' @examples
#' mat <- matrix(runif(100), 10, 10)
#' splitted.mat <- splitMatrix(mat, 2,2)
splitMatrix <- function(mat, rows=2, cols=2, transpose=FALSE) {

  paste.dot <- function (x, y) {
    paste(x, y, sep='.')
  }

  tm <- function(x) {
    t(as.matrix(x))
  }

  makeFact <- function(fact, length) {
    if (!is.factor(fact)) {
      if (is.numeric(fact)) {
        stopifnot(fact[1]>=0)
        if (fact==0) fact <- 1
        n <- fact[1]
        ni <- round(length/n)
        fact <- rep(seq(1,n), rep(ni,n))
        nd <- length-length(fact)
        if (nd>0) {
          fact <- c(fact,rep(n, nd))
        }
        fact <- factor(fact)
      }
    }
    return(fact)
  }

  if (!is.splitMatrix(mat)) {
    rows <- makeFact(rows, nrow(mat))
    cols <- makeFact(cols, ncol(mat))
    nrows <- nlevels(rows)
    ncols <- nlevels(cols)

    tmd <- function(x) as.data.frame(t(as.matrix(x)))

    if (is.matrix(mat)) mat <- as.data.frame(mat)
    if (transpose==TRUE) {
      out <- matrix(list(), nrow=ncols, ncol=nrows, dimnames=list(levels(cols), levels(rows)))
      res <- sapply(split(mat, rows), tmd, simplify = FALSE)
      for (j in 1:nrows) {
        out[,j] <- sapply(split(res[[j]], cols), as.matrix, simplify=FALSE)
      }
      names(out) <- outer(levels(cols), levels(rows), paste.dot)
    } else {
      out <- matrix(list(), nrow=nrows, ncol=ncols, dimnames=list(levels(rows), levels(cols)))
      res <- sapply(split(mat, rows), tmd, simplify = FALSE)
      j <- 1
      for (j in 1:nrows) {
        out[j,] <- sapply(split(res[[j]], cols), tm, simplify = FALSE)
      }
      names(out) <- outer(levels(rows), levels(cols), paste.dot)    }
  } else {
    class(mat) <- class(mat)[class(mat) != 'splitMatrix'] # to prevent executing possible overloaded functions
    dim <- base::dim(mat)
    for (j in 1:dim[2]) {
      d_j <- dim(mat[1,j][[1]])[2]
      if (dim[1]>1) {
        for (i in 2:dim[1]) {
          d_ <- dim(mat[i,j][[1]])[2]
          if (d_j!=d_) {
            stop('row dimensions do not match')
          }
        }
      }
    }
    for (i in 1:dim[1]) {
      d_i <- dim(mat[i,1][[1]])[1]
      if (dim[2]>1) {
        for (j in 2:dim[2]) {
          if (d_i!=dim(mat[i,j][[1]])[1]) {
            stop('column dimensions do not match')
          }
        }
      }
    }

    mdim <- c(0,0)
    for (i in 1:dim[1]) {
      mdim[1] <- mdim[1]+dim(mat[i,j][[1]])[1]
    }
    for (j in 1:dim[2]) {
      mdim[2] <- mdim[2] +dim(mat[i,j][[1]])[2]
    }

    rows <- makeFact(rows, mdim[1])
    cols <- makeFact(cols, mdim[2])

    for (j in 1:dim[2]) {
      if (1==j) col <- list()
      c_j <- mat[,j]
      col_j <- c_j[[1]]
      if (length(c_j)>1) {
        for (i in 2:length(c_j)) {
          col_j <- rbind(col_j, c_j[[i]])
        }
      }
      col_j <- splitMatrix(col_j, rows=rows, cols=1, transpose=FALSE)
      col[[j]] <- col_j
    }
    out <- matrix(list(), nlevels(rows), nlevels(cols))
    j <- 1
    for (j in 1:nlevels(rows)) {
      for (i in 1:length(col)) {
        if (i==1) {
          row_j <- col[[i]][[j]]
        } else {
          row_j <- cbind(row_j, col[[i]][[j]])
        }
        #col[[i]][[j]] <- list() # clean up memory
      }
      out[j,] <- splitMatrix(row_j, rows=1, cols=cols, transpose=transpose)
    }
  }

  class(out) <- c('splitMatrix', class(out))
  return(out)
}

#' @rdname splitMatrix
#' @export
is.splitMatrix <- function(mat) {
  "splitMatrix" %in% class(mat)
}

#' @param ... Additional arguments passed to the function controlling the conversion process.
#' @rdname splitMatrix
#' @export
as.splitMatrix <- function(mat, ...) {
  splitMatrix(mat, ...)
}

#' @export
as.matrix.splitMatrix <- function(x, ...) {
  x <- splitMatrix(x, 1, 1)[[1]]
  as.matrix(x, ...)
}

#' @export
as.data.frame.splitMatrix <- function(x, ...) {
  as.data.frame(as.matrix(x), ...)
}

#' @export
print.splitMatrix <- function(x, ...) {
  out <- sapply(x, function(y) paste0("(",paste(base::dim(y), collapse=" x "),")"))
  out <- paste(names(x), out, sep=": ")
  out <- matrix(out, base::dim(x)[1], base::dim(x)[2],
                dimnames=list(row.names(x), colnames(x)))
  print.default(out, ...)
}

#' @export
`[.splitMatrix` <- function(x, i, j, ...) {
  class(x) <- class(x)[class(x)!='splitMatrix']
  z <- x[i,j, ..., drop=FALSE]
  if (length(z)>1) {
    class(z) <- c('splitMatrix', class(z))
  } else {
    z <- z[[1]]
  }
  return(z)
}

#' @export
`[<-.splitMatrix` <- function(x, i=NULL, j=NULL, value, ...) {
  class(x) <- class(x)[class(x)!='splitMatrix']
  if (is.null(i)) i <- seq(1,dim(x)[1])
  if (is.null(j)) j <- seq(1,dim(x)[2])
  n <- length(x[i,j,..., drop=FALSE])
  if (!is.splitMatrix(value)) {
    if (n==1) {
      x[i,j,...][[1]] <-value
    } else {
      for (ii in i) for (jj in j) x[ii,jj][[1]] <- value
    }
  } else {
    d <- dim(x[i,j,..., drop=FALSE])
    d1 <- dim(value)
    stopifnot(all(d==d1))
    for (k in 1:length(i)) {
      for (l in 1:length(j)) {
        x[i[k], j[l]][[1]] <- value[k,l]
      }
    }
  }
  class(x) <- c('splitMatrix', class(x))
  x
}

#' @export
t.splitMatrix <- function(x) {
  d <- dim(x)
  for (i in 1:d[1]) {
    for (j in 1:d[2]) {
      x[i,j] <- t(x[i,j])
    }
  }
  class(x) <- class(x)[class(x)!='splitMatrix']
  x <- t(x)
  class(x) <- c('splitMatrix', class(x))
  x
}
