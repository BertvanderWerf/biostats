
#' splitMatrices
#'
#' Splits a matrix or splitMatrix into a matrix of matrices along rows or columns based on a number or factor.
#'
#' @param mat A matrix to split or a splitMatrix to re-split
#' @param rows A factor with length equal to the number of rows of the input matrix. If a single number is provided, it indicates the number of rows the matrix should be split.
#' @param cols A factor with length equal to the number of columns of the input matrix. If a single number is provided, it indicates the number of columns the matrix should be split.
#' @param transpose creates the sub matrices transpose
#'
#' @return a splitMatrix containing a matrix of matrices
#' @export
#'
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

  class(out) <- c(class(out), 'splitMatrix')
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

