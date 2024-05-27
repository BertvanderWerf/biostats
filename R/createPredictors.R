#' createPredictors
#'
#' @param object a fitted model or a character string indicating the explanatory variables
#' @param data the dataset to create the predictors from, when omitted getData(object) is used)
#' @param marginal possible values c('yes','no','minmax','existing') see details section
#' @param nsteps integer value > 0, indicating the number of steps to use for creating a series of a continuous variable
#'
#' @details
#' Additional details...
#'
#' @return a new dataset with the predictor values as specified
#' @export
#'
#' @examples
#' fit <- lm(x ~ Sex*AgeClass, data=data)
#' newData <- createPredictors(fit, marginal='yes')
createPredictors <- function(object, data=NULL, marginal=c('yes','no','minmax','existing'), nsteps=1) {
  marginal <- match.arg(marginal)
  nsteps <- pmax(1, nsteps)

  if (is.character(object)) {
    stopifnot(!is.null(data))
    stopifnot(all(object %in% names(data)))
    Names <- object
  } else if (is.null(data)) {
    data <- getData(object)
    Names <- getFixedVars(object, data=data)
  }
  Factor <- sapply(data[,Names], is.factor)
  Text <- sapply(data[,Names], is.character)
  if (any(Text)) {
    Warning('Having text as a predictor can cause problems because the order of the "levels" is not defined')
  }

  if (marginal=='existing') {
    predictors <- data[,Names]
    predictors <- predictors[!duplicated(predictors),]
    predictors <- na.omit(predictors)
  } else if (marginal=='no') {
    predictors <- data[,Names]
    predictors <- na.omit(predictors)
  } else {
    L <- list()
    for (i in 1:length(Names)) {
      if (Factor[i]) {
        L[[Names[i]]] <- levels(data[,Names[i]])
      } else if (Text[i]) {
        L[[Names[i]]] <- unique(data[,Names[i]])
      } else if (marginal=='yes') {
        L[[Names[i]]] <- mean(data[,Names[i]], na.rm=TRUE)
      } else  { # marginal = minmax
        min <- min(data[,Names[i]], na.rm=TRUE)
        max <- max(data[,Names[i]], na.rm=TRUE)
        L[[Names[i]]] <- seq(min, max, (max-min)/nsteps)
      }
    }
    if (length(L)>0) {
      predictors <- expand.grid(L)
    } else {
      predictors <- data.frame()
    }
  }
  return(predictors)
}
