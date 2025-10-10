#' table1
#'
#' @param data
#' @param rows
#' @param cols
#' @param name.total
#' @param fun
#' @param ...
#'
#' @returns
#' @export
#'
#' @examples
table1 <- function(data, rows, cols, name.total='Total', fun=chisq.test, ...) {
  library(dplyr)

  test <- function(x1, x2, na.rm=TRUE, digits=3, na.default='0 (0%)') {
    if (na.rm==TRUE) {
      ni <- sum(!is.na(x1))
      n <- sum(!is.na(x2))
    } else {
      ni <- length(x1)
      n <- length(x2)
    }
    paste0(ni," (",signif(100*ni/n, digits), "%)")
  }

  count_n <- function(x, na.rm=TRUE, na.default=0) {
    if (na.rm==TRUE) {
      ni <- sum(!is.na(x))
    } else {
      ni <- length(x)
    }
    return(ni)
  }

  meanAndSd <- function (x, na.rm = TRUE, digits = 2, ...) {
    m <- mean(x, na.rm = na.rm)
    if (is.na(m)) {
      NA
    } else {
      paste0(round(mean(x, na.rm = na.rm), digits), " (", round(sd(x,
                                                                   na.rm = na.rm), digits), ")")
    }
  }

  medianAndCL <- function(x, na.rm=TRUE, digits = 2, ...) {
    m <- quantile(x, prob=c(0.5, 0.25, 0.75))
    paste0(round(m[1], digits), "(", round(m[2], digits),", ",round(m[3], digits),")")
  }

  # if total as a factor does not exist, create it with the name name.total
  if (name.total %in% names(data)) {
    if (is.factor(data[,name.total]) && nlevels(data[,name.total])>1) {
      stop(paste(name.total,'is already in data and has more than 1 level, please choose anoter name for name.total'))
    } else {
      u <- unique(data[,name.total])
      u <- u[!is.na(u)]
      if (length(u)>1) {
        stop(paste(name.total,'is already in data and has more than 1 level, please choose anoter name for name.total'))
      } else {
        data[,name.total] <- factor(data[,name.total], u)
      }
    }
  } else {
    data[,name.total] <- factor(name.total)
  }

  tab <- ctable(data, name.total, rows=c(Level=name.total), cols=cols, rfun='test', envir=environment(), ...)
  tab.total <- ctable(data, name.total, rows=c(Level=name.total), cols=c(name.total), fun='count_n', envir=environment(), ...)
  names(tab.total)[3] <- name.total
  tab <- left_join(tab, tab.total, by=c('variable','Level'))
  tab$p_value <- NA
  #tab$p_value.eur_pac <- NA

  TAB <- tab
  TAB.mean <- data.frame()
  TAB.median <- data.frame()
  for (term in rows[!(rows %in% cols)]) {
    if (is.factor(data[,term]) | is.character(data[,term])) {
      tab <- ctable(data, term, rows=term, cols=cols, cfun='test', envir=environment(), ...)
      tab.total <- ctable(data, term, rows=term, cols=c(Ethnicity=name.total), cfun='test', envir=environment(), ...)
      names(tab.total)[3] <- name.total
      tab <- left_join(tab, tab.total, by=c('variable',term))

      tab.test <- ctable(data, term, rows=term, cols=cols, fun='count_n', envir=environment(),...)
      tab.test <- as.matrix(tab.test[,-c(1:2)])
      #  row.names(tab.test) <- tab[,term]
      c <- fun(tab.test)
      p_value <- c$p.value
      tab$p_value <- NA
      tab$p_value[1] <- p_value

      # tab.test <- tab.test[,c('European','Pacific Peoples')]
      # c <- chisq.test(tab.test)
      # p_value <- c$p.value
      # tab$p_value.eur_pac <- NA
      # tab$p_value.eur_pac[1] <- p_value

      names(tab)[names(tab)==term] <- 'Level'
      TAB <- rbind(TAB, tab)
    } else {
      if ((count_n(unique(data[,term]))==1) && all(unique(data[,term]) %in% c(0,1))) { # binomial regression
        stop('binomial regression not implemented yet')
      } else {
        info <- ctable(data, term, rows=c(Ethnicity=cols), fun=c('mean','sd', n='count_n'), envir=environment(), ...)
        b <- bartlett.test2(info$sd, info$n)
        x <- data[,term]-tapply(data[,term], data[,cols], mean)[as.integer(data[,cols])]
        k <- ks.test(x, 'pnorm', mean(x), sd(x))
        b$p.value
        k$p.value
        if (b$p.value<0.01 | k$p.value <0.01) {
          kr <- kruskal.test(x=data[,term], g=data[,cols])
          kr$p.value
          tab <- cbind(ctable(data, term, cols=cols, fun='medianAndCL', envir=environment(), ...),
                       ctable(data, term, cols=name.total, fun='medianAndCL', envir=environment(), ...)[,-1, drop=FALSE],
                       p_value=kr$p.value)
          TAB.median <- rbind(TAB.median, tab)
        } else {
          a <- aov(as.formula(paste(term, cols, sep='~')), data)
          s <- summary(a)[[1]]
          tab <- cbind(ctable(data, term, cols=cols, fun='meanAndSd', envir=environment(), ...),
                       ctable(data, term, cols=name.total, fun='meanAndSd', envir=environment(), ...)[-1, drop=FALSE],
                       p_value=s$`Pr(>F)`[1])
          TAB.mean <- rbind(TAB.mean, tab)
        }
      }
    }
  }
  # print(environment())
  # print(environment(test))
  return(list(TAB, TAB.mean, TAB.mean))
}
