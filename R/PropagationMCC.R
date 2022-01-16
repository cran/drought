#' Compute drought propagation based on maximum correlation
#' 
#' Compute the pearson correlation between multi-time scale SPI and 1-month SRI
#' to reflect the most possible propagation time (PT) from
#' meteorological drought to hydrological drought.
#' Note here the propagation of meteorological to hydrological drought is used as an example.
#' The propagation of other types of drought  can also be computed.
#'
#' @param X The vector of monthly meteorological variable (e.g., precipitation)
#' @param Y The vector of monthly hydrological variables (e.g., runoff)
#' @param acc Maximum of propagation time (or accumulation periods)
#' @param lim The limits interval for color
#' @param color Color vector in plot
#' @references Xu, Y. et al (2019). Propagation from meteorological drought to hydrological drought under the impact of human activities: A case study in northern China. J. Hydrol. 579, 124147.
#' @return Plot of correlation matrix
#' @examples
#' X=runif(120, min = 0, max = 100) # 10-year monthly data
#' Y=runif(120, min = 0, max = 100) # 10-year monthly data
#' acc <- 12
#' lim <- c(-1,1)
#' PropagationMCC(X, Y, acc, lim)
#' @export
PropagationMCC <- function (X, Y, acc = 12, lim = c(-1,1), color = NA) {
  #  compute SRI-1
  fit <- SDI(Y, ts = 1)
  SD2 = matrix(t(fit$SDI), ncol=12, byrow=1)

  mcorr = matrix(nrow=12, ncol=acc)

  #  compute SPI-1 to SPI-acc
  for (k in 1:acc){
    fit <- SDI(X, ts = k)
    SD1 = matrix(t(fit$SDI), ncol=12, byrow=1)

    # compute for each month
    for (q in 1:12){
      z0 <-cbind(SD1[,q],SD2[,q])
      # Remove the null value
      z <- stats::na.omit(z0)
      mcorr[q,k] <- stats::cor(z[,1],z[,2], method = "pearson")
    }
  }

  rownames(mcorr) <- c("Jan", "Feb", "Mar", "Apr", "May",
                      "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")
  colnames(mcorr) <- c(1:acc)

  if (is.na(color[1]) == 1){
    # default colors
    corrplot::corrplot(mcorr, method = "color", is.corr = FALSE, col.lim = lim ,
                       tl.col ="black", tl.srt = 45)
  }else{
    # custom colors
    corrplot::corrplot(mcorr, method = "color",is.corr = FALSE, col.lim = lim , col = color,
                       tl.col ="black", tl.srt = 45)
  }
  return(mcorr)

}



