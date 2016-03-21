#' Check baseline equivalency
#'
#' @param raw.DF A number.
#' @param matched.DF A number.
#' @return Returns a list with plots and tables
#' @export
#' @import dplyr
#' @import ggplot2
#' @import data.table

CheckBaseline <- function(raw.DF, matched.DF = NULL, treatment, variables = NULL) {

  # Construct baseformulas for balancing tests
  if(missing(variables)==TRUE) {
    nums <- sapply(raw.DF, is.numeric)
    variables <- names(raw.DF[,nums])
  }
  baseformulas <- ""

  for(i in 1:length(variables)) {
    baseformulas[i] <- paste0(variables[i], " ~ ", treatment)
  }
  baseformulas <- baseformulas[which(baseformulas!=paste0(treatment, " ~ ", treatment))]
  #print(baseformulas)

  # Balancing test on non-matched data
  myttest0 <- lapply(baseformulas, stdbias, data = raw.DF)
  myttest0 <- rbindlist(myttest0)
  non_matched.tb <- myttest0[,1:2, with = FALSE]
  non_matched.tb[,Matching:="None"]
  #print(non_matched.tb)

  # Balancing test on matched data
  if(missing(matched.DF)==FALSE){
    myttest <- lapply(baseformulas, stdbias, data = matched.DF)
    myttest <- rbindlist(myttest)
    matched.tb <- myttest[,1:2, with = FALSE]
    matched.tb[,Matching:="Matched"]
    #print(matched.tb)

    # Stacking matched and not matched tables
    combined.tb <- rbind(non_matched.tb, matched.tb)
    #print(combined.tb)

    # PS distribution
    mypsplot <- psdplot(matched.DF, treatment)


    # Std Bias with arrows
    mysbplot.2 <- sbplot_wm(combined.tb)
  } #<-end if missing(matched.DF)

  # Std Bias
  mysbplot.1 <- sbplot_nm(plot.df = non_matched.tb)

  # Output
  if(missing(matched.DF)==TRUE) {
    output <- list(baseline.plot = mysbplot.1)
  } else {
    output <- list(propensity.plot = mypsplot,
                   baseline.plot = mysbplot.2)
  }
  return(output)

}
