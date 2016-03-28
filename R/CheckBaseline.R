#' Check baseline equivalency
#'
#' @param raw.DF data frame you want to check.
#' @param matched.DF matched data frame. This one is optional.
#' @param treatment treatment variable
#' @param variables you want to check
#' @return Returns a list with the baseline equivalence plot and table.
#' Additionally, if matched.DF is provided, it returns the propensity score distibution plot.
#' @export
#' @import dplyr
#' @import ggplot2
#' @import data.table
#' @examples
#' data("fake.df")
#' data("fake.df.matched")
#' X <- CheckBaseline(
#'   raw.DF = fake.df,
#'     matched.DF = fake.df.matched,
#'       treatment = 'Treatment',
#'         variables = c('V2', 'V1', 'pre.test')
#'         )
#' X$baseline.plot
#' @section Vignette:
#' browseVignettes('checkbaseline')



CheckBaseline <- function(raw.DF, matched.DF = NULL, treatment, variables = NULL, names = NULL) {

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

    # Renaming--assuming the names option was invoked
    if(missing(names)==FALSE & missing(variables)==FALSE & length(variables)==length(names)) {
      for(j in 1:length(variables)) {
        for(i in 1:nrow(combined.tb)) {
          combined.tb$Name <- as.character(combined.tb$Name)
          if(combined.tb$Name[i]==variables[j]) {
             combined.tb$Name[i] <- names[j]
          combined.tb$sortorder[i] <- j
          }
        }
      }
    }


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
                   baseline.plot = mysbplot.2,
                   balance.tbl = combined.tb)
  }
  return(output)

}
