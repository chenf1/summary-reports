#' summary table of numeric variables
#'
#' @param report_dt data frame.
#' @return the statistics nubmers of numberic varibles in input data frame.
#' @examples
#' summ_numeric(data.frame(c1 = c(1,2,4,6,7),c2=c("a","v","b","t","e")))

summ_numeric <- function(report_dt){
  #check numeric vars summary
  res <- lapply(report_dt[sapply(report_dt, is.numeric)], function(x) rbind( mean = mean(x,na.rm = TRUE) ,
                                                                             sd = sd(x,na.rm = TRUE) ,
                                                                             median = median(x,na.rm = TRUE) ,
                                                                             minimum = min(x,na.rm = TRUE) ,
                                                                             maximum = max(x,na.rm = TRUE) ,
                                                                             N = length(x),
                                                                             Missing = sum(is.na(x))
  ))
  return(res)
}

