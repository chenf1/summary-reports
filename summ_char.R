#' summary table of character variables
#'
#' @param report_dt data frame.
#' @return the one way frequency tables of character varibles in input data frame.
#' @examples
#' summ_char(data.frame(c1 = c(1,2,4,6,7),c2=c("a","v","b","a","e")))

summ_char <- function(report_dt){
  csumm <- lapply((report_dt[sapply(report_dt, is.character)]), function(x) table(x, useNA = 'ifany'))

  demo_lst <- list()

  for(i in c(1:length(csumm))){
    demo_lst[[i]] <- data.frame(csumm[i])
    demo_lst[[i]]$perct <- 100*demo_lst[[i]][,2]/sum(demo_lst[[i]][,2])
    demo_lst[[i]][,1]<-as.character(demo_lst[[i]][,1])
    demo_lst[[i]] <- rbind(demo_lst[[i]],c('Total', sum(demo_lst[[i]][,2]), sum(demo_lst[[i]][,3])))

  }
  return(demo_lst)
}
