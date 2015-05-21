#' Convert price data exported from Meta Trader 4 to xts
#'
#' This function takes a data frame that was exported from Meta Trader 4
#' as input and converts it to a multivariate xts time series.
#' @param MTdata A data frame that was created using the csv-file that was
#' exported by Meta Trader 4 with the columns "Date", "Time", "O", "H", "L",
#' "C" and "Vol".
#' @keywords xts
#' @export
#' @examples
#' convertMTdataToXts()

convertMTdataToXts <- function(MTdata){
      require(xts)
      colnames(MTdata) <- c("Date", "Time", "O", "H", "L", "C", "Vol")
      MTdata$Date <- as.Date(as.character(MTdata$Date), format = "%Y.%m.%d")
      datetime <- paste(MTdata$Date, MTdata$Time)
      datetime <- as.POSIXlt(datetime, format = "%Y-%m-%d %H:%M")
      MTdata$Datetime <- datetime; rm(datetime)
      # The xts data frame must not contain the character columns
      return(xts(MTdata[, c("O", "H", "L", "C", "Vol")], order.by = MTdata$Datetime))
}
