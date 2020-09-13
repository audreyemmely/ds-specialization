library("data.table")

outcome <- data.table::fread('outcome-of-care-measures.csv')
outcome[,(11) := lapply(.SD, as.numeric), .SDcols = (11)]
outcome[, lapply(.SD, hist, xlab="Deaths", main="30-day mortality rates for heart attack", col="gray"), .SDcols = (11)]