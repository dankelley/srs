l2p <- function(l, scale=4.3)
{
    rval <- NULL
    for (letter in l) {
        if (scale == 100) {                 # BUG: should permit scheme; should base on p2l to be DRY
            if (length(grep("^A\\+$", letter, ignore.case=TRUE))>0) rval <- c(rval, 97)
            else if (length(grep("^A$",    letter, ignore.case=TRUE))>0) rval <- c(rval, 92)
            else if (length(grep("^A\\-$", letter, ignore.case=TRUE))>0) rval <- c(rval, 85)
            else if (length(grep("^B\\+$", letter, ignore.case=TRUE))>0) rval <- c(rval, 79)
            else if (length(grep("^B$",    letter, ignore.case=TRUE))>0) rval <- c(rval, 74)
            else if (length(grep("^B\\-$", letter, ignore.case=TRUE))>0) rval <- c(rval, 69)
            else if (length(grep("^C\\+$", letter, ignore.case=TRUE))>0) rval <- c(rval, 65)
            else if (length(grep("^C$",    letter, ignore.case=TRUE))>0) rval <- c(rval, 61)
            else if (length(grep("^C\\-$", letter, ignore.case=TRUE))>0) rval <- c(rval, 57)
            else if (length(grep("^D\\+$", letter, ignore.case=TRUE))>0) rval <- c(rval, 54)
            else if (length(grep("^D$",    letter, ignore.case=TRUE))>0) rval <- c(rval, 52)
            else if (length(grep("^D\\-$", letter, ignore.case=TRUE))>0) rval <- c(rval, 50)
            else if (length(grep("^F$",    letter, ignore.case=TRUE))>0) rval <- c(rval, 25)
            else stop("Unknown letter grade '",letter,"'")
        } else {
            if (length(grep("^A\\+$", letter, ignore.case=TRUE))>0) rval <- c(rval, scale*4.3/4.3)
            else if (length(grep("^A$",    letter, ignore.case=TRUE))>0) rval <- c(rval, scale*4.0/4.3)
            else if (length(grep("^A\\-$", letter, ignore.case=TRUE))>0) rval <- c(rval, scale*3.7/4.3)
            else if (length(grep("^B\\+$", letter, ignore.case=TRUE))>0) rval <- c(rval, scale*3.3/4.3)
            else if (length(grep("^B$",    letter, ignore.case=TRUE))>0) rval <- c(rval, scale*3.0/4.3)
            else if (length(grep("^B\\-$", letter, ignore.case=TRUE))>0) rval <- c(rval, scale*2.7/4.3)
            else if (length(grep("^C\\+$", letter, ignore.case=TRUE))>0) rval <- c(rval, scale*2.3/4.3)
            else if (length(grep("^C$",    letter, ignore.case=TRUE))>0) rval <- c(rval, scale*2.0/4.3)
            else if (length(grep("^C\\-$", letter, ignore.case=TRUE))>0) rval <- c(rval, scale*1.7/4.3)
            else if (length(grep("^D\\+$", letter, ignore.case=TRUE))>0) rval <- c(rval, scale*1.3/4.3)
            else if (length(grep("^D$",    letter, ignore.case=TRUE))>0) rval <- c(rval, scale*1.0/4.3)
            else if (length(grep("^D\\-$", letter, ignore.case=TRUE))>0) rval <- c(rval, scale*0.7/4.3)
            else if (length(grep("^F$",    letter, ignore.case=TRUE))>0) rval <- c(rval, scale*0.0/4.3)
            else stop("Unknown letter grade '",letter,"'")
        }
    }
    rval
}
