l2p <- function(l, scale=100)
{
    rval <- NULL
    for (letter in l) {
        if (scale == 100) {                 # BUG: should permit scheme; should base on p2l to be DRY
            if      (length(grep("^A\\+$", letter, ignore.case=TRUE))>0) rval <- c(rval, 0.5*(90+100))
            else if (length(grep("^A$",    letter, ignore.case=TRUE))>0) rval <- c(rval, 0.5*(85+90))
            else if (length(grep("^A\\-$", letter, ignore.case=TRUE))>0) rval <- c(rval, 0.5*(80+85))
            else if (length(grep("^B\\+$", letter, ignore.case=TRUE))>0) rval <- c(rval, 0.5*(75+80))
            else if (length(grep("^B$",    letter, ignore.case=TRUE))>0) rval <- c(rval, 0.5*(70+75))
            else if (length(grep("^B\\-$", letter, ignore.case=TRUE))>0) rval <- c(rval, 0.5*(65+70))
            else if (length(grep("^C\\+$", letter, ignore.case=TRUE))>0) rval <- c(rval, 0.5*(62+65))
            else if (length(grep("^C$",    letter, ignore.case=TRUE))>0) rval <- c(rval, 0.5*(58+62))
            else if (length(grep("^C\\-$", letter, ignore.case=TRUE))>0) rval <- c(rval, 0.5*(55+58))
            else if (length(grep("^D$",    letter, ignore.case=TRUE))>0) rval <- c(rval, 0.5*(50+55))
            else if (length(grep("^F$",    letter, ignore.case=TRUE))>0) rval <- c(rval, 0.5*( 0+50))
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
