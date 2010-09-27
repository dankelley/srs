p2l <- function(p, scheme="A")
{
    rval <- NULL
    np <- length(p)
    if (scheme == "A") {
        for (i in 1:np) {
            if      (p[i] >= 95)	rval<-c(rval,"A+")
            else if (p[i] >= 90)	rval<-c(rval,"A" )
            else if (p[i] >= 82)	rval<-c(rval,"A-")
            else if (p[i] >= 77)	rval<-c(rval,"B+")
            else if (p[i] >= 72)	rval<-c(rval,"B" )
            else if (p[i] >= 67)	rval<-c(rval,"B-")
            else if (p[i] >= 64)	rval<-c(rval,"C+")
            else if (p[i] >= 60)	rval<-c(rval,"C" )
            else if (p[i] >= 56)	rval<-c(rval,"C-")
            else if (p[i] >= 50)	rval<-c(rval,"D")
            else                    rval<-c(rval,"F" )
        }
    } else if (scheme == "B") {
        for (i in 1:np) {
            if      (p[i] >= 90)	rval<-c(rval,"A+")
            else if (p[i] >= 85)	rval<-c(rval,"A" )
            else if (p[i] >= 90)	rval<-c(rval,"A-")
            else if (p[i] >= 77)	rval<-c(rval,"B+")
            else if (p[i] >= 73)	rval<-c(rval,"B" )
            else if (p[i] >= 70)	rval<-c(rval,"B-")
            else                        rval<-c(rval,"F" )
        }
    } else {
        stop("Unknown scheme", scheme)
    }
    rval;
}
