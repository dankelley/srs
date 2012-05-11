p2l <- function(p, scheme="A")
{
    rval <- NULL
    np <- length(p)
    if (scheme == "A") {
        for (i in 1:np) {
            if      (p[i] >= 90)	rval<-c(rval,"A+")
            else if (p[i] >= 85)	rval<-c(rval,"A" )
            else if (p[i] >= 80)	rval<-c(rval,"A-")
            else if (p[i] >= 75)	rval<-c(rval,"B+")
            else if (p[i] >= 70)	rval<-c(rval,"B" )
            else if (p[i] >= 65)	rval<-c(rval,"B-")
            else if (p[i] >= 62)	rval<-c(rval,"C+")
            else if (p[i] >= 58)	rval<-c(rval,"C" )
            else if (p[i] >= 55)	rval<-c(rval,"C-")
            else if (p[i] >= 50)	rval<-c(rval,"D")
            else                    rval<-c(rval,"F" )
        }
    } else if (scheme == "B") {
        for (i in 1:np) {
            if      (p[i] >= 90)	rval<-c(rval,"A+")
            else if (p[i] >= 85)	rval<-c(rval,"A" )
            else if (p[i] >= 80)	rval<-c(rval,"A-")
            else if (p[i] >= 75)	rval<-c(rval,"B+")
            else if (p[i] >= 70)	rval<-c(rval,"B" )
            else if (p[i] >= 65)	rval<-c(rval,"B-")
            else                        rval<-c(rval,"F" )
        }
    } else {
        stop("Unknown scheme", scheme)
    }
    rval;
}
