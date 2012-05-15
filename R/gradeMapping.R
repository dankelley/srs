gradeMapping <- function(scheme=c("A", "B"))
{
    scheme <- match.arg(scheme)
    if (scheme == "A") {
        rval <- list(letter    = c( "F",  "D",  "C-",  "C", "C+", "B-",  "B", "B+", "A-",  "A", "A+"), 
                     lowerLimit=c(    0,   50,    55,   58,   62,   65,   70,   75,   80,   85,   90))
    } else if (scheme == "B") {
        ##         F=FM    D    C-   C   C+   B-    B   B+   A-    A   A+
        ##center <- c(0, 1.0, 1.7, 2.0, 2.3, 2.7, 3.0, 3.3, 3.7, 4.0, 4.3)
        ##lowerLimit <- c(0, head(center+c(diff(center)/2, 0), -1))
        ##> lowerLimit
        ##[1] 0.00 0.50 1.35 1.85 2.15 2.50 2.85 3.15 3.50 3.85 4.15
        rval <- list(letter    = c( "F",  "D",  "C-",  "C", "C+", "B-",  "B", "B+", "A-",  "A", "A+"), 
                     lowerLimit= c(0.00, 0.50,  1.35, 1.85, 2.15, 2.50, 2.85, 3.15, 3.50, 3.85, 4.15))
    } else {
        stop("unkown scheme") # cannot get here
    }
    rval
}

