library(grid)
plot.srs <- function(x, gleft=1.7, skip.presentations=TRUE, skip.inprep=TRUE, skip.communications=FALSE, jitter=FALSE, colours=c("blue"), quiet=TRUE, ...)
{
    if (!inherits(x, "srs"))
        stop("method is only for srs objects")
    cex <- par("cex")
    grid.newpage()
    n <- length(x)
                                        # Underlay in grade area
    g <- c(0, 0.7, 1.0, 1.3,  1.7, 2.0, 2.3, 2.7, 3.0, 3.3, 3.7, 4.0, 4.3)
    l <- c("F", "D-","D", "D+", "C-", "C","C+", "B-","B","B+","A-","A","A+")
    g.use <- g >= gleft
    g <- g[g.use]
    l <- l[g.use]
    ys <- c(length(x)+0.25, 1-0.26)
    lvp<-viewport(x=0.0,just="left",width=0.3,height=.8,xscale=c(0.0,1.0),yscale=ys)
    rvp<-viewport(x=0.3,just="left",width=0.7,height=.8,xscale=c(gleft,4.5),yscale=ys)
    pushViewport(rvp)
    glen <- length(g)
    for (i in 1:glen) {
        if (g[i] < 3.7)	grid.lines(c(g[i],g[i]),ys,gp=gpar(col="darkgray", lwd=2),default.units="native")
        else            grid.lines(c(g[i],g[i]),ys,gp=gpar(col="darkgray",lwd=2),default.units="native")
        grid.text(l[i],g[i],ys[1]+.1,default.units="native",gp=gpar(cex=cex)) # 1.5
        grid.text(l[i],g[i],ys[2]-.1,default.units="native",gp=gpar(cex=cex)) # 1.5
    }
    for (i in 1:n)
        grid.lines(c(g[1],g[glen]),i,default.units="native",gp=gpar(col="darkgray",lwd=3))
    upViewport()

    haveAlpha <- !is.null(getOption('device')) && "X11" != names(dev.cur())[1]

    for (i in 1:n) {                    # Student by student
        pushViewport(lvp)
        y <- i
        ## Name, grades, scholarships, prizes, communications
        nc <- length(x[[i]]$grades$course)

        ##grid.text(x[[i]]$name, x=0.9, y=y, gp=gpar(cex=1.4, col=colours[1]), default.units="native", just="right")

        mip <- if (is.na(x[[i]]$enrolled))
            " ? mon total"
        else
            sprintf(" %.0f mon total", difftime(Sys.Date(), as.POSIXct(x[[i]]$enrolled), "days")/30)

        info <- paste(x[[i]]$name, "\n",
                      if (nchar(x[[i]]$program) > 0) x[[i]]$program else "",
                      mip, "\n",
                      sprintf("%.2f %% (= %.2f / %.0f)",
                              mean(x[[i]]$grades$point), sum(x[[i]]$grades$point), nc), sep="")
        ##info <- paste(info, "\n")
                                        # Scholarships
        s <- x[[i]]$scholarships
        ls <- length(s)
        if (ls > 0) for (is in 1:ls) info <- paste(info,"\n",s[is],sep="")
        ##info <- paste(info, "\n")
                                        # Prizes
        p <- x[[i]]$prizes
        lp <- length(p)
        if (lp > 0) for (ip in 1:lp) info <- paste(info,"\n",p[ip],sep="")
        ##info <- paste(info, "\n")
                                        # Presentations
        if (skip.presentations) {
            if (skip.inprep) communications <- sprintf("%d pub/acc + %d sub",
                                                       length(x[[i]]$papers.published)+
                                                       length(x[[i]]$papers.inpress),
                                                       length(x[[i]]$papers.submitted))
            else communications <- sprintf("%d pub/acc + %d sub + %d inprep",
                                           length(x[[i]]$papers.published)+
                                           length(x[[i]]$papers.submitted),
                                           length(x[[i]]$papers.inprep))
        } else {
            if (skip.inprep) communications <- sprintf("%d pub/acc + %d sub\n%d presentations",
                                                       length(x[[i]]$papers.published)+
                                                       length(x[[i]]$papers.inpress),
                                                       length(x[[i]]$papers.submitted),
                                                       length(x[[i]]$presentations))
            else communications <- sprintf("%d pub/acc + %d sub + %d inprep\n%d presentations",
                                           length(x[[i]]$papers.published)+
                                           length(x[[i]]$papers.inpress),
                                           length(x[[i]]$papers.submitted),
                                           length(x[[i]]$papers.inprep),
                                           length(x[[i]]$presentations))
        }
        ##	grid.text(info,x=0.9,y=y,default.units="native",just="right")
        if (!skip.communications)
            info <- paste(info,"\n", communications)
        grid.text(info, x=0.9, y=y, default.units="native", just="right",
                  gp=gpar(col="black",cex=cex)) # 1.2

        ##	cat(paste("y     is ",y,"\n"))
        ##	y.pt <- convertY(unit(y,"native"),"points")
        ##	sh.pt <- convertY(unit(1,"strheight","M"),"points")
        ##	cat(paste("stringheight/pt ",sh.pt,"\n"))
        ##	sh.pt <- convertY(unit(1,"strheight",info),"points")
        ##	cat(paste("stringheight/pt ",sh.pt,"\n"))
        ##	cat(paste("y /pt  is ",y.pt,"\n"))
        ##	cat(paste("y-/pt  is ",y.pt-sh.pt,"\n"))
        ##	yshift <- convertY(convertY(y.pt - sh.pt,"points"),"native")
        ##	cat(paste("y-/nat is ",yshift,"\n"))
        ##	grid.text(communications, x=0.9, y=yshift, default.units="native", just="right")
        ##	grid.text(communications, x=0.9, y=yshift, default.units="native", just="right")
        upViewport()

        ## Dots
        pushViewport(rvp)
        if (jitter) {
            xg <- jitter(x[[i]]$grades$point, amount=0.03)
            yg <- jitter(rep(y, length(xg)),amount=0.4/n)
            #grid.points(xg,yg,pch=21,gp=gpar(fill="yellow",col="red",cex=1.2*cex, lwd=3))
            grid.points(xg,yg,gp=gpar(col="red",cex=1.2*cex, lwd=3))
        } else {
            for (grade in g) {
                w <- x[[i]]$grades$point == grade
                ng <- sum(w)
                if (ng > 0) {
                    yy <- y + if (ng ==1) 0 else  seq(-0.1, 0.1, length.out=ng)
                    grid.points(x[[i]]$grades$point[w],yy,pch=21,
                                gp=gpar(fill="yellow",col="red", cex=1.2*cex, lwd=3))
                }
            }
        }
        ## cross
        mean <- mean(x[[i]]$grades$point)
        grid.lines(c(mean,mean),c(i-0.5/n,i+0.5/n),default.units="native", 
                   gp=gpar(col="blue", alpha=if (haveAlpha) 0.6 else 1, lwd=6))
        if (!quiet) print(i-0.5/n)
        tt <- try(t.test(x[[i]]$grades$point), silent=TRUE)
        if (class(tt) != "try-error") {
            ci <- tt$conf.int
            ci[2] <- min(ci[2], 4.3)
            grid.lines(ci, rep(i,2),default.units="native",
                       gp=gpar(col="blue", alpha=if (haveAlpha) 0.6 else 1, lwd=6))
        }
        upViewport()
    }
}
