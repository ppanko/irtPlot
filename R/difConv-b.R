crtFrmu <- function(x, ind = inds) {
    dt <- list()
    dt$a <- rep(x[ind+1], ind)
    dt$b <- x[1:ind]
    dt$c <- rep(0, ind)
    return(dt)
}

crtFrmn <- function(x, ind = inds) {
    dt <- list()
    dt$a <- x[(ind+1):length(x)]
    dt$b <- x[1:ind]
    dt$c <- rep(0, ind)
    return(dt)
}

calcD <- function(dat, tht = theta) {
    iprb <- list()
    for(i in 1:length(dat[[1]])) {
        iprb[[i]] <- dat$c[i] + (1 - dat$c[i])*((exp(1.7*dat$a[i]*(tht-dat$b[i])))/(1+(exp(1.7*dat$a[i]*(tht-dat$b[i])))))
    }
    return(iprb)
}

dbind <- function(x) {
        itms <- data.frame(prb = unlist(x),  grp = rep(names(hi2), each = length(theta)), tht = theta)
}

plotDif <- function(itms, ttl, x1 = itms$tht, y1 = itms$prb, grp, ylbs = ylb, lgd = "Group") {

    ggplot2::ggplot(itms, aes(x = x1, y = y1)) +
        geom_line(aes(color = grp), size = 1) +
        ggtitle(paste(ttl, '\n')) +
        xlab(expression(atop(,Ability(theta)))) +
        scale_x_continuous(breaks = seq(min(x1), max(x1), 1)) +
        ylab(ylbs) +
        theme(axis.title = element_text(size = 14, face = "italic"), title = element_text(size = 15, face = "bold")) +
        guides(color=guide_legend(title=lgd))
}
