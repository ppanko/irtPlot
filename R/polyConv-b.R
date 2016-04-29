namNamp <- function(x, t) {
    if(t == "icc") title <- paste0("Cumulative Response Curves \n for the ", x, " Item")
    else if(t == "iif") title <- paste0("Response Probabilities \n for the ", x, " Item")
}

pFun <- function(x, l) {

    obj <- list()
    t <- length(l)
    for (i in 1:(t-1)) {

        obj[[i]] <- data.frame(prb = (exp(1.7*x[t,]*(theta - x[i,])))/(1 + exp(1.7*x[t,]*(theta - x[i,]))),
                               level = l[i],
                               tht = theta)
    }

    return(obj)
}

plotPoly <- function(itms, ttl, x1 = itms$tht, y1 = itms$prb, group = factor(itms$level), ylbs, lgd = "Response") {

    ggplot2::ggplot(itms, aes(x = x1, y = y1)) +
        geom_line(aes(color = group), size = 1) +
        ggtitle(paste(ttl, '\n')) +
        xlab(expression(atop(,Ability(theta)))) +
        scale_x_continuous(breaks = seq(min(x1), max(x1), 1)) +
        ylab(ylbs) +
        theme(axis.title = element_text(size = 14, face = "italic"), title = element_text(size = 15, face = "bold")) +
        guides(color=guide_legend(title=lgd))
}

prints <- function(x) {
    par(ask = TRUE)
    x
}

gSave <- function(x, flnm, dDir, res, hgt, wdt) {
    ggplot2::ggsave(filename = paste0(dDir, "/", flnm), plot = x, dpi = res, height = hgt, width = wdt)
}

cmbFun <- function(x, l) {
    k <- length(l)
    p <- rep(list(list()), k)
    for(i in 1:k) {
        if(i == 1) {
            p[[i]] <- rep(0, length(theta))
        } else if(i == 2) {
            p[[i]] <- x[k] * 1.7*(theta - x[i-1])
        } else if(i > 1) {
            p[[i]] <- p[[i - 1]] + 1.7 * x[k] * (theta - x[i-1])
        }

    }
    return(p)
}

rpbFun <- function(x) {
    z <- list()
    for(i in 1:(length(x)-1)) {
        z[[i]] <- x[[i]]/x[[length(x)]]
    }
    return(z)
}

prbFun <- function(x) {
    o <- list()
    for(i in 2:length(x)) {
        o[[i-1]] <- x[[i]]/(x[[i]] + x[[i-1]])
    }
    return(o)
}

mbind <- function(x, l) data.frame(prb = do.call("c", x), level = l, tht = theta)

respFun <- function(x) {
    lst <- list()
    for(i in 1:(length(x))) {
        if(i == 1) lst[[i]] <- 1 - x[[i]]$prb
        if(i == 1) lst[[i+1]] <- x[[i]]$prb - x[[i + 1]]$prb
        else if (i == length(x)) lst[[i+1]] <- x[[i]]$prb
        else lst[[i+1]] <- x[[i]]$prb - x[[i + 1]]$prb
    }
    return(unlist(lst))
}

cbmFun <- function(x,y = theta,z) {
            w <- data.frame(prb = x, tht = y, level = rep(z, each = length(y)))
        }
