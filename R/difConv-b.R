namNamd <- function(x, t) {
    if(t == "icc") { title <- sapply(x, function(z) paste0("Item Characteristic Functions \n for the ", z, " Item Between Groups"))
} else if (t == "lmr") title <- paste0("DIF Statistics \n for the ", length(x), " Items")
}

crtFrmu <- function(x, ind) {
    dt <- list()
    dt$a <- rep(x[ind+1], ind)
    dt$b <- x[1:ind]
    dt$c <- rep(0, ind)
    return(dt)
}

crtFrmn <- function(x, ind) {
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

dbind <- function(x, g) {
        itms <- data.frame(prb = unlist(x),  grp = rep(levels(g), each = length(theta)), tht = theta)
}

plotDif <- function(itms, ttl, x1 = itms$tht, y1 = itms$prb, grp, ylbs, lgd = "Group") {

    ggplot2::ggplot(itms, aes(x = x1, y = y1)) +
        geom_line(aes(color = grp), size = 1) +
        ggtitle(paste(ttl, '\n')) +
        xlab(expression(atop(,Ability(theta)))) +
        scale_x_continuous(breaks = seq(min(x1), max(x1), 1)) +
        ylab(ylbs) +
        theme(axis.title = element_text(size = 14, face = "italic"), title = element_text(size = 15, face = "bold")) +
        guides(color=guide_legend(title=lgd))
}

plotLMR <- function(itms, ttl, x1 = itms$itm, y1 = itms$lr, group = itms$mth, color = itms$mth, ylbs, lgd = "Method", nm, thrs) {

    ggplot2::ggplot(itms, aes(x = x1, y = y1)) +
        geom_point(aes(color = group), size = 2) +
        geom_line(aes(color = group), size = 1) +
        geom_hline(colour = "#F8766D", yintercept = thrs[1], linetype = "dotted", size = 1.3) +
        geom_hline(colour = "#00BA38", yintercept = thrs[2], linetype = "dotted", size = 1.3) +
        geom_hline(colour = "#619CFF", yintercept = thrs[3], linetype = "dotted", size = 1.3) +
        ggtitle(paste(ttl, '\n')) +
        xlab("\n Items") +
        scale_x_continuous(breaks=1:length(nm), labels = nm) +
        ylab(ylbs) +
        theme(axis.title = element_text(size = 14, face = "italic"), title = element_text(size = 15, face = "bold")) +
        guides(color=guide_legend(title=lgd))
}
