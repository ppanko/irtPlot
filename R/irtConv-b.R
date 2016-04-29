namNami <- function(t, i) {
    if(t == "icc") title <- paste0("Item Characteristic Curves \n for the ", i, " Items")
    else if(t == "iif") title <- paste0("Item Information Functions \n for the ", i, " Items")
}

namNaml <- function(t, i) {
    if(t == "likl") title <- paste0("Likelihood Estimates \n for the ", i, " Observations")
    else if(t == "logl") title <- paste0("Log-Likelihood Estimates \n for the ", i, " Observations")
}

#type icc calculation
calcP <- function(dat, theta = tht) {
    iprb <- dat["c"] + (1 - dat["c"])*((exp(1.7*dat["a"]*(theta-dat["b"])))/(1+(exp(1.7*dat["a"]*(theta-dat["b"])))))

    return(iprb)
}

                                        # type iif calculation
calcI <- function(dat, theta = tht) {
    inf <- (1.7^2*dat["a"]^2*(1 - dat["c"]))/((dat["c"]+exp(1.7*dat["a"]*(theta-dat["b"])))*(1+exp(-1.7*dat["a"]*(theta-dat["b"])))^2)

    return(inf)
}

logLik <- function(d, cf, t) {

    prb <- rep(NA, length(d))
    out <- rep(NA, length(t))

    for (i in 1:length(t)) {
        for(j in 1:length(d)) {

            if(d[j] == 0) {

                prb[j] <- 1 - (cf$c[j] + (1 - cf$c[j])*((exp(1.7*cf$a[j]*(t[i]-cf$b[j])))/(1+(exp(1.7*cf$a[j]*(t[i]-cf$b[j]))))))

            } else {

                prb[j] <- cf$c[j] + (1 - cf$c[j])*((exp(1.7*cf$a[j]*(t[i]-cf$b[j])))/(1+(exp(1.7*cf$a[j]*(t[i]-cf$b[j])))))

            }

        }

        out[i] <- prod(prb)
    }

    return(out)
}

## plots!
plotIrt <- function(itms, ttl, x1, y1, grp, ylbs, lgd = "Items") {

    ggplot2::ggplot(itms, aes(x = x1, y = y1)) +
        geom_line(aes(color = grp), size = 1) +
        ggtitle(paste(ttl, '\n')) +
        xlab(expression(atop(,theta))) +
        scale_x_continuous(breaks = seq(min(x1), max(x1), 1)) +
        ylab(ylbs) +
        theme(axis.title = element_text(size = 14, face = "italic"), title = element_text(size = 15, face = "bold")) +
        guides(color=guide_legend(title=lgd))
}

