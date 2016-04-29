
irtPlot <- function(dat,
                    theta,
                    title = NULL,
                    ddir = getwd(),
                    save = FALSE,
                    model,
                    type,
                    filename = paste0(model, "_", type, "_", ".jpg"),
                    dpi = 800,
                    height = 6.5,
                    width = 10,
                    itmNam = colnames(dat),
                    subS = NULL) {

    ind <- ncol(dat)
    ylb <- expression(atop(P(theta),))
    item = factor(rep(itmNam, each = length(theta))) #,
    levels(item) <- itmNam

    if(length(title) < 1) title2 <- namNami(t = type, i = ind)
    else title2 <- title

    dt <- data.frame(matrix(ncol = 3, nrow = ind))
    colnames(dt) <- c("a", "b", "c")

    if(model == "1PL") {

        out <- summary(ltm::rasch(dat))$coefficients[,1]

        dt$a <- rep(out[ind+1], ind)
        dt$b <- out[1:ind]
        dt$c <- rep(0, ind)

    }  else if(model == "2PL") {

        out <- summary(ltm::ltm(dat ~ z1))$coefficients[,1]

        dt$a <- out[(ind+1):length(out)]
        dt$b <- out[1:ind]
        dt$c <- rep(0, ind)

    }  else if (model == "3PL") {

        out <- summary(ltm::tpm(dat, max.guessing=1))$coefficients[,1]

        dt$a <- out[(ind+ind+1):length(out)]
        dt$b <- out[(ind+1):(ind+ind)]
        dt$c <- out[1:ind]

    } else stop("Please provide a valid model, comrade")

                                        #type icc
    if(type == "icc") {

        prb <- as.vector(apply(dt, 1, calcP))
        itms <- data.frame(prb = prb, theta = rep(theta, ind),item = item)
        itmplot <- plotThings(itms, ttl = title2, x1 = itms$theta, y1 = itms$prb, grp = itms$item, ylbs = ylb)

                                        #type iif
    } else if(type == "iif") {

        inf <- as.vector(apply(dt, 1, calcI))
        itms <- data.frame(inf = inf, theta = rep(theta, ind),item = item)
        itmplot <- plotThings(itms, ttl = title2, x1 = itms$theta, y1 = itms$inf, grp = itms$item, ylbs = ylb)

                                        #type logl/likl
    } else if(type == "likl"|type == "logl") {

        if(length(subS) > 0) dat <- subS

        if(length(title2) < 1) title2 <- namNaml(t = type, i = nrow(dat))

        vl <- apply(dat, 1, logLik, cf = dt, t = theta)
        itms <- data.frame(likl = c(vl),
                           logl = log(c(vl)),
                           Obs = factor(rep(1:nrow(dat), each = length(theta))),
                           theta = theta)

        if(type == "likl") {

            y1 <- itms$likl
            ylb <- "Likelihood \n"

        } else {

            y1 <- itms$logl
            ylb <- "Log-likelihood \n"
        }

        itmplot <- plotThings(itms, title2, x1 = itms$theta, y1 = y1, grp = itms$Obs, ylbs = ylb, lgd = "Obs.")

    } else stop("Please provide a valid plot type, comrade")

    if (save == TRUE) ggplot2::ggsave(itmplot, file = paste0(ddir,"/",filename), dpi = dpi, height = height, width = width)

    print(itmplot)

}

