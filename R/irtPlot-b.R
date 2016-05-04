#' Plot binary Item Response Theory models.
#'
#' \code{irtPlot} returns a plot within a new device and saves to chosen directory.
#'
#' This function is capable of generating several models and plot types.
#' Namely, the models are \code{"1PL"},\code{"2PL"}, and \code{"3PL"} and the possible types are
#' \code{"icc"}, \code{"tcc"}, \code{"iif"}, \code{"tif"}, \code{"likl"}, and \code{"logl"}
#'
#' @param dat A data frame containing at least one item as a column.
#' @param theta The ability parameter across which to plot response parameterization.
#' @param model The desired model to fit to the data.
#' @param type The plot type to generate. See Details for a list of valid types.
#' @param save Whether or not the generated plots will be saved. Default is \code{"FALSE"}
#'
#'
#'
#' @return Prints the specified plot by default leaving the options to save
#' 	 to the user.
#' @examples
#' ## Load data:
#' library(ltm)
#' data <- Science[,c(1,3)]
#' colnames(data) <- paste0("Item ", 1:ncol(data))
#'

irtPlot <- function(dat,
                    theta = seq(-3, 3, 0.001),
                    title = NULL,
                    ddir = getwd(),
                    save = FALSE,
                    model,
                    type,
                    filename = paste0(model, "_", type, "_", ".jpg"),
                    dpi = 300,
                    height = 8.5,
                    width = 10,
                    itmNam = colnames(dat),
                    subS = NULL,
                    silent = FALSE) {

    ind <- ncol(dat)
    ylb <- expression(atop(P(theta),))
    item = factor(rep(itmNam, each = length(theta)))
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
    if(type == "icc"|type == "tcc") {

        prb <- apply(dt, 1, calcP)

        if(type == "tcc") {

            ylb <- "Expected Score \n"
            itms <- data.frame(prb = rowSums(prb), theta = theta)
            itmplot <- plotSin(itms, ttl = title2, x1 = itms$theta, y1 = itms$prb, ylbs = ylb)

        } else if(type == "icc") {

            prv <- as.vector(prb)
            itms <- data.frame(prb = prv, theta = rep(theta, ind),item = item)
            itmplot <- plotIrt(itms, ttl = title2, x1 = itms$theta, y1 = itms$prb, grp = itms$item, ylbs = ylb)

        }
                                        #type iif
    } else if(type == "iif"|type == "tif") {

        inf <- apply(dt, 1, calcI)
        ylb <- expression(atop(Info(theta),))

        if(type == "tif") {

            itms <- data.frame(inf = rowSums(inf), theta = theta)
            itmplot <- plotSin(itms, ttl = title2, x1 = itms$theta, y1 = itms$inf, ylbs = ylb)

        } else if(type == "iif") {

            inv <- as.vector(inf)
            itms <- data.frame(inf = inv, theta = rep(theta, ind),item = item)
            itmplot <- plotIrt(itms, ttl = title2, x1 = itms$theta, y1 = itms$inf, grp = itms$item, ylbs = ylb)

        }
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

        itmplot <- plotIrt(itms, title2, x1 = itms$theta, y1 = y1, grp = itms$Obs, ylbs = ylb, lgd = "Obs.")

    } else stop("Please provide a valid plot type, comrade")

    if(save == TRUE) ggplot2::ggsave(itmplot, file = paste0(ddir,"/",filename), dpi = dpi, height = height, width = width)

    if(silent == FALSE) print(itmplot)

}

