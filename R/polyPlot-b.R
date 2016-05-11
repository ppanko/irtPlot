#' Plot Polytomous Item Response Theory models.
#'
#' \code{polyPlot} returns a plot within a new device and saves to chosen directory.
#'
#' This function specializes in generating polytomous models and plot types.
#' Namely, the models are \code{"grm"} and \code{"pcm"} and the possible types are
#' \code{"icc"}, \code{"icf"}, and \code{"crp"}.
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
#' ## Example notation:
#' library(ltm)
#' data <- Science[,c(1,3)]
#' colnames(data) <- paste0("Item ", 1:ncol(data))
#' polyPlot(data, theta, model = "grm", type = "crp")

polyPlot <- function(dat,
                     theta = seq(-3, 3, 0.01),
                     title = NULL,
                     ddir = getwd(),
                     save = FALSE,
                     model,
                     type,
                     fln = paste0(model, "_", type, "_", colnames(dat), ".jpg"),
                     dpi = 300,
                     height = 8.5,
                     width = 10,
                     itmNam = colnames(dat),
                     silent = FALSE)

{

    ylb <- expression(atop(P[x](theta),))

    ld <- lapply(dat, function(x) factor(levels(x), levels = levels(x)))

    if(length(title) < 1) title2 <- sapply(itmNam, namNamp, t = type)

    if(model == "grm") {

        cf <- summary(ltm::grm(dat))$coefficients
        prb <- mapply(pFun, x = cf, l = ld, SIMPLIFY = FALSE)

        if(type == "crp") {

            pdt <- lapply(prb, function(x) do.call(rbind.data.frame, x))

            for(i in 1:length(pdt)) {
                pdt[[i]]$level <- rep(paste0("Y > ", 1:(length(levels(pdt[[i]]$level))-1)), each = length(theta))
            }

            plPlot <- list()
            for(i in 1:length(pdt)) plPlot[[i]] <- plotPoly(pdt[[i]], ttl = title2[i], ylbs = ylb)

        } else if(type == "icc"|type == "icf") {

            lst <- lapply(prb, respFun)
            pdt <- mapply(cbmFun, x = lst, z = ld, SIMPLIFY = FALSE)

            if(type == "icf") {

                ylb <- "Expected Score"
                spd <- lapply(pdt, function(x) do.call("cbind", split(x$prb, x$level)))
                inx <- lapply(ld, function(x) 1:length(x))

                mt1 <- mapply(mult, x = inx, y = spd, SIMPLIFY = FALSE)
                mt2 <- lapply(mt1, function(x) Reduce("+", x))
                mt3 <- lapply(mt2, cbmFunc)

                plPlot <- list()
                for(i in 1:length(mt3)) plPlot[[i]] <- plotCurv(mt3[[i]], ttl = title2[i], ylbs = ylb)

            } else if (type == "icc") {

                plPlot <- list()
                for(i in 1:length(pdt)) plPlot[[i]] <- plotPoly(pdt[[i]], ttl = title2[i], ylbs = ylb)

            }

        } else stop("Please provide a valid plot type, comrade")

    } else if(model == "pcm") {

        md <- summary(ltm::gpcm(dat))
        cf <- lapply(md$coefficients, function(x) x[,1])
        pdt <- mapply(cmbFun, x = cf, l = ld, SIMPLIFY = FALSE)
        pdt1 <- lapply(pdt, function(x) lapply(x, exp))

        for (i in 1:length(pdt1)) {

            pdt1[[i]][[length(ld[[i]])+1]] <- Reduce("+", pdt1[[i]])
        }

        rdt <- lapply(pdt1, rpbFun)

        if(type == "icc"|type == "icf") {

            if(type == "icf") {

                ylb <- "Expected Score \n"
                spd <- lapply(rdt, function(x) do.call("cbind", x))
                inx <- lapply(ld, function(x) 1:length(x))

                mt1 <- mapply(mult, x = inx, y = spd, SIMPLIFY = FALSE)
                mt2 <- lapply(mt1, function(x) Reduce("+", x))
                mt3 <- lapply(mt2, cbmFunc)

                plPlot <- list()
                for(i in 1:length(mt3)) plPlot[[i]] <- plotCurv(mt3[[i]], ttl = title2[i], ylbs = ylb)

            } else if (type == "icc") {

                lds <- list()
                for(i in 1:length(ld)) lds[[i]] <- rep(ld[[i]], each = length(theta))

                rdt1 <- mapply(mbind, x = rdt, l = lds, SIMPLIFY = FALSE)

                plPlot <- list()
                for(i in 1:length(pdt)) plPlot[[i]] <- plotPoly(rdt1[[i]], ttl = title2[i], ylbs = ylb)

            }

        } else if (type == "crp") {

            odt <- lapply(rdt, prbFun)
            ld <- lapply(dat, function(x) paste0("Y > ", 1:(length(levels(x))-1)))

            lds <- list()
            for(i in 1:length(ld)) lds[[i]] <- rep(ld[[i]], each = length(theta))

            odt1 <- mapply(mbind, x = odt, l = lds, SIMPLIFY = FALSE)

            plPlot <- list()
            for(i in 1:length(pdt)) plPlot[[i]] <- plotPoly(odt1[[i]], ttl = title2[i], ylbs = ylb)

        } else stop("Please provide a valid plot type, comrade")
    }

    if(save == TRUE) mapply(gSave, x = plPlot , flnm = fln, dDir = ddir, res = dpi, hgt = height, wdt = width)

    if(silent == FALSE) print(lapply(plPlot, prints))

}
