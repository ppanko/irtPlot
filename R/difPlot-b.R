#' Differential Item Functioning IRT models.
#'
#' \code{difPlot} returns a plot within a new device and saves to chosen directory.
#'
#' This function specializes in generating models to examine DIF.
#' Currently, the only supported type is \code{"icc"} with \code{"1PL"} and \code{"2PL"} models.
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
#' theta <- seq(-3,3, 0.01)
#' difPlot(data, theta, grp = group, model = "1PL", type = "icc")
#'
#'

difPlot <- function(dat,
                    theta = seq(-3,3, 0.01),
                    title = NULL,
                    ddir = getwd(),
                    save = FALSE,
                    model,
                    grp,
                    fln = paste0(model, "_", "dif_", colnames(dat), ".jpg"),
                    dpi = 300,
                    height = 8.5,
                    width = 10,
                    itmNam = colnames(dat),
                    silent = FALSE)

{

    if(length(title) < 1) title2 <- sapply(itmNam, function(x) paste0("Item Characteristic Functions \n for the ", x, " Item Between Groups"))
    else title2 <- title

    ylb <- expression(atop(P(theta),))
    inds <- ncol(dat)

    if(model == "1PL") {

        out <- by(dat, grp, function(x) summary(ltm::rasch(x))$coefficients[,1])
        cf <- lapply(out, crtFrmu, ind = inds)

    } else if(model == "2PL") {

        out <- by(dat, grp, function(x) summary(ltm::ltm(x ~ z1))$coefficients[,1])
        cf <- lapply(out, crtFrmn, ind = inds)

    } else stop("Please enter a valid plot type, comrade")

    prb <- lapply(cf, calcD)

    for(j in 1:length(prb)) {
        for(i in 1:length(prb)) {
            prb[[j]][[i]] <- prb[[i]][[j]]
        }
    }

    itms <- lapply(prb, dbind, g = grp)

    itmplot <- mapply(plotDif, itms, title2, ylbs = ylb, SIMPLIFY = FALSE)

    if(save == TRUE) mapply(gSave, x = itmplot, flnm = fln, dDir = ddir, res = dpi, hgt = height, wdt = width)

    if(silent == FALSE) print(lapply(itmplot, prints))
}
