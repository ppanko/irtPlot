difPlot <- function(dat,
                    theta = seq(-3,3, 0.01),
                    title = NULL,
                    ddir = getwd(),
                    save = FALSE,
                    model,
                    grp,
                    fln = paste0(model, "_", "dif_", colnames(dat), ".jpg"),
                    dpi = 800,
                    height = 8.5,
                    width = 10,
                    itmNam = colnames(dat),
                    silent = FALSE)

{

    if(length(title) < 1) title2 <- sapply(itmNam, function(x) paste0("Item Characteristic Functions \n for the ", x, " Item Between Groups"))
    else title2 <- title

    ylb <- expression(atop(P(theta),))
    ind <- ncol(dat)

    if(model == "1PL") {

        out <- by(dat, grp, function(x) summary(ltm::rasch(x))$coefficients[,1])
        cf <- lapply(out, crtFrmu)

    } else if(model == "2PL") {

        out <- by(dat, grp, function(x) summary(ltm::ltm(x ~ z1))$coefficients[,1])
        cf <- lapply(out, crtFrmn)

    } else stop("Please enter a valid plot type, comrade")

    prb <- lapply(cf, calcD)

    for(j in 1:length(prb)) {
        for(i in 1:length(prb)) {
            prb[[j]][[i]] <- prb[[i]][[j]]
        }
    }

    itms <- lapply(prb, dbind)

    itmplot <- mapply(plotDif, itms, title2, SIMPLIFY = FALSE)

    if(save == TRUE) mapply(gSave, x = itmplot, flnm = fln, dDir = ddir, res = dpi, hgt = height, wdt = width)

    if(silent == FALSE) print(lapply(itmplot, prints))
}
