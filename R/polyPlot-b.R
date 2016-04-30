polyPlot <- function(dat,
                     theta,
                     title = NULL,
                     ddir = getwd(),
                     save = FALSE,
                     model,
                     type,
                     fln = paste0(model, "_", type, "_", colnames(dat), ".jpg"),
                     dpi = 800,
                     height = 6.5,
                     width = 10,
                     itmNam = colnames(dat))

{

    ylb <- expression(atop(P[x](theta),))

    ld <- lapply(dat, function(x) factor(levels(x), levels = levels(x)))

    if(length(title) < 1) title2 <- sapply(itmNam, namNamp, t = type)

    if(model == "grm") {

        cf <- summary(ltm::grm(dat, IRT.param = FALSE))$coefficients
        prb <- mapply(pFun, x = cf, l = ld, SIMPLIFY = FALSE)

        if(type == "icc") {

            pdt <- lapply(prb, function(x) do.call(rbind.data.frame, x))

            for(i in 1:length(pdt)) {
                pdt[[i]]$level <- rep(paste0("X > ", 1:(length(levels(pdt[[i]]$level))-1)), each = length(theta))
            }

            plPlot <- list()
            for(i in 1:length(pdt)) plPlot[[i]] <- plotPoly(pdt[[i]], ttl = title2[i], ylbs = ylb)

        } else if(type == "iif") {

            lst <- lapply(prb, respFun)
            pdt <- mapply(cbmFun, x = lst, z = ld, SIMPLIFY = FALSE)

            plPlot <- list()
            for(i in 1:length(pdt)) plPlot[[i]] <- plotPoly(pdt[[i]], ttl = title2[i], ylbs = ylb)

        } else stop("Please provide a valid plot type, comrade")

    } else if(model == "pcm") {

        md <- summary(ltm::gpcm(dat, IRT.param = FALSE))
        cf <- lapply(md$coefficients, function(x) x[,1])
        pdt <- mapply(cmbFun, x = cf, l = ld, SIMPLIFY = FALSE)
        pdt1 <- lapply(pdt, function(x) lapply(x, exp))

        for (i in 1:length(pdt1)) {

            pdt1[[i]][[length(ld[[i]])+1]] <- Reduce("+", pdt1[[i]])
        }

        rdt <- lapply(pdt1, rpbFun)

        if(type == "iif") {

            lds <- list()
            for(i in 1:length(ld)) lds[[i]] <- rep(ld[[i]], each = length(theta))

            rdt1 <- mapply(mbind, x = rdt, l = lds, SIMPLIFY = FALSE)

            plPlot <- list()
            for(i in 1:length(pdt)) plPlot[[i]] <- plotPoly(rdt1[[i]], ttl = title2[i], ylbs = ylb)

        } else if (type == "icc") {

            odt <- lapply(rdt, prbFun)
            ld <- lapply(dat, function(x) paste0("X > ", 1:(length(levels(x))-1)))

            lds <- list()
            for(i in 1:length(ld)) lds[[i]] <- rep(ld[[i]], each = length(theta))

            odt1 <- mapply(mbind, x = odt, l = lds, SIMPLIFY = FALSE)

            plPlot <- list()
            for(i in 1:length(pdt)) plPlot[[i]] <- plotPoly(odt1[[i]], ttl = title2[i], ylbs = ylb)

        } else stop("Please provide a valid plot type, comrade")
    }

    if(save == TRUE) mapply(gSave, x = plPlot , flnm = fln, dDir = ddir, res = dpi, hgt = height, wdt = width)

    print(lapply(plPlot, prints))

}
