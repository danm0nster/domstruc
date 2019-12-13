#' Convert from ahull to spatial polygon
#'
#' @param x
#' @param increment
#' @param rnd
#' @param proj4string
#'
#' @return

# Modified version of ah2sp from rangeBuilder package.
# TODO: Clean up this function, since it computes more things than needed.
# TODO: Consider renaming it (it no longer returns Spatial Polygons)
ah2sp <- function(x, increment=360, rnd=10, proj4string=CRS(as.character(NA))) {
  require(alphahull)
  require(maptools)
  if (class(x) != "ahull") {
    stop("x needs to be an ahull class object")
  }
  # Extract the edges from the ahull object as a dataframe
  xdf <- as.data.frame(x$arcs)
  # Remove all cases where the coordinates are all the same
  xdf <- subset(xdf, xdf$r > 0)
  res <- NULL
  if (nrow(xdf) > 0) {
    # Convert each arc to a line segment
    linesj <- list()
    prevx <- NULL
    prevy <- NULL
    j <- 1
    for (i in 1:nrow(xdf)) {
      rowi <- xdf[i, ]
      v <- c(rowi$v.x, rowi$v.y)
      theta <- rowi$theta
      r <- rowi$r
      cc <- c(rowi$c1, rowi$c2)
      # Arcs need to be redefined as strings of points. Work out the number of
      # points to allocate in this arc segment.
      ipoints <- 2 + round(increment * (rowi$theta / 2), 0)
      # Calculate coordinates from arc() description for ipoints along the arc.
      angles <- anglesArc(v, theta)
      seqang <- seq(angles[1], angles[2], length = ipoints)
      x <- round(cc[1] + r * cos(seqang), rnd)
      y <- round(cc[2] + r * sin(seqang), rnd)
      # Check for line segments that should be joined up and combine their
      # coordinates
      if (is.null(prevx)) {
        prevx <- x
        prevy <- y
      } else if (x[1] == round(prevx[length(prevx)], rnd) &&
                 y[1] == round(prevy[length(prevy)], rnd)) {
        if (i == nrow(xdf)) {
          # We have got to the end of the dataset
          prevx <- append(prevx, x[2:ipoints])
          prevy <- append(prevy, y[2:ipoints])
          prevx[length(prevx)] <- prevx[1]
          prevy[length(prevy)] <- prevy[1]
          coordsj <- cbind(prevx, prevy)
          colnames(coordsj) <- NULL
          # Build as Line and then Lines class
          linej <- Line(coordsj)
          linesj[[j]] <- Lines(linej, ID = as.character(j))
        } else {
          prevx <- append(prevx, x[2:ipoints])
          prevy <- append(prevy, y[2:ipoints])
        }
      } else {
        # We have got to the end of a set of lines, and there are several such
        # sets, so convert the whole of this one to a line segment and reset.
        prevx[length(prevx)] <- prevx[1]
        prevy[length(prevy)] <- prevy[1]
        coordsj <- cbind(prevx, prevy)
        colnames(coordsj) <- NULL
        # Build as Line and then Lines class
        linej <- Line(coordsj)
        linesj[[j]] <- Lines(linej, ID = as.character(j))
        j <- j + 1
        prevx <- NULL
        prevy <- NULL
      }
    }
    # Promote to SpatialLines
    lspl <- SpatialLines(linesj)
    # Convert lines to polygons Pull out Lines slot and check which lines have
    # start and end points that are the same
    lns <- slot(lspl, "lines")
    polys <- sapply(lns, function(x) {
      crds <- slot(slot(x, "Lines")[[1]], "coords")
      identical(crds[1, ], crds[nrow(crds), ])
    })
    # Select those that do and convert to SpatialPolygons
    polyssl <- lspl[polys]
    list_of_Lines <- slot(polyssl, "lines")
    sppolys <- SpatialPolygons(
      list(Polygons(lapply(list_of_Lines,
                           function(x) {
                             Polygon(slot(slot(x, "Lines")[[1]], "coords"))
                             }),
                    ID = "1")), proj4string = proj4string)
    # Create a set of ids in a dataframe, then promote to
    # SpatialPolygonsDataFrame
    hid <- sapply(slot(sppolys, "polygons"), function(x) slot(x, "ID"))
    areas <- sapply(slot(sppolys, "polygons"), function(x) slot(x, "area"))
    df <- data.frame(hid, areas)
    names(df) <- c("HID", "Area")
    rownames(df) <- df$HID
    res <- SpatialPolygonsDataFrame(sppolys, data = df)
    res <- res[which(res@data$Area > 0), ]
  }
  return(coordsj)
}


convex_hull <-  function(blur_data) {
  subxfile.blur.hFrP <- subset(blur_data,
                               select = c(blur, focus_ci_hi, position)
  )
  names(subxfile.blur.hFrP) <- c("blur", "x", "y")
  subxfile.blur.hFrP$type <- "subxfile.blur.hFrP"

  #
  subxfile.blur.lFrP <- subset(blur_data,
                               select = c(blur, focus_ci_lo, position)
  )
  names(subxfile.blur.lFrP) <- c("blur", "x", "y")
  subxfile.blur.lFrP$type <- "subxfile.blur.lFrP"

  #
  subxfile.blur.rFhP <- subset(blur_data,
                               select = c(blur, focus, position_ci_hi)
  )
  names(subxfile.blur.rFhP) <- c("blur", "x", "y")
  subxfile.blur.rFhP$type <- "subxfile.blur.rFhP"

  #
  subxfile.blur.rFlP <- subset(blur_data,
                               select = c(blur, focus, position_ci_lo)
  )
  names(subxfile.blur.rFlP) <- c("blur", "x", "y")
  subxfile.blur.rFlP$type <- "subxfile.blur.rFlP"


  errors.xy <- rbind(subxfile.blur.hFrP,
                     subxfile.blur.lFrP,
                     subxfile.blur.rFhP,
                     subxfile.blur.rFlP)

  errors.xy.justxy <- subset(errors.xy, select = c(x, y))


  # ALPHA HULL: construct hull around simulated downward heuristic data using
  # alpha hull method (to make hull tight to ends of CIs)
  ahuld <- alphahull::ahull(errors.xy.justxy, alpha = 2)

  # Convert to polygon (conversion code above by Andrew Beven,
  # http://r-sig-geo.2731867.n2.nabble.com/alpha-hull-ahull-to-polygon-shapefile-td7342734.html)
  ahuld.conv <- ah2sp(ahuld)
  ahuld.conv <- as.data.frame(ahuld.conv)
  names(ahuld.conv) <- c("x", "y")
  return(ahuld.conv)
}

make_polygons <- function(data, blur_data) {
  # Define polygons to delineate regions to categorize strategies

  # BULLY POLYGON: region above simulated downward heuristic polygon, excluding
  # undefined area
  bully.start.data <- subset(blur_data, blur == 0)
  names(bully.start.data)
  bully.start.data <- subset(bully.start.data, select = c(focus, position))
  bully.start <- c(2, bully.start.data$position)

  blur.line <- subset(blur_data, select = c(focus, position))
  origin.corners <- subset(blur_data, blur == 1)
  origin.highx <- c(origin.corners$focus, 2)
  # origin.highx <- c(origin.corners$focus, 2)

  bully.poly <- rbind(bully.start, blur.line, origin.highx)
  names(bully.poly) <- c("x", "y")

  # CLOSE COMPETITORS POLYGON: region below simulated downward heuristic
  # polygon, excluding undefined area
  clcomps.start.data <- subset(blur_data, blur == 0)
  clcomps.start.data <- subset(clcomps.start.data, select = c(focus, position))
  clcomps.start <- c(2, clcomps.start.data$position) # same start as bully
  origin.corner.coords.clcomp <- c(origin.corners$focus, -2)

  clcomps.poly <- rbind(clcomps.start, blur.line, origin.corner.coords.clcomp)
  names(clcomps.poly) <- c("x", "y")

  # UNDEFINED POLYGON: region to left of simulated downward heuristic polygon
  undef.start.data <- subset(blur_data, blur == 1)
  undef.x <- undef.start.data$focus
  undef.poly <- data.frame(rbind(
    c(undef.x, 2), c(-1, 2), c(-1, -1), c(undef.x, -1)))

  names(undef.poly) <- c("x", "y")
  polygons <- list(
    bully = bully.poly,
    clcomps = clcomps.poly,
    undef = undef.poly
  )
  return(polygons)
}

point_inside_polygon <- function(data, blur_data, ahuld.conv) {
  polygons <- make_polygons(data, blur_data)
  bully.poly <- polygons[["bully"]]
  clcomps.poly <- polygons[["clcomps"]]
  undef.poly <- polygons[["undef"]]
  ##########################
  # Are points in polygons?
  ##########################

  # TODO: modularize code below, by using functions.

  # r= "real" focus or position, the actual calculated value
  # h= "high" end of error bar
  # l= "low" end of error bar

  # is real data in downward heuristic polygon? (using 4 points of extreme ends
  # of errorbars)
  PinP.real.rFrP <- point.in.polygon(data$focus, data$position,
                                     ahuld.conv$x, ahuld.conv$y,
                                     mode.checked = FALSE
  )
  PinP.real.hFrP <- point.in.polygon(data$focus_ci_hi, data$position,
                                     ahuld.conv$x, ahuld.conv$y,
                                     mode.checked = FALSE
  )
  PinP.real.lFrP <- point.in.polygon(data$focus_ci_lo, data$position,
                                     ahuld.conv$x, ahuld.conv$y,
                                     mode.checked = FALSE
  )
  PinP.real.rFhP <- point.in.polygon(data$focus, data$position_ci_hi,
                                     ahuld.conv$x, ahuld.conv$y,
                                     mode.checked = FALSE
  )
  PinP.real.rFlP <- point.in.polygon(data$focus, data$position_ci_lo,
                                     ahuld.conv$x, ahuld.conv$y,
                                     mode.checked = FALSE
  )
  point.inorout <- cbind.data.frame(PinP.real.rFrP,
                                    PinP.real.hFrP,
                                    PinP.real.lFrP,
                                    PinP.real.rFhP,
                                    PinP.real.rFlP)

  # is real data in BULLY polygon? (using 4 points of extreme ends of errorbars)
  PinP.bully.rFrP <- point.in.polygon(data$focus, data$position,
                                      bully.poly$x, bully.poly$y,
                                      mode.checked = FALSE
  )
  PinP.bully.hFrP <- point.in.polygon(data$focus_ci_hi, data$position,
                                      bully.poly$x, bully.poly$y,
                                      mode.checked = FALSE
  )
  PinP.bully.lFrP <- point.in.polygon(data$focus_ci_lo, data$position,
                                      bully.poly$x, bully.poly$y,
                                      mode.checked = FALSE
  )
  PinP.bully.rFhP <- point.in.polygon(data$focus, data$position_ci_hi,
                                      bully.poly$x, bully.poly$y,
                                      mode.checked = FALSE
  )
  PinP.bully.rFlP <- point.in.polygon(data$focus, data$position_ci_lo,
                                      bully.poly$x, bully.poly$y,
                                      mode.checked = FALSE
  )
  point.inorout.bully <- cbind.data.frame(PinP.bully.rFrP,
                                          PinP.bully.hFrP,
                                          PinP.bully.lFrP,
                                          PinP.bully.rFhP,
                                          PinP.bully.rFlP)

  # is real data in CLOSE COMPETITORS?? (using 4 points of extreme ends of errorbars)
  PinP.CC.rFrP <- point.in.polygon(data$focus, data$position,
                                   clcomps.poly$x, clcomps.poly$y,
                                   mode.checked = FALSE
  )
  PinP.CC.hFrP <- point.in.polygon(data$focus_ci_hi, data$position,
                                   clcomps.poly$x, clcomps.poly$y,
                                   mode.checked = FALSE
  )
  PinP.CC.lFrP <- point.in.polygon(data$focus_ci_lo, data$position,
                                   clcomps.poly$x, clcomps.poly$y,
                                   mode.checked = FALSE
  )
  PinP.CC.rFhP <- point.in.polygon(data$focus, data$position_ci_hi,
                                   clcomps.poly$x, clcomps.poly$y,
                                   mode.checked = FALSE
  )
  PinP.CC.rFlP <- point.in.polygon(data$focus, data$position_ci_lo,
                                   clcomps.poly$x, clcomps.poly$y,
                                   mode.checked = FALSE
  )
  point.inorout.CC <- cbind.data.frame(PinP.CC.rFrP,
                                       PinP.CC.hFrP,
                                       PinP.CC.lFrP,
                                       PinP.CC.rFhP,
                                       PinP.CC.rFlP)


  # is real data in UNDEFINED?? (using 4 points of extreme ends of errorbars)
  PinP.UND.rFrP <- point.in.polygon(data$focus, data$position,
                                    undef.poly$x, undef.poly$y,
                                    mode.checked = FALSE
  )
  PinP.UND.hFrP <- point.in.polygon(data$focus_ci_hi, data$position,
                                    undef.poly$x, undef.poly$y,
                                    mode.checked = FALSE
  )
  PinP.UND.lFrP <- point.in.polygon(data$focus_ci_lo, data$position,
                                    undef.poly$x, undef.poly$y,
                                    mode.checked = FALSE
  )
  PinP.UND.rFhP <- point.in.polygon(data$focus, data$position_ci_hi,
                                    undef.poly$x, undef.poly$y,
                                    mode.checked = FALSE
  )
  PinP.UND.rFlP <- point.in.polygon(data$focus, data$position_ci_lo,
                                    undef.poly$x, undef.poly$y,
                                    mode.checked = FALSE
  )

  point.inorout.UND <- cbind.data.frame(PinP.UND.rFrP,
                                        PinP.UND.hFrP,
                                        PinP.UND.lFrP,
                                        PinP.UND.rFhP,
                                        PinP.UND.rFlP)

  # compile all point in poly results
  polyinVout <- cbind.data.frame(point.inorout,
                                 point.inorout.bully,
                                 point.inorout.CC,
                                 point.inorout.UND)
  return(polyinVout)
}
