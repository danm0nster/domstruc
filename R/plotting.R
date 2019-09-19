library(alphahull)
library(scales)

#' Title
#'
#' @param blur_data
#'
#' @return
#' @export
#'
#' @examples
dom_plot_strategy <- function(data, blur_data) {
  dev.new(width=8, height=8, unit="in")

  subxfile.blur.hFrP <- subset(blur_data,
                               select=c(blur, focus_ci_hi, position))
  names(subxfile.blur.hFrP) <- c("blur", "x", "y")
  subxfile.blur.hFrP$type <- "subxfile.blur.hFrP"

  #
  subxfile.blur.lFrP <- subset(blur_data,
                               select=c(blur, focus_ci_lo, position))
  names(subxfile.blur.lFrP) <- c("blur", "x", "y")
  subxfile.blur.lFrP$type <- "subxfile.blur.lFrP"

  #
  subxfile.blur.rFhP <- subset(blur_data,
                               select=c(blur, focus, position_ci_hi))
  names(subxfile.blur.rFhP) <- c("blur", "x", "y")
  subxfile.blur.rFhP$type <- "subxfile.blur.rFhP"

  #
  subxfile.blur.rFlP <- subset(blur_data,
                               select=c(blur, focus, position_ci_lo))
  names(subxfile.blur.rFlP) <- c("blur", "x", "y")
  subxfile.blur.rFlP$type <- "subxfile.blur.rFlP"


  errors.xy <- rbind(subxfile.blur.hFrP, subxfile.blur.lFrP, subxfile.blur.rFhP, subxfile.blur.rFlP)

  errors.xy.justxy <- subset(errors.xy, select=c(x,y))


  # ALPHA HULL: construct hull around simulated downward heuristic data using alpha hull method (to make hull tight to ends of CIs)
  ahuld <- alphahull::ahull(errors.xy.justxy, alpha = 2)

  #Convert to polygon (conversion code above by Andrew Beven, http://r-sig-geo.2731867.n2.nabble.com/alpha-hull-ahull-to-polygon-shapefile-td7342734.html)
  ahuld.conv <- ah2sp(ahuld)
  str(ahuld.conv)
  ahuld.conv <- as.data.frame(ahuld.conv)
  names(ahuld.conv) <- c("x", "y")

  # Define polygons to delineate regions to categorize strategies

  # BULLY POLYGON: region above simulated downward heuristic polygon, excluding undefined area
  bully.start.data <- subset(blur_data, blur==0)
  names(bully.start.data)
  bully.start.data <- subset(bully.start.data, select=c(focus,position))
  bully.start <- c(2, bully.start.data$position)

  blur.line <- subset(blur_data, select=c(focus, position))
  origin.corners <- subset(blur_data, blur==1)
  origin.highx <- c(origin.corners$focus, 2)
  #origin.highx <- c(origin.corners$focus, 2)

  bully.poly <- rbind(bully.start, blur.line, origin.highx)
  names(bully.poly) <- c("x", "y")

  # CLOSE COMPETITORS POLYGON: region below simulated downward heuristic polygon, excluding undefined area
  clcomps.start.data <- subset(blur_data, blur==0)
  clcomps.start.data <- subset(clcomps.start.data, select=c(focus,position))
  clcomps.start <- c(2, clcomps.start.data$position) #same start as bully
  origin.corner.coords.clcomp <- c(origin.corners$focus, -2)

  clcomps.poly <- rbind(clcomps.start, blur.line, origin.corner.coords.clcomp)
  names(clcomps.poly) <- c("x", "y")

  # UNDEFINED POLYGON: region to left of simulated downward heuristic polygon
  undef.start.data <- subset(blur_data, blur==1)
  undef.x <- undef.start.data$focus
  undef.poly <- data.frame(rbind(c(undef.x, 2), c(-1, 2), c(-1,-1), c(undef.x,-1)))

  names(undef.poly) <- c("x", "y")

  ##########################
  # Are points in polygons?
  ##########################

  #r= "real" focus or position, the actual calculated value
  #h= "high" end of error bar
  #l= "low" end of error bar

  # is real data in downward heuristic polygon? (using 4 points of extreme ends of errorbars)
  PinP.real.rFrP <- point.in.polygon(data$focus, data$position,
                                     ahuld.conv$x, ahuld.conv$y,
                                     mode.checked=FALSE)
  PinP.real.hFrP <- point.in.polygon(data$focus_ci_hi, data$position,
                                     ahuld.conv$x, ahuld.conv$y,
                                     mode.checked=FALSE)
  PinP.real.lFrP <- point.in.polygon(data$focus_ci_lo, data$position,
                                     ahuld.conv$x, ahuld.conv$y,
                                     mode.checked=FALSE)
  PinP.real.rFhP <- point.in.polygon(data$focus, data$position_ci_hi,
                                     ahuld.conv$x, ahuld.conv$y,
                                     mode.checked=FALSE)
  PinP.real.rFlP <- point.in.polygon(data$focus, data$position_ci_lo,
                                     ahuld.conv$x, ahuld.conv$y,
                                     mode.checked=FALSE)
  point.inorout <- cbind.data.frame(PinP.real.rFrP, PinP.real.hFrP, PinP.real.lFrP, PinP.real.rFhP, PinP.real.rFlP)

  # is real data in BULLY polygon? (using 4 points of extreme ends of errorbars)
  PinP.bully.rFrP <- point.in.polygon(data$focus, data$position,
                                      bully.poly$x, bully.poly$y,
                                      mode.checked=FALSE)
  PinP.bully.hFrP <- point.in.polygon(data$focus_ci_hi, data$position,
                                      bully.poly$x, bully.poly$y,
                                      mode.checked=FALSE)
  PinP.bully.lFrP <- point.in.polygon(data$focus_ci_lo, data$position,
                                      bully.poly$x, bully.poly$y,
                                      mode.checked=FALSE)
  PinP.bully.rFhP <- point.in.polygon(data$focus, data$position_ci_hi,
                                      bully.poly$x, bully.poly$y,
                                      mode.checked=FALSE)
  PinP.bully.rFlP <- point.in.polygon(data$focus, data$position_ci_lo,
                                      bully.poly$x, bully.poly$y,
                                      mode.checked=FALSE)
  point.inorout.bully <- cbind.data.frame(PinP.bully.rFrP, PinP.bully.hFrP, PinP.bully.lFrP, PinP.bully.rFhP, PinP.bully.rFlP)

  # is real data in CLOSE COMPETITORS?? (using 4 points of extreme ends of errorbars)
  PinP.CC.rFrP <- point.in.polygon(data$focus, data$position,
                                   clcomps.poly$x, clcomps.poly$y,
                                   mode.checked=FALSE)
  PinP.CC.hFrP <- point.in.polygon(data$focus_ci_hi, data$position,
                                   clcomps.poly$x, clcomps.poly$y,
                                   mode.checked=FALSE)
  PinP.CC.lFrP <- point.in.polygon(data$focus_ci_lo, data$position,
                                   clcomps.poly$x, clcomps.poly$y,
                                   mode.checked=FALSE)
  PinP.CC.rFhP <- point.in.polygon(data$focus, data$position_ci_hi,
                                   clcomps.poly$x, clcomps.poly$y,
                                   mode.checked=FALSE)
  PinP.CC.rFlP <- point.in.polygon(data$focus, data$position_ci_lo,
                                   clcomps.poly$x, clcomps.poly$y,
                                   mode.checked=FALSE)
  point.inorout.CC <- cbind.data.frame(PinP.CC.rFrP, PinP.CC.hFrP, PinP.CC.lFrP, PinP.CC.rFhP, PinP.CC.rFlP)


  # is real data in UNDEFINED?? (using 4 points of extreme ends of errorbars)
  PinP.UND.rFrP <- point.in.polygon(data$focus, data$position,
                                    undef.poly$x, undef.poly$y,
                                    mode.checked=FALSE)
  PinP.UND.hFrP <- point.in.polygon(data$focus_ci_hi, data$position,
                                    undef.poly$x, undef.poly$y,
                                    mode.checked=FALSE)
  PinP.UND.lFrP <- point.in.polygon(data$focus_ci_lo, data$position,
                                    undef.poly$x, undef.poly$y,
                                    mode.checked=FALSE)
  PinP.UND.rFhP <- point.in.polygon(data$focus, data$position_ci_hi,
                                    undef.poly$x, undef.poly$y,
                                    mode.checked=FALSE)
  PinP.UND.rFlP <- point.in.polygon(data$focus, data$position_ci_lo,
                                    undef.poly$x, undef.poly$y,
                                    mode.checked=FALSE)

  point.inorout.UND <- cbind.data.frame(PinP.UND.rFrP, PinP.UND.hFrP, PinP.UND.lFrP, PinP.UND.rFhP, PinP.UND.rFlP)

  # compile all point in poly results
  polyinVout <- cbind.data.frame(point.inorout, point.inorout.bully, point.inorout.CC, point.inorout.UND)

  names(polyinVout)

  # combine and save point and poly data ********************************************************** TOGGLE
  #SUMM.point.topoly <- rbind(SUMM.point.topoly, polyinVout) # bind to summary dataset


  ##########################PLOT
  #plot real data
  with(data, plot(focus, position,
                           ylim=c(-0.1,1), xlim=c(-0.1,1), las=1,
                           col="blue", pch=5,
                           xlab="focus", ylab="position"))

  #plot error around real data

  #horizontal focus error, at real position
  graphics::segments(x0=data$focus_ci_lo, x1=data$focus_ci_hi,
           y0=data$position, y1=data$position,
           col=scales::alpha("blue", 0.7), lwd=1.5)

  #vertical position error, at real focus
  graphics::segments(x0=data$focus, x1=data$focus,
           y0=data$position_ci_lo, y1=data$position_ci_hi,
           col=scales::alpha("blue", 0.7), lwd=1.5)

  #plot line for increasingly blurred downward heuristic
  lines(x=blur_data$focus, y=blur_data$position)
  points(x=blur_data$focus, y=blur_data$position)
  text(x=blur_data$focus, y=blur_data$position+0.05, label=blur_data$blur, cex=0.6)

  #horizontal focus error, at blurred position
  graphics::segments(x0=blur_data$focus_ci_lo, x1=blur_data$focus_ci_hi,
           y0=blur_data$position, y1=blur_data$position,
           col=scales::alpha("black", 0.7), lwd=1.5)

  #vertical position error, at blurred focus
  graphics::segments(x0=blur_data$focus, x1=blur_data$focus,
           y0=blur_data$position_ci_lo, y1=blur_data$position_ci_hi,
           col=scales::alpha("black", 0.7), lwd=1.5)

  polygon(ahuld.conv, col=scales::alpha("red",0.05), border=scales::alpha("red",0.4), lwd=1.5)


  # save plot
  #dev.print(pdf, file=paste(device, Lblurplots, file.name, "-blur.pdf",sep=""))


  ########## plot showing strategy polygons
  #plot real data
  with(data, plot(focus, position,
                           ylim=c(-0.1,1), xlim=c(-0.1,1), las=1,
                           col="blue", pch=5,
                           xlab="focus", ylab="position"))

  # Add strategy polygons to bottom plotted layer
  #undefined (light grey)
  polygon(undef.poly, col=scales::alpha("grey",0.05), border=scales::alpha("black",0.5), lty=2, lwd=1.5)

  #bully (blue)
  polygon(bully.poly, col=scales::alpha("blue",0.05), border=scales::alpha("black",0.5), lty=2, lwd=1.5)

  #close competitors (red)
  polygon(clcomps.poly, col=scales::alpha("red",0.05), border=scales::alpha("black",0.5), lty=2, lwd=1.5)

  # downward heuristic (black, plotted over white)
  polygon(ahuld.conv, col=scales::alpha("white",1), border=scales::alpha("white",1), lwd=1.5) # overplot other strategies first
  polygon(ahuld.conv, col=scales::alpha("black",0.05), border=scales::alpha("black",0.4), lwd=1.5)


  legend("bottomright",
         c("Downward heuristic",
           "Bullying",
           "Close competitors",
           "Undefined"),
         #bty="n",
         col="black",
         pt.bg = c(scales::alpha("black", 0.2),
                   scales::alpha("blue", 0.2),
                   scales::alpha("red", 0.2),
                   scales::alpha("grey", 0.2)),
         pch=22, cex=1, pt.cex=2
  )
  # add points and segments to upper layer

  #plot line for increasingly blurred downward heuristic, text above top of high position errorbars
  lines(x=blur_data$focus, y=blur_data$position)
  points(x=blur_data$focus, y=blur_data$position)
  text(x=blur_data$focus, y=blur_data$position_ci_hi+0.025, label=blur_data$blur, cex=0.6)

  #horizontal focus error, at blurred position
  graphics::segments(x0=blur_data$focus_ci_lo, x1=blur_data$focus_ci_hi,
           y0=blur_data$position, y1=blur_data$position,
           col=scales::alpha("black", 0.7), lwd=1.5)

  #vertical position error, at blurred focus
  graphics::segments(x0=blur_data$focus, x1=blur_data$focus,
           y0=blur_data$position_ci_lo, y1=blur_data$position_ci_hi,
           col=scales::alpha("black", 0.7), lwd=1.5)

  # add real data

  #horizontal focus error, at real position
  graphics::segments(x0=data$focus_ci_lo, x1=data$focus_ci_hi,
           y0=data$position, y1=data$position,
           col=scales::alpha("blue", 0.7), lwd=2)

  #vertical position error, at real focus
  graphics::segments(x0=data$focus, x1=data$focus,
           y0=data$position_ci_lo, y1=data$position_ci_hi,
           col=scales::alpha("blue", 0.7), lwd=2)

  with(data, points(focus, position, col="white", bg = "blue", pch=23, cex=1.75))

}
