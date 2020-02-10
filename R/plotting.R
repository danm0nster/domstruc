library(alphahull)
library(scales)

#' Plot focus, position and show strategy regions based on downward null
#' heuristic.
#'
#' @param data A data frame containing one row with columns `focus` and `position`
#' @param blur_data A data frame that is created by `dom_make_blur_data()`
#'
#' @return
#' @export
#'
#' @examples
dom_plot_strategy <- function(data, blur_data, show_data_ci = FALSE) {
  # TODO: Check input data is in the right format (option to not have ci in data if show_data_ci == FALSE)
  # Get convex hull of blur data with error bars
  ahuld.conv <- convex_hull(blur_data)

  polygons <- make_polygons(blur_data)
  bully.poly <- polygons[["bully"]]
  clcomps.poly <- polygons[["clcomps"]]
  undef.poly <- polygons[["undef"]]

  ########## plot showing strategy polygons
  # plot real data
  with(data, plot(focus, position,
    ylim = c(-0.1, 1), xlim = c(-0.1, 1), las = 1,
    col = "blue", pch = 5,
    xlab = "focus", ylab = "position"
  ))

  # Add strategy polygons to bottom plotted layer
  # undefined (light grey)
  polygon(undef.poly, col = scales::alpha("grey", 0.05),
          border = scales::alpha("black", 0.5), lty = 2, lwd = 1.5)

  # bully (blue)
  polygon(bully.poly, col = scales::alpha("blue", 0.05),
          border = scales::alpha("black", 0.5), lty = 2, lwd = 1.5)

  # close competitors (red)
  polygon(clcomps.poly, col = scales::alpha("red", 0.05),
          border = scales::alpha("black", 0.5), lty = 2, lwd = 1.5)

  # downward heuristic (black, plotted over white)
  polygon(ahuld.conv, col = scales::alpha("white", 1),
          border = scales::alpha("white", 1), lwd = 1.5) # overplot other strategies first
  polygon(ahuld.conv, col = scales::alpha("black", 0.05),
          border = scales::alpha("black", 0.4), lwd = 1.5)


  legend("bottomright",
    c(
      "Downward heuristic",
      "Bullying",
      "Close competitors",
      "Undefined"
    ),
    # bty="n",
    col = "black",
    pt.bg = c(
      scales::alpha("black", 0.2),
      scales::alpha("blue", 0.2),
      scales::alpha("red", 0.2),
      scales::alpha("grey", 0.2)
    ),
    pch = 22, cex = 1, pt.cex = 2
  )
  # add points and segments to upper layer

  # plot line for increasingly blurred downward heuristic, text above top of high position errorbars
  lines(x = blur_data$focus, y = blur_data$position)
  points(x = blur_data$focus, y = blur_data$position)
  text(x = blur_data$focus, y = blur_data$position_ci_hi + 0.025,
       label = blur_data$blur, cex = 0.6)

  # horizontal focus error, at blurred position
  graphics::segments(
    x0 = blur_data$focus_ci_lo, x1 = blur_data$focus_ci_hi,
    y0 = blur_data$position, y1 = blur_data$position,
    col = scales::alpha("black", 0.7), lwd = 1.5
  )

  # vertical position error, at blurred focus
  graphics::segments(
    x0 = blur_data$focus, x1 = blur_data$focus,
    y0 = blur_data$position_ci_lo, y1 = blur_data$position_ci_hi,
    col = scales::alpha("black", 0.7), lwd = 1.5
  )

  # add real data
  if (show_data_ci) {
    # horizontal focus error, at real position
    graphics::segments(
      x0 = data$focus_ci_lo, x1 = data$focus_ci_hi,
      y0 = data$position, y1 = data$position,
      col = scales::alpha("blue", 0.7), lwd = 2
    )

    # vertical position error, at real focus
    graphics::segments(
      x0 = data$focus, x1 = data$focus,
      y0 = data$position_ci_lo, y1 = data$position_ci_hi,
      col = scales::alpha("blue", 0.7), lwd = 2
    )
  }
  with(data, points(focus, position, col = "white", bg = "blue",
                      pch = 23, cex = 1.75))

}

#' Categorize group-level strategy
#'
#' @param data
#' @param blur_data
#' @param use_data_ci Controls whether confidence intercal on data is used.
#'   Default is FALSE, meaning only the point estimate is used.
#' @return String containing the name of the strategy used in the group.
#' @export
#'
#' @examples
dom_categorize_strategy <- function(data, blur_data, use_data_ci = FALSE) {
  # TODO: Check data and blur_data. Should not contain NA.
  ahuld.conv <- convex_hull(blur_data)
  SUMM.point.topoly <- point_inside_polygon(data, blur_data, ahuld.conv, use_data_ci)
  polygons <- make_polygons(blur_data)
  bully.poly <- polygons[["bully"]]
  clcomps.poly <- polygons[["clcomps"]]
  undef.poly <- polygons[["undef"]]

  # real data in downward heuristic?
  SUMM.point.topoly$datalocVSheuristic.DH.nIN <- SUMM.point.topoly$PinP.real.hFrP+SUMM.point.topoly$PinP.real.lFrP+SUMM.point.topoly$PinP.real.rFhP+SUMM.point.topoly$PinP.real.rFlP

  # real data in bully?
  SUMM.point.topoly$datalocVSheuristic.BU.nIN <- SUMM.point.topoly$PinP.bully.hFrP+SUMM.point.topoly$PinP.bully.lFrP+SUMM.point.topoly$PinP.bully.rFhP+SUMM.point.topoly$PinP.bully.rFlP

  # real data in close completitors?
  SUMM.point.topoly$datalocVSheuristic.CC.nIN <- SUMM.point.topoly$PinP.CC.hFrP+SUMM.point.topoly$PinP.CC.lFrP+SUMM.point.topoly$PinP.CC.rFhP+SUMM.point.topoly$PinP.CC.rFlP

  # real data in undefined?
  SUMM.point.topoly$datalocVSheuristic.UN.nIN <- SUMM.point.topoly$PinP.UND.hFrP+SUMM.point.topoly$PinP.UND.lFrP+SUMM.point.topoly$PinP.UND.rFhP+SUMM.point.topoly$PinP.UND.rFlP


  SUMM.point.topoly$strategy <- "xx"
  SUMM.point.topoly$strategy[SUMM.point.topoly$datalocVSheuristic.DH.nIN>0] <- "downward.heuristic"
  SUMM.point.topoly$strategy[SUMM.point.topoly$datalocVSheuristic.DH.nIN==0 & SUMM.point.topoly$datalocVSheuristic.BU.nIN==4] <- "pure.bully"
  SUMM.point.topoly$strategy[SUMM.point.topoly$datalocVSheuristic.DH.nIN==0 & SUMM.point.topoly$datalocVSheuristic.CC.nIN==4] <- "pure.close.competitors"
  SUMM.point.topoly$strategy[SUMM.point.topoly$datalocVSheuristic.DH.nIN==0 & SUMM.point.topoly$datalocVSheuristic.BU.nIN>0 & SUMM.point.topoly$datalocVSheuristic.CC.nIN>0] <- "mixed.strategies"
  SUMM.point.topoly$strategy[SUMM.point.topoly$datalocVSheuristic.DH.nIN==0 & SUMM.point.topoly$datalocVSheuristic.BU.nIN==0 & SUMM.point.topoly$datalocVSheuristic.CC.nIN==0] <- "pure.undefined"
  SUMM.point.topoly$strategy[SUMM.point.topoly$datalocVSheuristic.DH.nIN==0 & SUMM.point.topoly$datalocVSheuristic.BU.nIN==3] <- "mostly.bully"
  SUMM.point.topoly$strategy[SUMM.point.topoly$datalocVSheuristic.DH.nIN==0 & SUMM.point.topoly$datalocVSheuristic.CC.nIN==3] <- "mostly.close.competitors"
  SUMM.point.topoly$strategy[SUMM.point.topoly$datalocVSheuristic.DH.nIN==0 & SUMM.point.topoly$datalocVSheuristic.UN.nIN==3] <- "mostly.undefined"

  return(SUMM.point.topoly$strategy)
}
