#' Function to plot probabilities
#'
#' Summarizes probabilities per interval, with the size of each circle proportional to sample size
#'
#' @param x x-values
#' @param y y-values
#' @param ints Number of intervals (defauls is 4)
#' @param binom.model Not yet implemented. Will be used to plot the predicted probability from binomila model
#' @param ylim Specify ylim for plot
#'
#' @export
#'
#' @examples
#'

plot_prob <- function(x, y, ints = 4, binom.model = NULL, ylim = NULL){

  if(!(is.numeric(x)|is.integer(x))) {error("x not a number")}
  if(any((y %in% c(0,1)) != TRUE)) {error("y not binomial")}

  int_borders <- seq(from = min(x), to = max(x), length.out = ints + 1)
  df <- data.frame(y = y, x = x, int = cut(x, breaks = int_borders, include.lowest = TRUE))
  df <- df %>%
    group_by(int) %>%
    summarize(int_n = n(), int_prob = mean(y)) %>% ungroup %>%
    mutate(int_min = as.numeric(str_extract(int, "\\-*\\d+\\.*\\d*(?=,)")),
           int_max = as.numeric(str_extract(int, "(?<=,)\\-*\\d+\\.*\\d*"))) %>%
    mutate(int_mid = int_max - (int_max - int_min)/2)

  plot(x = df$int_mid, y = df$int_prob,
       cex = df$int_n^(1/2)/max(df$int_n)^(1/4),
       ylim = ylim)
}

#' Function to plot three variables in 3d plot
#'
#' Creates a 3d plot using the scatter3D function from the plot3D package
#' Predictions from model can be provided as well to plot the prediction plane
#'
#' @param x1 Values for first independent variable (x-parameter)
#' @param x1_label Label for first independent variable
#' @param x2 Values for second independent variable (y-parameter)
#' @param x2_label Label for second independent variable
#' @param y Values for dependent variable (z-paramter)
#' @param y_label Label for dependent variable
#' @param intercept Coefficient for the intercept of the linear model
#' @param x1_coef Coefficient for x1 of the linear model
#' @param x1_coef Coefficient for x2 of the linear model
#' @param grid.lines Number of gridlines for the prediction plane
#' @param theta Angle 1 (default 30)
#' @param phi Angle 2 (default 30)
#'
#' @export
#'
#' @examples
#'

plot_2vars <- function(x1, x1_label, x2, x2_label, y, y_label,
                       intercept = NULL, x1_coef = NULL, x2_coef = NULL,
                       grid.lines = 20, theta = 30, phi = 30){
  require(plot3D)

  # Define plotting parameters for surface (if intercept, x1_coef, and x2_coef are specified)
  if(!is.null(intercept) & !is.null(x1_coef) & !is.null(x1_coef)){
    # Create grid
    x1.pred <- seq(from = min(x1), to = max(x1), length.out = grid.lines)
    x2.pred <- seq(from = min(x2), to = max(x2), length.out = grid.lines)
    x1x2 <- expand.grid(x1 = x1.pred, x2 = x2.pred)
    # Calculate predicted values for grid (crosspoints)
    y.grid.pred <- matrix(intercept + x1x2$x1 * x1_coef + x1x2$x2 * x2_coef,
                          nrow = grid.lines, ncol = grid.lines)
    # Calculate predicted values for x1 and x2
    y.pred <- intercept + x1 * x1_coef + x2 * x2_coef

    # Define color of dots based on predicted values
    col.var = y - y.pred

    # Define surface parameters
    params.surf = list(surf = list(x = x1.pred, y = x2.pred, z = y.grid.pred,
                                   facets = NA, fit = y.pred, col = "grey60"))
  } else {
    params.surf <- NULL
    col.var <- rep(1, length(y))
  }

  # Define plotting parameters for dots
  params.dots <- list(x = x1, y = x2, z = y, pch = 19, cex = 0.5,
                      xlab = x1_label, ylab = x2_label, zlab = y_label,
                      col = if_else(col.var >= 0, "black", "red"),
                      colvar = NULL,
                      colkey = FALSE,
                      theta = theta, phi = phi,
                      labels = paste(round(x1, 2), round(x2, 2), round(y, 2), sep = ", "))

  # Plot dots and optionally surface (if params.surf is not null)
  old.par <- par(no.readonly = TRUE)
  on.exit(par(old.par))

  par(mar = c(2,1,1,1))
  if(is.null(params.surf)){
    do.call(scatter3D, c(params.dots))
  } else {
    do.call(scatter3D, c(params.dots, params.surf, bty = "b2"))
  }
}
