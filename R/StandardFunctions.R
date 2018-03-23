#' Write to clipboard
#'
#' Standard separator is tab and row.names = FALSE
#'
#' @param dataset The dataset to write as a table to the clipboard (MAC)
#'
#' @export
#'
#' @examples
#' write.clipboard (table1)

## Clipboard

write.clipboard <- function(dataset, row.names = FALSE){
  write.table(dataset, con <- pipe("pbcopy", "w"), sep = "\t", row.names = row.names)
  close (con)
}


#' transf.hist
#'
#' Creates different histograms of a variable including non-transformed values
#' sqrt(x+0.1) and log(x+0.1) transformed values
#'
#' @param add.value The value added to x before transformation (standard = 0.1)
#' @param breaks Number of breaks (standard 6)
#'
#' @export
#'

transf.hist <- function(x, add.value = 0.1) {
  op <- par(no.readonly = T)
  par(mfrow = c(1,3))
  hist(x, main = "Not transformed")
  hist(log(x + add.value), main = "log-transformed")
  hist(sqrt(x + add.value), main = "sqrt-transformed")
  ?hist
  on.exit(par(op))
}

#' resetPar
#'
#' Resets all graphical parameters without starting a new session
#'
#'
#' @export
#'
#'@examples
#' par(resetPar())

resetPar <- function() {
  dev.new()
  op <- par(no.readonly = TRUE)
  dev.off()
  op
}
