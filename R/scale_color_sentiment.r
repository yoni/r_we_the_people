#' Adds a manual color scale for sentiment polarity.
#' @export
scale_color_sentiment <- function()
  scale_color_manual(values=c('red', 'black', 'green'))
