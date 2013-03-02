#' Generates plots of wordclouds for each petition.
#' @param petitions data.frame of petitions, e.g. loaded from client$petitions()
#' @param path the path to write the images to
#' @param column the column to use as the input text
#' @export
#' @examples
#' data(petitions)
#' one_petition <- head(petitions, n=1)
#' generate_wordcloud_plots(one_petition, tempdir(), 'body')
generate_wordcloud_plots <- function(petitions, path, column) {
  ddply(
    petitions,
    .(id),
    .fun = function(p) {
      filename <- sprintf("petition_wordcloud_%s.png", unique(p$id))
      full_path <- file.path(path, filename)
      png(full_path)
      plot_wordcloud(p, column)
      dev.off()
    }
  )
}
