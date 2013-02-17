#' Melts the petitions data.frame, creating multiple rows for each petition issue.
#' @param petitions data.frame loaded using load_petitions
#' @return melted data.frame with one row per issue
#' @export
melt_issues <- function(petitions) {
  issue_name_fields <- grep("issues.?.name",names(petitions), value=TRUE)
  melted_petitions <- melt(petitions, measure.vars=issue_name_fields)
  melted_petitions$issue <- melted_petitions$value
  melted_petitions$variable <- NULL
  melted_petitions$value <- NULL
  issue_id_fields <- grep("issues.?.id",names(petitions), value=TRUE)
  melted_petitions <- melted_petitions[, !(names(melted_petitions) %in% issue_id_fields)]
  unique(subset(melted_petitions, is.na(issue) == FALSE))
}
