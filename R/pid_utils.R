#' Build a precedence-encoded process ID
#'
#' Encodes priority and routing metadata into a stable process ID string used by
#' orchestrated bundles.
#'
#' @param priority Integer priority (lower values sort earlier).
#' @param model_id Model identifier string.
#' @param sub_pid Underlying process ID string within the model.
#'
#' @return A character scalar process ID in encoded form.
#' @export
priority_pid <- function(priority, model_id, sub_pid) {
  stopifnot(length(priority) == 1L, length(model_id) == 1L, length(sub_pid) == 1L)
  priority <- as.integer(priority)
  if (is.na(priority)) stop("priority must be coercible to integer.")
  model_id <- as.character(model_id)
  sub_pid  <- as.character(sub_pid)
  sprintf("%06d|%s|%s", priority, model_id, sub_pid)
}

#' Parse a precedence-encoded process ID
#'
#' Decodes an orchestrated process ID back into priority and routing components.
#'
#' @param pid Character scalar produced by `priority_pid()`.
#'
#' @return A list with components `priority`, `model_id`, and `sub_pid`.
#' @export
parse_priority_pid <- function(pid) {
  stopifnot(length(pid) == 1L)
  pid <- as.character(pid)
  parts <- strsplit(pid, "\\|", fixed = FALSE)[[1]]
  if (length(parts) < 3L) stop("pid does not appear to be a precedence-encoded process id.")
  list(
    priority = as.integer(parts[[1]]),
    model_id = parts[[2]],
    sub_pid  = paste(parts[3:length(parts)], collapse = "|")
  )
}
