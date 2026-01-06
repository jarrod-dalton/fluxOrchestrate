#' Deterministic precedence encoding for process ids
#'
#' Utilities for encoding event precedence into `process_id` strings so the Core Engine's
#' deterministic tie-break (time_next, process_id) enforces a priority ordering without
#' requiring changes to patientSimCore.
#'
#' @param priority Integer. Lower values indicate higher priority.
#' @param model_id Character scalar identifying the model.
#' @param sub_pid Character scalar, the model-local process id.
#' @return Character scalar.
#' @export
priority_pid <- function(priority, model_id, sub_pid) {
  stopifnot(length(priority) == 1L, length(model_id) == 1L, length(sub_pid) == 1L)
  priority <- as.integer(priority)
  if (is.na(priority)) stop("priority must be coercible to integer.")
  model_id <- as.character(model_id)
  sub_pid  <- as.character(sub_pid)
  sprintf("%06d|%s|%s", priority, model_id, sub_pid)
}

#' Parse precedence-encoded process ids
#'
#' @param pid Character scalar produced by [priority_pid()].
#' @return A list with fields `priority`, `model_id`, `sub_pid`.
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
