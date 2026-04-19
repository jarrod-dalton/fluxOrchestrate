priority_pid <- function(priority, model_id, sub_pid) {
  stopifnot(length(priority) == 1L, length(model_id) == 1L, length(sub_pid) == 1L)
  priority <- as.integer(priority)
  if (is.na(priority)) stop("priority must be coercible to integer.")
  model_id <- as.character(model_id)
  sub_pid  <- as.character(sub_pid)
  sprintf("%06d|%s|%s", priority, model_id, sub_pid)
}

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
