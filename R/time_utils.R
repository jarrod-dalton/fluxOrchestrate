# ------------------------------------------------------------------------------
# Numeric model-time validation helpers
# ------------------------------------------------------------------------------

.pso_time_unit_label <- function(sim_ctx = NULL) {
  if (!is.null(sim_ctx) && is.list(sim_ctx) &&
      !is.null(sim_ctx$time_spec) && is.list(sim_ctx$time_spec)) {
    u <- sim_ctx$time_spec$unit
    if (is.character(u) && length(u) == 1L && nzchar(u)) return(as.character(u))
  }
  "unitless"
}

.pso_time_spec_equal <- function(x, y) {
  if (!inherits(x, "time_spec") || !inherits(y, "time_spec")) return(FALSE)

  identical(x$unit, y$unit) &&
    identical(x$origin_class, y$origin_class) &&
    identical(x$zone, y$zone) &&
    isTRUE(all.equal(
      as.numeric(x$origin_posix),
      as.numeric(y$origin_posix),
      tolerance = 0
    ))
}

.pso_assert_numeric_scalar <- function(x, name, sim_ctx = NULL) {
  if (inherits(x, "Date") || inherits(x, c("POSIXct", "POSIXt"))) {
    unit <- .pso_time_unit_label(sim_ctx)
    stop(
      sprintf(
        "%s must be numeric model time (unit: '%s'). Calendar time inputs are out of scope for fluxOrchestrate.",
        name, unit
      ),
      call. = FALSE
    )
  }
  if (!is.numeric(x) || length(x) != 1L || !is.finite(x)) {
    unit <- .pso_time_unit_label(sim_ctx)
    stop(sprintf("%s must be a finite numeric scalar (model time; unit: '%s').", name, unit), call. = FALSE)
  }
  invisible(TRUE)
}

.pso_assert_proposal_time_next <- function(proposal, sim_ctx = NULL) {
  if (is.null(proposal) || !is.list(proposal)) {
    stop("proposal must be a list.", call. = FALSE)
  }
  if (is.null(proposal$time_next)) {
    stop("proposal must include a scalar field 'time_next'.", call. = FALSE)
  }
  .pso_assert_numeric_scalar(proposal$time_next, name = "proposal$time_next", sim_ctx = sim_ctx)
  invisible(TRUE)
}
