#' Create an orchestration bundle over multiple model bundles
#'
#' Returns a ModelBundle-compatible list of functions (per patientSimCore) that composes
#' multiple underlying bundles on a shared patient timeline. Orchestration is implemented
#' as an add-on wrapper: the Core Engine remains unchanged.
#'
#' Core idea: sub-models *propose* candidate next events. The engine selects the single
#' next event globally (earliest time; deterministic tie-break by `process_id`). Orchestration controls:
#' - which models are allowed to propose (eligibility gating)
#' - deterministic precedence among same-time events (priority encoding into `process_id`)
#' - optional cross-model payload updates after transitions (policy-driven)
#'
#' @param models Named list of model bundles (each bundle is a list with patientSimCore bundle functions).
#' @param policy List of callbacks. Any missing callbacks fall back to defaults.
#'   - `eligible_models(patient, ctx)` -> character vector of model ids (subset of names(models))
#'   - `event_priority(proposal, patient, ctx)` -> integer (lower = higher priority)
#'   - `on_transition(event, patient, ctx, model_changes)` -> additional changes (named list)
#'   - `stop(patient, event, ctx, per_model_stop)` -> TRUE/FALSE
#' @param schema Optional universal schema. If NULL, merges core default schema with model schemas when available.
#' @return A ModelBundle-compatible list.
#' @export
orchestrated_bundle <- function(models,
                               policy = NULL,
                               schema = NULL) {
  if (!is.list(models) || is.null(names(models)) || any(names(models) == "")) {
    stop("models must be a named list of bundles.")
  }

  pol_default <- list(
    eligible_models = function(patient, ctx = NULL) names(models),
    event_priority  = function(proposal, patient, ctx = NULL) 500L,
    on_transition   = function(event, patient, ctx = NULL, model_changes) list(),
    stop            = function(patient, event = NULL, ctx = NULL, per_model_stop = NULL) {
      alive <- NA
      st <- tryCatch(patient$as_list("alive"), error = function(e) NULL)
      if (!is.null(st) && "alive" %in% names(st)) alive <- st$alive
      if (!is.na(alive) && !isTRUE(alive)) return(TRUE)

      if (is.null(per_model_stop)) return(FALSE)
      eligible <- names(per_model_stop)
      if (!length(eligible)) return(FALSE)
      all(unlist(per_model_stop[eligible], use.names = FALSE))
    }
  )

  if (is.null(policy)) policy <- list()
  if (!is.list(policy)) stop("policy must be a list.")
  pol <- modifyList(pol_default, policy)

  if (is.null(schema)) {
    core_schema <- patientSimCore::default_patient_schema()
    model_schemas <- lapply(models, function(b) {
  s <- NULL
  if (is.list(b) && !is.null(b$schema)) s <- b$schema
  if (is.null(s)) return(NULL)
  # allow models to omit schema (or provide empty list) without breaking orchestration
  if (!is.list(s) || !length(s)) return(NULL)
  if (is.null(names(s)) || any(names(s) == "")) return(NULL)
  s
})
model_schemas <- Filter(Negate(is.null), model_schemas)

schema <- merge_schemas_strict(c(list(core_schema), model_schemas))
  }

  split_by_model <- function(x_named) {
    if (is.null(x_named) || !length(x_named)) return(list())
    out <- list()
    for (pid in names(x_named)) {
      parts <- parse_priority_pid(pid)
      mid <- parts$model_id
      spid <- parts$sub_pid
      if (is.null(out[[mid]])) out[[mid]] <- list()
      out[[mid]][[spid]] <- x_named[[pid]]
    }
    out
  }

  filter_process_ids_by_model <- function(process_ids) {
    if (is.null(process_ids)) return(NULL)
    mids <- list()
    for (pid in process_ids) {
      parts <- parse_priority_pid(pid)
      if (is.null(mids[[parts$model_id]])) mids[[parts$model_id]] <- character()
      mids[[parts$model_id]] <- c(mids[[parts$model_id]], parts$sub_pid)
    }
    mids
  }

  add_meta <- function(prop, orch_pid, model_id, sub_pid, priority) {
    prop$model_id <- model_id
    prop$sub_pid  <- sub_pid
    prop$priority <- as.integer(priority)
    prop$process_id <- orch_pid
    prop
  }

  propose_events <- function(patient, ctx = NULL, process_ids = NULL, current_proposals = NULL) {
    # Orchestration operates strictly on numeric model time.
    .pso_assert_numeric_scalar(patient$last_time, name = "patient$last_time", ctx = ctx)

    eligible <- pol$eligible_models(patient, ctx)
    eligible <- intersect(eligible, names(models))
    if (!length(eligible)) return(list())

    current_by_model <- split_by_model(current_proposals)
    pids_by_model <- filter_process_ids_by_model(process_ids)

    out <- list()

    for (mid in eligible) {
      b <- models[[mid]]
      sub_current <- current_by_model[[mid]]
      sub_pids <- if (is.null(process_ids)) NULL else pids_by_model[[mid]]

      props <- b$propose_events(
        patient = patient,
        ctx = ctx,
        process_ids = sub_pids,
        current_proposals = sub_current
      )
      if (!length(props)) next

      for (spid in names(props)) {
        p <- props[[spid]]
        .pso_assert_proposal_time_next(p, ctx = ctx)
        pr <- pol$event_priority(p, patient, ctx)
        orch_pid <- priority_pid(pr, mid, spid)
        out[[orch_pid]] <- add_meta(p, orch_pid, mid, spid, pr)
      }
    }

    out
  }

  transition <- function(patient, event, ctx = NULL) {
    mid <- event$model_id
    spid <- event$sub_pid
    if (is.null(mid) || is.null(spid)) {
      if (!is.null(event$process_id)) {
        parts <- parse_priority_pid(event$process_id)
        mid <- parts$model_id
        spid <- parts$sub_pid
      } else {
        stop("Orchestrated events must include routing metadata (model_id/sub_pid) or process_id.")
      }
    }

    mid <- as.character(mid)
    if (!mid %in% names(models)) stop(sprintf("Unknown model_id '%s' in event.", mid))
    b <- models[[mid]]

    model_changes <- list()
    if (!is.null(b$transition)) {
      model_changes <- b$transition(patient = patient, event = event, ctx = ctx)
      if (is.null(model_changes)) model_changes <- list()
    }

    extra <- pol$on_transition(event, patient, ctx, model_changes)
    if (is.null(extra)) extra <- list()

    c(model_changes, extra)
  }

  stop <- function(patient, event = NULL, ctx = NULL) {
    eligible <- pol$eligible_models(patient, ctx)
    eligible <- intersect(eligible, names(models))

    per_model <- list()
    for (mid in eligible) {
      b <- models[[mid]]
      per_model[[mid]] <- if (!is.null(b$stop)) isTRUE(b$stop(patient = patient, event = event, ctx = ctx)) else FALSE
    }

    isTRUE(pol$stop(patient, event, ctx, per_model))
  }

  list(
    name = "orchestrated_bundle",
    schema = schema,
    propose_events = propose_events,
    transition = transition,
    stop = stop
  )
}