#' Compose multiple model bundles into a single orchestrated bundle
#'
#' Builds an orchestration-layer bundle that routes proposals and transitions to
#' underlying model bundles, applies priority-based process IDs, and supports
#' cross-model policy hooks.
#'
#' @param models Named list of model bundles to orchestrate.
#' @param policy Optional list overriding orchestration hooks such as
#'   `eligible_models`, `event_priority`, `on_transition`, and `stop`.
#' @param schema Optional pre-merged schema. If `NULL`, schemas are merged from
#'   model bundle schemas.
#'
#' @return A model bundle list compatible with `fluxCore::Engine`.
#' @export
orchestrated_bundle <- function(models,
                               policy = NULL,
                               schema = NULL) {
  if (!is.list(models) || is.null(names(models)) || any(names(models) == "")) {
    stop("models must be a named list of bundles.")
  }

  pol_default <- list(
    eligible_models = function(entity, sim_ctx = NULL, param_ctx = NULL) names(models),
    event_priority  = function(proposal, entity, sim_ctx = NULL, param_ctx = NULL) 500L,
    on_transition   = function(event, entity, sim_ctx = NULL, param_ctx = NULL, model_changes) list(),
    stop            = function(entity, event = NULL, sim_ctx = NULL, param_ctx = NULL, per_model_stop = NULL) {
      if (is.null(per_model_stop)) return(FALSE)
      eligible <- names(per_model_stop)
      if (!length(eligible)) return(FALSE)
      all(unlist(per_model_stop[eligible], use.names = FALSE))
    }
  )

  if (is.null(policy)) policy <- list()
  if (!is.list(policy)) stop("policy must be a list.")
  pol <- utils::modifyList(pol_default, policy)

  if (is.null(schema)) {
    model_schemas <- lapply(models, function(b) {
      s <- NULL
      if (is.list(b) && !is.null(b$schema)) s <- b$schema
      if (is.null(s)) return(NULL)
      if (!is.list(s) || !length(s)) return(NULL)
      if (is.null(names(s)) || any(names(s) == "")) return(NULL)
      s
    })
    model_schemas <- Filter(Negate(is.null), model_schemas)
    if (length(model_schemas) == 0L) {
      stop("schema is NULL and no non-empty model schemas were supplied; provide `schema` or model bundle schemas.")
    }
    schema <- merge_schemas_strict(model_schemas)
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

  propose_events <- function(entity, sim_ctx = NULL, param_ctx = NULL, process_ids = NULL, current_proposals = NULL) {
    # Orchestration operates strictly on numeric model time.
    .pso_assert_numeric_scalar(entity$last_time, name = "entity$last_time", sim_ctx = sim_ctx)

    eligible <- pol$eligible_models(entity, sim_ctx, param_ctx)
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
        entity = entity,
        sim_ctx = sim_ctx,
        param_ctx = param_ctx,
        process_ids = sub_pids,
        current_proposals = sub_current
      )
      if (!length(props)) next

      for (spid in names(props)) {
        p <- props[[spid]]
        .pso_assert_proposal_time_next(p, sim_ctx = sim_ctx)
        pr <- pol$event_priority(p, entity, sim_ctx, param_ctx)
        orch_pid <- priority_pid(pr, mid, spid)
        out[[orch_pid]] <- add_meta(p, orch_pid, mid, spid, pr)
      }
    }

    out
  }

  transition <- function(entity, event, sim_ctx = NULL, param_ctx = NULL) {
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
      model_changes <- b$transition(entity = entity, event = event, sim_ctx = sim_ctx, param_ctx = param_ctx)
      if (is.null(model_changes)) model_changes <- list()
    }

    extra <- pol$on_transition(event, entity, sim_ctx, param_ctx, model_changes)
    if (is.null(extra)) extra <- list()

    c(model_changes, extra)
  }

  stop <- function(entity, event = NULL, sim_ctx = NULL, param_ctx = NULL) {
    eligible <- pol$eligible_models(entity, sim_ctx, param_ctx)
    eligible <- intersect(eligible, names(models))

    per_model <- list()
    for (mid in eligible) {
      b <- models[[mid]]
      per_model[[mid]] <- if (!is.null(b$stop)) isTRUE(b$stop(entity = entity, event = event, sim_ctx = sim_ctx, param_ctx = param_ctx)) else FALSE
    }

    isTRUE(pol$stop(entity, event, sim_ctx, param_ctx, per_model))
  }

  list(
    name = "orchestrated_bundle",
    schema = schema,
    propose_events = propose_events,
    transition = transition,
    stop = stop
  )
}
