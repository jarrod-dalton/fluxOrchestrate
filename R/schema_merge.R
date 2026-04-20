merge_schemas_strict <- function(...) {
  schemas <- list(...)

  is_schema <- function(x) {
    # A schema is a *named* list whose entries are variable specs (lists)
    # containing at least $default and $type.
    is.list(x) && !is.null(names(x)) && length(x) > 0L &&
      all(vapply(x, is.list, logical(1))) &&
      all(vapply(x, function(spec) !is.null(spec$default), logical(1))) &&
      all(vapply(x, function(spec) !is.null(spec$type), logical(1)))
  }

  if (length(schemas) == 1L && is.list(schemas[[1]]) && is.null(names(schemas))) {
    # Allow merge_schemas_strict(list_of_schemas) where list_of_schemas is a list
    # of *schemas* (each itself a named list of variable specs). Do not unwrap
    # when a single schema is provided.
    if (!is_schema(schemas[[1]]) && length(schemas[[1]]) > 0L && all(vapply(schemas[[1]], is_schema, logical(1)))) {
      schemas <- schemas[[1]]
    }
  }

  if (!length(schemas)) stop("No schemas provided.")

  out <- list()

  deep_equal <- function(a, b) {
    # Best-effort deep equality without pulling in extra dependencies.
    # Note: functions (e.g., validate) cannot be safely serialized, so we compare their deparse.
    normalize <- function(x) {
      if (is.function(x)) return(paste(deparse(x), collapse = "\n"))
      if (is.list(x)) return(lapply(x, normalize))
      x
    }
    identical(normalize(a), normalize(b))
  }

  for (s in schemas) {
    if (is.null(s) || !is.list(s)) {
      stop("Each schema must be a named list (like default_entity_schema()).")
    }

    # Centralize schema contract enforcement in fluxCore.
    # This also normalizes type metadata (e.g., case, allowed set).
    s <- fluxCore::schema_validate(s)

    for (nm in names(s)) {
      if (is.null(out[[nm]])) {
        out[[nm]] <- s[[nm]]
      } else {
        if (!deep_equal(out[[nm]], s[[nm]])) {
          stop(sprintf("Schema conflict for variable '%s'. Definitions differ.", nm))
        }
      }
    }
  }

  out
}
