#' Strictly merge patient schemas
#'
#' Schemas in patientSimCore are named lists where each element is a list describing a
#' state variable (type/levels/default/coerce/validate/blocks/etc.). This helper merges
#' multiple schemas and errors on any conflicting definition for a shared variable name.
#'
#' @param ... Named or unnamed schema objects (named lists).
#' @return A merged schema (named list).
#' @export
merge_schemas_strict <- function(...) {
  schemas <- list(...)
  if (length(schemas) == 1L && is.list(schemas[[1]]) && is.null(names(schemas))) {
    # allow merge_schemas_strict(list_of_schemas)
    schemas <- schemas[[1]]
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
    if (is.null(s) || !is.list(s)) stop("Each schema must be a named list (like default_patient_schema()).")
    if (is.null(names(s)) || any(names(s) == "")) stop("Each schema must be a named list with non-empty names.")

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
