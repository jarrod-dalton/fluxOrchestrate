test_that("orchestrator passes current_proposals to submodels by sub_pid", {
  seen <- new.env(parent = emptyenv())
  echo_model <- list(
    name = "echo",
    schema = list(),
    propose_events = function(entity, ctx=NULL, process_ids=NULL, current_proposals=NULL) {
      seen$current <- current_proposals
      if (!is.null(current_proposals) && !is.null(current_proposals[["p"]])) {
        return(list(p = current_proposals[["p"]]))
      }
      list(p = list(event_type="x", time_next = 1))
    },
    transition = function(entity, event, ctx=NULL) list(),
    stop = function(entity, event=NULL, ctx=NULL) FALSE
  )

  b <- orchestrated_bundle(
    models = list(echo = echo_model),
    policy = list(event_priority = function(proposal, entity, ctx=NULL) 123L)
  )

  p <- fluxCore::new_entity(init = list(), schema = b$schema, time0 = 0)

  orch_pid <- priority_pid(123, "echo", "p")
  cur <- list()
  cur[[orch_pid]] <- list(event_type="x", time_next = 5)

  props <- b$propose_events(p, current_proposals = cur)
  expect_true(!is.null(seen$current))
  expect_true(!is.null(seen$current[["p"]]))
  expect_equal(seen$current[["p"]]$time_next, 5)
  expect_equal(props[[orch_pid]]$time_next, 5)
})
