is_step_unlocked <- function(step_id, state) {
  step_id %in% state$unlocked_steps
}

is_step_visited <- function(step_id, state) {
  step_id %in% state$visited_steps
}

unlock_step <- function(step_id, state) {
  state$unlocked_steps <- unique(c(state$unlocked_steps, step_id))
  invisible(state$unlocked_steps)
}

visit_step <- function(step_id, state) {
  state$visited_steps <- unique(c(state$visited_steps, step_id))
  invisible(state$visited_steps)
}

unlock_next_steps <- function(step, state) {
  if (length(step$next_steps) > 0) {
    state$unlocked_steps <- unique(c(state$unlocked_steps, step$next_steps))
  }

  invisible(state$unlocked_steps)
}
