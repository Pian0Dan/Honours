library(Matrix)
library(gurobi)

LEXIMIN_EPS <- 0.0005

read_leximin_instance <- function(data_dir, k = NULL) {
  categories_path <- file.path(data_dir, "categories.csv")
  respondents_path <- file.path(data_dir, "respondents.csv")

  if (!file.exists(categories_path)) {
    stop("Could not find categories.csv in ", data_dir, call. = FALSE)
  }
  if (!file.exists(respondents_path)) {
    stop("Could not find respondents.csv in ", data_dir, call. = FALSE)
  }

  categories <- read.csv(categories_path, stringsAsFactors = FALSE, check.names = FALSE)
  respondents <- read.csv(respondents_path, stringsAsFactors = FALSE, check.names = FALSE)

  required_category_columns <- c("category", "feature", "min", "max")
  missing_category_columns <- setdiff(required_category_columns, names(categories))
  if (length(missing_category_columns) > 0) {
    stop("categories.csv is missing columns: ",
         paste(missing_category_columns, collapse = ", "), call. = FALSE)
  }

  category_names <- unique(categories$category)
  missing_respondent_columns <- setdiff(category_names, names(respondents))
  if (length(missing_respondent_columns) > 0) {
    stop("respondents.csv is missing category columns: ",
         paste(missing_respondent_columns, collapse = ", "), call. = FALSE)
  }

  if (is.null(k)) {
    dir_name <- basename(normalizePath(data_dir, mustWork = FALSE))
    matches <- regmatches(dir_name, regexec("_(\\d+)$", dir_name))[[1]]
    if (length(matches) == 2) {
      k <- as.integer(matches[2])
    } else {
      stop("Please provide k, or use a data directory ending in _<panel_size>.",
           call. = FALSE)
    }
  }

  list(
    k = as.integer(k),
    categories = categories,
    respondents = respondents,
    category_names = category_names,
    agent_ids = seq_len(nrow(respondents))
  )
}

feature_rows <- function(instance) {
  rows <- instance$categories
  rows$min <- as.integer(rows$min)
  rows$max <- as.integer(rows$max)
  rows
}

make_committee_key <- function(committee) {
  paste(sort(as.integer(committee)), collapse = ",")
}

committee_from_solution <- function(x) {
  which(x > 0.5)
}

build_committee_model <- function(instance, weights = rep(0, length(instance$agent_ids))) {
  n <- length(instance$agent_ids)
  rows_i <- integer()
  cols_j <- integer()
  vals_x <- numeric()
  senses <- character()
  rhs <- numeric()
  row_index <- 0L

  add_row <- function(cols, vals, sense, right_hand_side) {
    row_index <<- row_index + 1L
    rows_i <<- c(rows_i, rep(row_index, length(cols)))
    cols_j <<- c(cols_j, cols)
    vals_x <<- c(vals_x, vals)
    senses <<- c(senses, sense)
    rhs <<- c(rhs, right_hand_side)
  }

  add_row(seq_len(n), rep(1, n), "=", instance$k)

  quotas <- feature_rows(instance)
  for (r in seq_len(nrow(quotas))) {
    category <- quotas$category[r]
    feature <- quotas$feature[r]
    matching_agents <- which(instance$respondents[[category]] == feature)
    add_row(matching_agents, rep(1, length(matching_agents)), ">", quotas$min[r])
    add_row(matching_agents, rep(1, length(matching_agents)), "<", quotas$max[r])
  }

  list(
    modelsense = "max",
    obj = as.numeric(weights),
    A = sparseMatrix(i = rows_i, j = cols_j, x = vals_x, dims = c(row_index, n)),
    sense = senses,
    rhs = rhs,
    vtype = rep("B", n),
    lb = rep(0, n),
    ub = rep(1, n)
  )
}

solve_committee <- function(instance, weights, params) {
  model <- build_committee_model(instance, weights)
  result <- gurobi(model, params)
  if (result$status != "OPTIMAL") {
    stop("No feasible committee found. Gurobi status: ", result$status, call. = FALSE)
  }
  committee_from_solution(result$x)
}

generate_initial_committees <- function(instance, params, rounds = 3 * length(instance$agent_ids)) {
  n <- length(instance$agent_ids)
  weights <- rep(1, n)
  committees <- list()
  committee_keys <- character()
  covered <- rep(FALSE, n)

  add_committee <- function(committee) {
    key <- make_committee_key(committee)
    if (!(key %in% committee_keys)) {
      committees[[length(committees) + 1L]] <<- committee
      committee_keys <<- c(committee_keys, key)
      covered[committee] <<- TRUE
      TRUE
    } else {
      FALSE
    }
  }

  for (round in seq_len(rounds)) {
    committee <- solve_committee(instance, weights, params)
    is_new <- add_committee(committee)
    weights[committee] <- weights[committee] * 0.8
    weights <- weights * n / sum(weights)
    if (!is_new) {
      weights <- 0.9 * weights + 0.1
    }
    message("Multiplicative weights phase, round ", round, "/", rounds,
            ". Discovered ", length(committees), " committees so far.")
  }

  for (agent in which(!covered)) {
    weights <- rep(0, n)
    weights[agent] <- 1
    committee <- solve_committee(instance, weights, params)
    if (agent %in% committee) {
      add_committee(committee)
    } else {
      message("Agent ", agent, " is not contained in any feasible committee.")
    }
  }

  if (all(covered)) {
    message("All agents are contained in some feasible committee.")
  }

  committees
}

solve_dual_stage <- function(n, committees, fixed_probabilities, params) {
  fixed_agents <- as.integer(names(fixed_probabilities))
  unfixed_agents <- setdiff(seq_len(n), fixed_agents)
  cap_col <- n + 1L

  rows_i <- integer()
  cols_j <- integer()
  vals_x <- numeric()
  senses <- character()
  rhs <- numeric()
  row_index <- 0L

  add_row <- function(cols, vals, sense, right_hand_side) {
    row_index <<- row_index + 1L
    rows_i <<- c(rows_i, rep(row_index, length(cols)))
    cols_j <<- c(cols_j, cols)
    vals_x <<- c(vals_x, vals)
    senses <<- c(senses, sense)
    rhs <<- c(rhs, right_hand_side)
  }

  add_row(unfixed_agents, rep(1, length(unfixed_agents)), "=", 1)
  for (committee in committees) {
    add_row(c(committee, cap_col), c(rep(1, length(committee)), -1), "<", 0)
  }

  obj <- rep(0, n + 1L)
  if (length(fixed_agents) > 0) {
    obj[fixed_agents] <- -as.numeric(fixed_probabilities)
  }
  obj[cap_col] <- 1

  model <- list(
    modelsense = "min",
    obj = obj,
    A = sparseMatrix(i = rows_i, j = cols_j, x = vals_x,
                     dims = c(row_index, n + 1L)),
    sense = senses,
    rhs = rhs,
    vtype = rep("C", n + 1L),
    lb = rep(0, n + 1L)
  )

  gurobi(model, params)
}

solve_final_distribution <- function(n, committees, fixed_probabilities, params) {
  m <- length(committees)
  eps_col <- m + 1L
  fixed_agents <- as.integer(names(fixed_probabilities))

  rows_i <- integer()
  cols_j <- integer()
  vals_x <- numeric()
  senses <- character()
  rhs <- numeric()
  row_index <- 0L

  add_row <- function(cols, vals, sense, right_hand_side) {
    row_index <<- row_index + 1L
    rows_i <<- c(rows_i, rep(row_index, length(cols)))
    cols_j <<- c(cols_j, cols)
    vals_x <<- c(vals_x, vals)
    senses <<- c(senses, sense)
    rhs <<- c(rhs, right_hand_side)
  }

  add_row(seq_len(m), rep(1, m), "=", 1)
  for (agent in fixed_agents) {
    containing <- which(vapply(committees, function(committee) agent %in% committee, logical(1)))
    add_row(c(containing, eps_col), c(rep(1, length(containing)), 1), ">",
            fixed_probabilities[as.character(agent)])
  }

  obj <- rep(0, m + 1L)
  obj[eps_col] <- 1
  model <- list(
    modelsense = "min",
    obj = obj,
    A = sparseMatrix(i = rows_i, j = cols_j, x = vals_x,
                     dims = c(row_index, m + 1L)),
    sense = senses,
    rhs = rhs,
    vtype = rep("C", m + 1L),
    lb = rep(0, m + 1L)
  )

  result <- gurobi(model, params)
  if (result$status != "OPTIMAL") {
    stop("Could not find a distribution over committees. Gurobi status: ",
         result$status, call. = FALSE)
  }

  probabilities <- pmin(pmax(result$x[seq_len(m)], 0), 1)
  probabilities / sum(probabilities)
}

panel_probabilities_data_frame <- function(committees, committee_probabilities) {
  #keep <- which(committee_probabilities > LEXIMIN_EPS)
  keep <- which(committee_probabilities > 0)
  data.frame(
    panel_id = seq_along(keep),
    panel_probability = committee_probabilities[keep],
    respondent_indices = vapply(
      committees[keep],
      function(committee) paste(sort(committee), collapse = ","),
      character(1)
    )
  )
}

leximin_distribution <- function(instance, rounds = 3 * length(instance$agent_ids),
                                 output_flag = 0, eps = LEXIMIN_EPS) {
  n <- length(instance$agent_ids)
  params <- list(OutputFlag = output_flag)
  barrier_params <- list(OutputFlag = output_flag, Method = 2, Crossover = 0)

  # First solve once to give a clear infeasibility error before column generation.
  invisible(solve_committee(instance, rep(0, n), params))

  committees <- generate_initial_committees(instance, params, rounds)
  committee_keys <- vapply(committees, make_committee_key, character(1))
  fixed_probabilities <- numeric()

  while (length(fixed_probabilities) < n) {
    message("Fixed ", length(fixed_probabilities), "/", n, " probabilities.")

    repeat {
      dual <- solve_dual_stage(n, committees, fixed_probabilities, barrier_params)
      if (dual$status != "OPTIMAL") {
        if (length(fixed_probabilities) == 0) {
          stop("Dual LP failed with status: ", dual$status, call. = FALSE)
        }
        fixed_probabilities <- pmax(0, fixed_probabilities - 0.0001)
        next
      }

      agent_weights <- dual$x[seq_len(n)]
      new_committee <- solve_committee(instance, agent_weights, params)
      value <- sum(agent_weights[new_committee])
      upper <- dual$x[n + 1L]
      dual_obj <- dual$objval

      message(sprintf("Maximin is at most %.2f%%, can do %.2f%% with %d committees. Gap %.2f%%.",
                      100 * (dual_obj - upper + value), 100 * dual_obj,
                      length(committees), 100 * (value - upper)))

      if (value <= upper + eps) {
        newly_fixed <- which(agent_weights > eps &
                               !(seq_len(n) %in% as.integer(names(fixed_probabilities))))
        if (length(newly_fixed) == 0) {
          stop("No new probabilities were fixed; try increasing eps slightly.",
               call. = FALSE)
        }
        new_values <- rep(max(0, dual_obj), length(newly_fixed))
        names(new_values) <- as.character(newly_fixed)
        fixed_probabilities <- c(fixed_probabilities, new_values)
        break
      }

      key <- make_committee_key(new_committee)
      if (!(key %in% committee_keys)) {
        committees[[length(committees) + 1L]] <- new_committee
        committee_keys <- c(committee_keys, key)
      }
    }
  }

  committee_probabilities <- solve_final_distribution(n, committees, fixed_probabilities, params)
  selection_probabilities <- rep(0, n)
  for (j in seq_along(committees)) {
    selection_probabilities[committees[[j]]] <-
      selection_probabilities[committees[[j]]] + committee_probabilities[j]
  }

  respondent_probabilities <- data.frame(
    respondent_index = seq_len(n),
    selection_probability = selection_probabilities
  )

  panel_probabilities <- panel_probabilities_data_frame(committees, committee_probabilities)

  list(
    respondent_probabilities = respondent_probabilities,
    panel_probabilities = panel_probabilities,
    committees = committees,
    committee_probabilities = committee_probabilities
  )
}

leximin_probabilities <- function(instance, rounds = 3 * length(instance$agent_ids),
                                  output_flag = 0, eps = LEXIMIN_EPS) {
  leximin_distribution(instance, rounds = rounds, output_flag = output_flag,
                       eps = eps)$respondent_probabilities
}

run_leximin <- function(data_dir, output_csv = NULL, panel_output_csv = NULL,
                        k = NULL, rounds = NULL, output_flag = 0) {
  instance <- read_leximin_instance(data_dir, k)
  if (is.null(rounds)) {
    rounds <- 3 * length(instance$agent_ids)
  }

  distribution <- leximin_distribution(instance, rounds = rounds,
                                       output_flag = output_flag)
  probabilities <- distribution$respondent_probabilities
  if (!is.null(output_csv)) {
    write.csv(probabilities, output_csv, row.names = FALSE)
  }
  if (!is.null(panel_output_csv)) {
    write.csv(distribution$panel_probabilities, panel_output_csv, row.names = FALSE)
  }
  probabilities
}


data_dir <- "data/example_small_20"
output_csv <- 'example_small_20_output.csv'
panel_output_csv <- 'example_small_20_panel_output.csv'

#data_dir <- "data/example_large_200"  # This will take a few hours
#output_csv <- 'example_large_200_output.csv'
#panel_output_csv <- 'example_large_200_panel_output.csv'
k <- 200 # k is panel size, if not found it will try to read it from the data_dir
output_flag = 1
run_leximin(data_dir,output_csv, panel_output_csv, k, output_flag = output_flag )
