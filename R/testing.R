#' Execute the given configurations on the testing instances specified in the
#' scenario
#'
#' @inheritParams removeConfigurationsMetaData
#' @inheritParams defaultScenario
#'
#' @return A list with the following elements:
#'   \describe{
#'     \item{\code{experiments}}{Experiments results.}
#'     \item{\code{seeds}}{Array of the instance seeds used in the experiments.}
#'   }
#'
#' @details A test instance set must be provided through `scenario[["testInstances"]]`.
#'
#' @seealso
#'  [testing_fromlog()]
#' 
#' @author Manuel López-Ibáñez
#' @export
testConfigurations <- function(configurations, scenario)
{
  # We need to set up a default scenario (and repeat all checks) in case
  # we are called directly instead of being called after executing irace.
  user_n_obj <- scenario$n_objectives 
  scenario <- checkScenario(scenario)
  if(is.null(scenario$n_objectives) && !is.null(user_n_obj)) scenario$n_objectives <- user_n_obj
  
  testInstances <- scenario[["testInstances"]]
  instances_id <- names(testInstances)
  if (length(testInstances) == 0L) irace_error("No test instances given")
  if (is.null(instances_id)) irace_error("testInstances must have names")
  
  # 2147483647 is the maximum value for a 32-bit signed integer.
  # We use replace = TRUE, because replace = FALSE allocates memory for each possible number.
  ## FIXME: scenario[["testInstances"]] and scenario$instances behave differently,
  ## we should unify them so that the seeds are also saved in scenario.
  instanceSeed <- runif_integer(length(testInstances))
  names(instanceSeed) <- instances_id
  
  # If there is no ID (e.g., after using readConfigurations), then add it.
  if (".ID." %not_in% colnames(configurations))
    configurations[[".ID."]] <- seq_nrow(configurations)
  
  # Create experiment list
  experiments <- createExperimentList(configurations, parameters = scenario$parameters,
    instances = testInstances, instances_ID = instances_id, seeds = instanceSeed,
    # We cannot use rep.int because scenario$boundMax may be NULL.
    bounds = rep(scenario$boundMax, nrow(configurations)))
  race_state <- RaceState$new(scenario)
  if (scenario$debugLevel >= 3L) {
    irace_note ("Memory used before execute_experiments() in testConfigurations():\n")
    race_state$print_mem_used()
  }
  race_state$start_parallel(scenario)
  on.exit(race_state$stop_parallel())
  # We cannot let targetRunner or targetEvaluator modify our random seed, so we save it.
  withr::local_preserve_seed()
  target_output <- execute_experiments(race_state, experiments, scenario)
  # targetEvaluator may be NULL. If so, target_output must contain the right
  # output already.
  if (!is.null(scenario$targetEvaluator))
    target_output <- execute_evaluator(race_state$target_evaluator, experiments,
      scenario, target_output)

  # FIXME: It would be much faster to convert target_output to a data.table like we do in race_wrapper(),
  # then dcast() to a matrix like we do elsewhere.

  n_objectives <- if(!is.null(scenario$n_objectives)) as.integer(scenario$n_objectives) else 1L
  
  row_ids_lookup <- as.vector(as.character(instances_id))
  col_ids_lookup <- as.vector(as.character(configurations$.ID.))

  if (n_objectives > 1) {
    testResults <- vector("list", n_objectives)
  } else {
    testResults <- vector("list", 1) 
  }


  for (k in 1:length(testResults)) {
      mat <- base::matrix(NA_real_, 
                          nrow = length(testInstances), 
                          ncol = nrow(configurations))
      rownames(mat) <- row_ids_lookup
      colnames(mat) <- col_ids_lookup
      testResults[[k]] <- mat
  }
  ## testResults <- matrix(NA, ncol = nrow(configurations), nrow = length(testInstances),
  ##                       # dimnames = list(rownames, colnames)
  ##                       dimnames = list(instances_id, configurations$.ID.))
  ## cost <- unlist_element(target_output, "cost")


  ## if (scenario$capping)
  ##   cost <- applyPAR(cost, boundMax = scenario$boundMax, boundPar = scenario$boundPar)
  # FIXME: Vectorize this loop
  for (i in seq_along(experiments)) {
    val <- target_output[[i]]$cost
    if (is.list(val)) val <- unlist(val)

    if (scenario$capping && length(val) == 1)
       val <- applyPAR(val, boundMax = scenario$boundMax, boundPar = scenario$boundPar)

    r_name <- as.character(experiments[[i]]$id_instance)
    c_name <- as.character(experiments[[i]]$id_configuration)
    r_idx <- match(r_name, row_ids_lookup)
    c_idx <- match(c_name, col_ids_lookup)
    
    if (is.na(r_idx) || is.na(c_idx)) next 

    for (k in 1:length(testResults)) {
         val_k <- if(length(val) >= k) val[k] else NA
         testResults[[k]][r_idx, c_idx] <- val_k
    }
    ## testResults[rownames(testResults) == experiments[[i]]$id_instance,
    ##             colnames(testResults) == experiments[[i]]$id_configuration] <- cost[i]
  }
  final_output <- if(n_objectives == 1) testResults[[1]] else testResults

  if (scenario$debugLevel >= 3L) {
    irace_note ("Memory used at the end of testConfigurations():\n")
    race_state$print_mem_used()
  }

  ## FIXME: Shouldn't we record these experiments in experiment_log ?
  list(experiments = final_output, seeds = instanceSeed)
  #list(experiments = testResults, seeds = instanceSeed)
}
