library("lpSolveAPI")
library("igraph")
source("./utils.r")

fmt <- function(sol, node = 1) {
  obj_str <- sprintf("(%g)\nZ = %.2f\n", node, sol$z)

  for (i in seq_along(sol$x)) {
    tmp <- sprintf("x%s = %.2f\n", i, sol$x[[i]])
    obj_str <- paste(obj_str, tmp)
  }

  return(sprintf(obj_str))
}

print.BoundedSolution <- function(obj) {
  cat("Z =", obj$z, "\n")

  for (i in seq_along(obj$x)) {
    cat(sprintf("x%s =", i), obj$x[[i]], "\n")
  }
}

create_sol <- function(obj_val, dvars) {
#' @title Create an Instance of BoundedSolution
#' @description Constructor function to create an instance of
#' the S3 class BoundedSolution
#' @param obj_val The value of the objective function
#' @param dvars A vector of numeric values for each descision variable of an
#' integer programming problem

  BoundedSolution <- list(
    z = get.objective(ip_prob),
    x = get.variables(ip_prob)
  )

  class(BoundedSolution) <- "BoundedSolution"
  names(BoundedSolution$x) <- sapply(seq_along(BoundedSolution$x), function(i) {
    return(sprintf("x%s", i))
  })

  return(BoundedSolution)
}

ip_prob <- make.lp(0, 2) %T>%
  lp.control(sense = "max") %T>%
  set.objfn(c(8, 5)) %T>%
  add.constraint(c(1, 1), "<=", 6) %T>%
  add.constraint(c(9, 5), "<=", 45)

solve(ip_prob)

ub_sol <- create_sol(get.objective(ip_prob), get.variables(ip_prob))
root <- fmt(ub_sol)
tree <- make_tree(0, 2) + vertex(root)

branch_bound <- function(upper_bound, ip_prob, tree, root, node) {
  branch_val <- upper_bound %>% get_dvars() %>% branch_on()

  if (branch_val == 0) {
    plot(tree,
         layout = layout.reingold.tilford(tree),
         vertex.size = 65,
         edge.arrow.size = 0.2,
         edge.label.cex = 0.7,
         vertex.label.cex = 0.7,
         vertex.shape = "circle"
    )
    return(ip_prob)
  }

  dvars <- get_dvars(upper_bound)
  branch_var <- get_varname(dvars, branch_val)
  constrs <- get_constr_coeffs(dvars, branch_val)
  dvar_floored <- floor(branch_val)
  dvar_ceiled <- ceiling(branch_val)

  # Solving problem for the left side of the tree
  node <- node + 1
  left_ip_prob <- ip_prob %T>% add.constraint(constrs, "<=", dvar_floored)
  left_status <- solve(left_ip_prob)
  left_ub_sol <- create_sol(
    get.objective(left_ip_prob),
    get.variables(left_ip_prob)
  )
  # Creating the left child node of tree
  left_node <- if (left_status == 0) fmt(left_ub_sol, node) else sprintf("(%g)\nInfeasible", node)
  left_label <- sprintf("%s <= %d", branch_var, dvar_floored)
  tree <- tree +
    vertex(left_node) +
    edge(root, left_node, label = left_label)

  # Remove the last constraint added for the left side of the branch
  left_ip_prob %>%
    get.constraints() %>%
    length() %>%
    delete.constraint(left_ip_prob, .)

  # Solving problem for the right side of the tree
  node <- node + 1
  right_ip_prob <- ip_prob %T>% add.constraint(constrs, ">=", dvar_ceiled)
  right_status <- solve(right_ip_prob)
  right_ub_sol <- create_sol(
    get.objective(right_ip_prob),
    get.variables(right_ip_prob)
  )
  # Creating the right child node of tree
  right_node <- if (right_status == 0) fmt(right_ub_sol, node) else sprintf("(%g)\nInfeasible", node)
  right_label <- sprintf("%s >= %d", branch_var, dvar_ceiled)
  tree <- tree +
    vertex(right_node) +
    edge(root, right_node, label = right_label)

  # Remove the last constraint added for the right side of the branch
  right_ip_prob %>%
    get.constraints() %>%
    length() %>%
    delete.constraint(right_ip_prob, .)

  if (left_status == 0 && right_status == 0) {
    if (left_ub_sol$z > right_ub_sol$z) {
      left_ip_prob <- ip_prob %T>%
        add.constraint(constrs, "<=", dvar_floored)

      branch_bound(left_ub_sol, left_ip_prob, tree, left_node, node)
    } else {
      right_ip_prob <- ip_prob %T>%
        add.constraint(constrs, ">=", dvar_ceiled)

      branch_bound(right_ub_sol, right_ip_prob, tree, right_node, node)
    }
  }
  else if (left_status == 0 && right_status != 0) {
    left_ip_prob <- ip_prob %T>%
      add.constraint(constrs, "<=", dvar_floored)

    branch_bound(left_ub_sol, left_ip_prob, tree, left_node, node)
  }
  else if (left_status != 0 && right_status == 0) {
    right_ip_prob <- ip_prob %T>%
      add.constraint(constrs, ">=", dvar_ceiled)

    branch_bound(right_ub_sol, right_ip_prob, tree, right_node, node)
  }
  else {
    return(ip_prob)
  }
}

branch_bound(ub_sol, ip_prob, tree, root, 1)
