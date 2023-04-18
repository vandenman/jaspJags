normalizeModel <- function(modelCode) {

  pat <- r"{\s*model\s*\{(.*)\}}"
  modelBody <- regmatches(modelCode, regexec(pat, modelCode))[[1]][[2]]

  # TODO:
  # normalizeTruncation
  return(modelBody)
}


isLoop <- function(x) !is.symbol(x) && identical(x[[1L]], as.symbol("for"))
getRelation <- function(x) {
  if (identical(x, as.symbol("~")))
    "stochastic"
  else
    "deterministic"
}
decomposeExpression <- function(x) {
  # TODO: lhs needs to specify a transformation
  lhs <- list(code = x[[2]], node = getNode(x), parameters = all.vars(x[[2]]))
  rhs <- list(code = x[[3]], parameters = all.vars(x[[3]]))
  return(list(lhs = lhs, rhs = rhs, relation = getRelation(x[[1]])))#, src = x))
}

getNode <- function(x) {
  if (is.call(x))
    return(getNode(x[[2]]))
  if (is.symbol(x))
    return(x)
  stop("could not determine node of ", x)
}
# to test
# getNode(x)
# getNode(quote(alpha[p]))
# getNode(quote(logistic(alpha)[p]))
# getNode(quote(logistic(alpha[p, q])))
# getNode(quote(logistic(alpha[p + z, q])))

decomposeJagsModel <- function(modelExpr) {
  result <- list()
  for (i in seq_along(modelExpr)) {
    if (isLoop(modelExpr[[i]])) {
      result[[length(result) + 1]] <- list(
        relation   = "loop",
        iterator   = modelExpr[[i]][[2]],
        code       = modelExpr[[i]][-4], # not entirely accurate but what gives
        parameters = all.vars(modelExpr[[i]][[3]]),
        children   = decomposeJagsModel(modelExpr[[i]][[4]][-1])
      )
    } else {
      result[[length(result) + 1]] <- decomposeExpression(modelExpr[[i]])
    }
  }
  result
}

nodes2df <- function(x, parents = c()) {
  # loop over result, bind to data.frame
  # node  parents  code  relation lhsParameters rhsParameters
  if (is.list(x) && is.null(names(x))) {
    return(do.call(rbind, lapply(x, nodes2df)))
  } else if (x$relation == "loop") {
    return(do.call(rbind, lapply(x$children, nodes2df, parents = union(parents, c(x$iterator, x$parameters)))))
  } else if (!is.null(x$relation)) {
    # TODO: lhs needs to specify a transformation
    return(data.frame(
      node          = as.character(x$lhs$node),
      parents       = I(list(unique(as.character(c(x$lhs$parameters, x$rhs$parameters, parents))))),
      codeLhs       = I(list(x$lhs$code)),
      codeRhs       = I(list(x$rhs$code)),
      parametersLhs = I(list(as.character(x$lhs$parameters))),
      parametersRhs = I(list(as.character(x$rhs$parameters))),
      relation      = x$relation
    ))
  } else {
    stop("should be impossible to reach!", domain = NA)
  }
}


normalizeTruncation <- function(x) {
  pat <- r"{~\s*([^T]*\))\s*T\s*\(([^,]*),([^,]*)\)}"
  gsub(pat, "~ truncated(\\1, \\2, \\3)", x)
}
# normalizeTruncation("x ~ dbeta(a, b)T(c, d)")
# normalizeTruncation("x ~ dbeta(a, b)T(, d)")
# normalizeTruncation("x ~ dbeta(a, b)T(c, )")
# normalizeTruncation("x ~ dbeta(a, b)T(,)")

extractDistribution <- function(x) {
  # pat <- r"{~\s*d([^T\(]*)}"
  pat <- r"{~\s*(truncated\()?d([^\(]*)}"
  regmatches(x, regexec(pat, x))[[1]][[3]]
  # regmatches(x, regexpr(pat, x))
}
# extractDistribution("x ~ dbeta(a, b)T(c, d)")
# extractDistribution("x ~ dbeta(a, b)T(, d)")
# extractDistribution("x ~ dbeta(a, b)T(c, )")
# extractDistribution("x ~ dbeta(a, b)T(,)")
# extractDistribution("x ~ truncated(dbeta(a, b), , )")
# extractDistribution("x ~ dt(a, b)T(,)")

extractPrior <- function(modelDf, node) {
  # TODO: add reasons for why the failure happened
  idx <- which(modelDf[["node"]] == node)
  if (length(idx) == 0L)
    return(list(FAILED = TRUE))
  if (length(modelDf[[idx, "parametersRhs"]]) != 0L)
    return(list(FAILED = TRUE))
  extractPriorFromExpr(modelDf[[idx, "codeRhs"]])
}

extractPriorFromExpr <- function(expr) {
  obj <- as.character(expr)

  dist <- lookupDistribution(dropFirstLetter(obj[[1]]))
  if (is.na(dist))
    return(list(FAILED = TRUE))

  distObj <- get(dist, envir = asNamespace("distr6"))
  args <- try(lapply(obj[-1L], \(x) eval(parse(text = x))))
  if (inherits(args, "try-error"))
    return(list(FAILED = TRUE))

  obj <- try(do.call(distObj$new, args))
  if (inherits(obj, "try-error"))
    return(list(FAILED = TRUE))
  return(list(FAILED = FALSE, obj = obj, className = dist, args = args, src = expr))
}

dropFirstLetter <- function(x) {
  # mainly to the letter 'd' in e.g., dunif
  substr(x, 2L, nchar(x))
}

lookupDistribution <- function(dist) {
  lst <- distr6::listDistributions()
  idx <- match(dist, tolower(lst$ShortName))
  if (is.na(idx))
    return(idx)
  return(lst$ClassName[idx])
}

findStochasticData <- function(modelDf, data) {
  intersect(modelDf[["node"]][modelDf$relation == "stochastic"], data)
}
