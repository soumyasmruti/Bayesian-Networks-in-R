## Function to create a conditional probability table
## Conditional probability is of the form p(x1 | x2, ..., xk)
## varnames: vector of variable names (strings)
## -- NOTE: first variable listed will be x1, remainder will be parents, x2, ..., xk
## probs: vector of probabilities for the flattened probability table
## levelsList: a list containing a vector of levels (outcomes) for each variable
## See the BayesNetExamples.r file for examples of how this function works
createCPT = function(varnames, probs, levelsList)
{
  ## Check dimensions agree
  if(length(probs) != prod(sapply(levelsList, FUN=length)))
    return(NULL)

  ## Set up table with appropriate dimensions
  m = length(probs)
  n = length(varnames)
  g = matrix(0, m, n)

  ## Convert table to data frame (with column labels)
  g = as.data.frame(g)
  names(g) = varnames

  ## This for loop fills in the entries of the variable values
  k = 1
  for(i in n:1)
  {
    levs = levelsList[[i]]
    g[,i] = rep(levs, each = k, times = m / (k * length(levs)))
    k = k * length(levs)
  }

  return(data.frame(probs = probs, g))
}

## Build a CPT from a data frame
## Constructs a conditional probability table as above, but uses frequencies
## from a data frame of data to generate the probabilities.
createCPT.fromData = function(x, varnames)
{
  levelsList = list()

  for(i in 1:length(varnames))
  {
    name = varnames[i]
    levelsList[[i]] = sort(unique(x[,name]))
  }

  m = prod(sapply(levelsList, FUN=length))
  n = length(varnames)
  g = matrix(0, m, n)

  ## Convert table to data frame (with column labels)
  g = as.data.frame(g)
  names(g) = varnames

  ## This for loop fills in the entries of the variable values
  k = 1
  for(i in n:1)
  {
    levs = levelsList[[i]]
    g[,i] = rep(levs, each = k, times = m / (k * length(levs)))
    k = k * length(levs)
  }

  ## This is the conditional probability column
  probs = numeric(m)
  numLevels = length(levelsList[[1]])
  skip = m / numLevels

  ## This chunk of code creates the vector "fact" to index into probs using
  ## matrix multiplication with the data frame x
  fact = numeric(ncol(x))
  lastfact = 1
  for(i in length(varnames):1)
  {
    j = which(names(x) == varnames[i])
    fact[j] = lastfact
    lastfact = lastfact * length(levelsList[[i]])
  }
  ## Compute unnormalized counts of subjects that satisfy all conditions
  a = as.matrix(x - 1) %*% fact + 1
  for(i in 1:m)
    probs[i] = sum(a == i)

  ## Now normalize the conditional probabilities
  for(i in 1:skip)
  {
    denom = 0 ## This is the normalization
    for(j in seq(i, m, skip))
      denom = denom + probs[j]
    for(j in seq(i, m, skip))
    {
      if(denom != 0)
        probs[j] = probs[j] / denom
    }
  }

  return(data.frame(probs = probs, g))
}

## Product of two factors
## A, B: two factor tables
##
## Should return a factor table that is the product of A and B.
## You can assume that the product of A and B is a valid operation.
productFactor = function(A, B)
{
  ## Your code here!
  ab <- merge(A, B, by = setdiff(intersect(names(A), names(B)), "probs"))
  ab$probs = ab$probs.x * ab$probs.y
  drops <- c("probs.x","probs.y")
  ab = ab[ , !(names(ab) %in% drops)]
  return(ab)
}

## Marginalize a variable from a factor
## A: a factor table
## margVar: a string of the variable name to marginalize
##
## Should return a factor table that marginalizes margVar out of A.
## You can assume that margVar is on the left side of the conditional.
marginalizeFactor = function(X, margVar)
{
  ## Your code here!
  setdifer = setdiff(colnames(X), list(margVar, "probs"))
  l <- strsplit(setdifer, " ")
  agglist <- list()
  for(s in l){
    st = paste("X", s, sep = "$")
    agglist <- c(agglist, list(eval(parse(text = st))))
  }
  agg <- aggregate(X$probs, by = agglist, FUN = sum)
  colnames(agg) <- c(setdifer, "probs") 
  return(agg)
}

## Marginalize a list of variables
## bayesnet: a list of factor tables
## margVars: a vector of variable names (as strings) to be marginalized
##
## Should return a Bayesian network (list of factor tables) that results
## when the list of variables in margVars is marginalized out of bayesnet.
aggregateFactors = function(aggnets){

  if(length(aggnets) > 2){
    mdf <- productFactor(aggnets[[1]], aggnets[[2]])
    for(i in 3:length(aggnets)){
      mdf <- productFactor(mdf, aggnets[[i]])
    }
    return(mdf)
  }else if(length(aggnets) == 2){
    return(productFactor(aggnets[[1]], aggnets[[2]]))
  }else if(length(aggnets) == 1){
    return(aggnets[[1]])
  }
  
}

marginalize = function(bayesnet, margVars)
{
  ## Your code here
  if (is.null(margVars))
  {
    return (bayesnet)
  }
  tempnet = bayesnet
  for(m in margVars){
    marlist <- list()
    restlist <- list()
    for(df in tempnet){
      if(m %in% names(df)){
        marlist <- c(marlist, list(df))
      }
      else{
        restlist <- c(restlist, list(df))
      }
    }
    if (length(marlist) != 0) {
      mardf <- marginalizeFactor(aggregateFactors(marlist), m)
      tempnet <- list(mardf)
    }
    for(r in restlist){
      tempnet <- c(tempnet, list(r))
    }
  }
  str = "x y z a b c d e f g h i j k"
  names(tempnet) <- c(strsplit(substr("x y z a b c d e f g", 1, 2*length(tempnet) - 1), " "))[[1]]
  return(tempnet)
}

## Observe values for a set of variables
## bayesnet: a list of factor tables
## obsVars: a vector of variable names (as strings) to be observed
## obsVals: a vector of values for corresponding variables (in the same order)
##
## Set the values of the observed variables. Other values for the variables
## should be removed from the tables. You do not need to normalize the factors
## to be probability mass functions.
# observe = function(bayesnet, obsVars, obsVals)
# {
#   ## Your code here!
#   if (is.null(obsVars) | is.null(obsVals) ) 
#   {
#     return (bayesnet)
#   }
#   names(bayesnet) <- c(strsplit(substr("x y z a b c d e f g h i j", 1, 2*length(bayesnet) - 1), " "))[[1]]
#   newnet <- list()
#   for(net in names(bayesnet)){
#       name <- paste("bayesnet", net, sep = "$")
#       df <- eval(parse(text = name))
#       check <- obsVars %in% names(df)
#       idx <- grep("TRUE", check)
#       cond <- c()
#       for(i in idx){
#         obj <- paste("bayesnet", net, obsVars[i], sep = "$")
#         obsv <- paste("'", obsVals[1], "'", sep = "")
#         co <- paste(obj, obsv, sep = "==") 
#         cond <- c(cond, co)
#       }
#       cond <- paste(cond, collapse = ' & ' )
#       if(length(grep("TRUE", check) > 0)){
#         newnet <- c(newnet, list(df[eval(parse(text = cond)),]))
#       }
#       else{
#         newnet <- c(newnet, list(df))
#       }
#     }
#     names(newnet) <- c(strsplit(substr("x y z a b c d e f g", 1, 2*length(newnet) - 1), " "))[[1]]
#     return(newnet)
#  
# }

observe = function(bayesnet, obsVars, obsVals)
{
  ## Your code here!
  if (is.null(obsVars) | is.null(obsVals) ) 
  {
    return (bayesnet)
  }
  names(bayesnet) <- c(strsplit(substr("x y z a b c d e f g h i j", 1, 2*length(bayesnet) - 1), " "))[[1]]
  newnet <- list()
  idx <- length(obsVars)  
  
  for(net in names(bayesnet)){
    name <- paste("bayesnet", net, sep = "$")
    df <- eval(parse(text = name))
    for (i in 1:idx)
    {
      if ((obsVars[i] %in% colnames(df)))
      {
      df <- subset(df, df[, obsVars[i]] == obsVals[i])
      }
    }
    newnet <- c(newnet, list(df))
  }
  names(newnet) <- c(strsplit(substr("x y z a b c d e f g", 1, 2*length(newnet) - 1), " "))[[1]]
  return(newnet)
  
}


## Run inference on a Bayesian network
## bayesnet: a list of factor tables
## margVars: a vector of variable names to marginalize
## obsVars: a vector of variable names to observe
## obsVals: a vector of values for corresponding variables (in the same order)
##
## This function should run marginalization and observation of the sets of
## variables. In the end, it should return a single joint probability table. The
## variables that are marginalized should not appear in the table. The variables
## that are observed should appear in the table, but only with the single
## observed value. The variables that are not marginalized or observed should
## appear in the table with all of their possible values. The probabilities
## should be normalized to sum to one.
infer = function(bayesnet, margVars, obsVars, obsVals)
{
  ## Your code here!
  if (is.character(margVars) & !(is.null(margVars)))
  {
    margVars = c(margVars)
  }
  if (is.character(obsVars) & !(is.null(obsVars)))
  {
    obsVars = c(obsVars)
  }
  if (is.character(obsVals) & !(is.null(obsVals)))
  {
    obsVals = c(obsVals)
  }
  obsdf = observe(bayesnet,  obsVars, obsVals)
  mardf = marginalize(obsdf, margVars)
  prodnet <- aggregateFactors(mardf)
  
  probsum = sum(prodnet$probs)
  testFunc <- function(a) a/probsum
  prodnet$probs <- mapply(testFunc, prodnet$probs)
  return(prodnet) 
}
