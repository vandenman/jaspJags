context("JAGS")

options <- jaspTools::analysisOptions("JAGS")
options$initialValues <- list(list(levels = "Row 1", name = "Parameter", values = "theta"),
                              list(levels = "Row 1", name = "R Code", values = "..."))
options$model <- list(columns = list(), model = "model{\n theta ~ dbeta(1, 1)\n k ~ dbinom(theta, n)\n mu ~ dnorm(0, 1)}",
                      parameters = c("theta", "k"))
options$noBurnin <- 1
options$noChains <- 4
options$noSamples <- 50
options$parametersShown <- c("theta", "mu")
options$plotAutoCor <- TRUE
options$plotDensity <- TRUE
options$plotHistogram <- TRUE
options$plotTrace <- TRUE
options$plotBivarHex <- TRUE
options$userData <- list(list(
  levels = c("Row 0", "Row 1"),
  name = "Parameter", values = c("k", "n")
), list(
  levels = c("Row 0", "Row 1"),
  name = "R Code",
  values = c("70", "100")
))
options$parameters <- c("\"theta\"", "\"mu\"")


old.coda.samples <- rjags::coda.samples
new.coda.samples <- function(...) {
  candidates <- c("jags-test-model.rds", file.path("tests", "testthat", "jags-test-model.rds"))
  testFile <- Filter(file.exists, candidates)
  return(readRDS(testFile))
}

jaspTools:::replaceFn("coda.samples", new.coda.samples, "rjags")
on.exit({jaspTools:::replaceFn("coda.samples", old.coda.samples, "rjags")})

set.seed(1)
results <- jaspTools::runAnalysis("JAGS", "debug.csv", options)

test_that("MCMC summary table results match", {
  table <- results[["results"]][["mainContainer"]][["collection"]][["mainContainer_mainTable"]][["data"]]
  jaspTools::expect_equal_tables(table,
                      list(-2.13189859421549, 0.0400235500930204, 1.75316153519098, 0.0150312557449907,
                           1.04100463312435, 162, "mu", 1.28053408855522,
                           1.08774224099908, 0.614492436789889, 0.69902486500684, 0.777692716928423,
                           0.696777556636238, 0.0431041676553818, 232, "theta",
                           1.25276545060289, 1.07739568349341))
})

test_that("plotAutoCor mu plot matches", {
  plotName <- results[["results"]][["mainContainer"]][["collection"]][["mainContainer_plotContainer"]][["collection"]][["mainContainer_plotContainer_plotAutoCor"]][["collection"]][["mainContainer_plotContainer_plotAutoCor_mu"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "plotAutoCor-mu", dir="JAGS")
})

test_that("plotAutoCor theta plot matches", {
  plotName <- results[["results"]][["mainContainer"]][["collection"]][["mainContainer_plotContainer"]][["collection"]][["mainContainer_plotContainer_plotAutoCor"]][["collection"]][["mainContainer_plotContainer_plotAutoCor_theta"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "plotAutoCor-theta", dir="JAGS")
})

test_that("plotDensity mu plot matches", {
  plotName <- results[["results"]][["mainContainer"]][["collection"]][["mainContainer_plotContainer"]][["collection"]][["mainContainer_plotContainer_plotDensity"]][["collection"]][["mainContainer_plotContainer_plotDensity_mu"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "plotDensity-mu", dir="JAGS")
})

test_that("plotDensity theta plot matches", {
  plotName <- results[["results"]][["mainContainer"]][["collection"]][["mainContainer_plotContainer"]][["collection"]][["mainContainer_plotContainer_plotDensity"]][["collection"]][["mainContainer_plotContainer_plotDensity_theta"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "plotDensity-theta", dir="JAGS")
})

test_that("plotHistogram mu plot matches", {
  plotName <- results[["results"]][["mainContainer"]][["collection"]][["mainContainer_plotContainer"]][["collection"]][["mainContainer_plotContainer_plotHistogram"]][["collection"]][["mainContainer_plotContainer_plotHistogram_mu"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "plotHistogram-mu", dir="JAGS")
})

test_that("plotHistogram theta plot matches", {
  plotName <- results[["results"]][["mainContainer"]][["collection"]][["mainContainer_plotContainer"]][["collection"]][["mainContainer_plotContainer_plotHistogram"]][["collection"]][["mainContainer_plotContainer_plotHistogram_theta"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "plotHistogram-theta", dir="JAGS")
})

test_that("plotTrace mu plot matches", {
  plotName <- results[["results"]][["mainContainer"]][["collection"]][["mainContainer_plotContainer"]][["collection"]][["mainContainer_plotContainer_plotTrace"]][["collection"]][["mainContainer_plotContainer_plotTrace_mu"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "plotTrace-mu", dir="JAGS")
})

test_that("plotTrace theta plot matches", {
  plotName <- results[["results"]][["mainContainer"]][["collection"]][["mainContainer_plotContainer"]][["collection"]][["mainContainer_plotContainer_plotTrace"]][["collection"]][["mainContainer_plotContainer_plotTrace_theta"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "plotTrace-theta", dir="JAGS")
})

test_that("Bivariate Scatter Plot matches", {
  plotName <- results[["results"]][["mainContainer"]][["collection"]][["mainContainer_plotContainer"]][["collection"]][["mainContainer_plotContainer_plotBivarHex"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "bivariate-scatter-plot", dir="JAGS")
})
