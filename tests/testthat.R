library(jaspTools)
library(testthat)

if (Sys.getenv("COVERAGE") == "true") {

  # so covr and jaspTools don't really play nicely.
  # below I fix this but we should come up with a better solution.
  # the main issue is that covr installs the module as an R pkg and runs the checks on the installed binary.
  # but jaspTools expects a source package (e.g., for locating qml and R files).

  modulePath <- normalizePath(file.path(getwd(), "../"))
  jaspTools::setPkgOption("module.dirs", modulePath)

  # move QML to where jaspTools looks
  dir.create(file.path(modulePath, "inst"))
  file.rename(file.path(modulePath, "qml"), file.path(modulePath, "inst", "qml"))
  file.rename(file.path(modulePath, "Description.qml"), file.path(modulePath, "inst", "Description.qml"))

  # already done by covr
  jaspTools::setPkgOption("reinstall.modules", FALSE)

  # we need this because jaspTools expects a source package and not a binary package (which is used by covr)
  jaspBase:::assignFunctionInPackage(function(funName, modulePath) {
    ns <- getNamespace(basename(modulePath))
    return(!is.null(ns[[funName]]))
  }, "rFunctionExistsInModule", "jaspTools")

} else {
  modulePath <- getwd()
}

jaspTools::runTestsTravis(modulePath)
