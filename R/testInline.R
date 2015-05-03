#' Write tests in your source
#'
#' This functions can be used in package development to keep your unit tests where your function definitions are. The code in \code{expr} will be saved into a R file in the test directory. And can then be called by your testing framework. Files will be created and replaced on the fly.
#'
#' @param label label for tests. Used only as default in other arguments
#' @param expr test code
#' @param file character with name of file in which expr will be stored
#' @param dir directory where you want your tests
#' @param eval if you want to evaluate your testcode right away
#' @param testFun how you test code is evaluated. Only relevant if called interactively
#'
#' @export
testInline <- function(label,
                       expr,
                       file = paste0("test-", label, ".R"),
                       dir = getOption("testInline.directory", "tests/testthat"),
                       eval = interactive(),
                       testFun = getOption("testInline.testFun", function() devtools::test(filter = label))) {

    calledFromUser <- function() {
        identical(parent.frame(2), .GlobalEnv)
    }

    testCode <- c(
        "# This file has been generated automatically and will be replaced soon.",
        deparse(substitute(expr))
    )

    testFileName <- processTestDirectory(dir) %paste0% "/" %paste0% file
    writeLines(testCode, testFileName)
    if (eval && calledFromUser()) testFun()
    invisible(NULL)

}

processTestDirectory <- function(dir) {

    reduce <- function(f, x) {
        Reduce(f, x[-1], init = x[1])
    }

    makeDir <- function(name) {
        if(!file.exists(name)) {
            message("Creating new directory '", name, "' ...")
            dir.create(name)
        }
        name
    }

    backToPackageRoot <- function() {
        wd <- normalizePath(getwd(), "/")
        sub("/R$", "", wd)
    }

    dir <- backToPackageRoot() %paste0% "/" %paste0% dir
    dir <- unlist(strsplit(dir, "/"))
    dir <- reduce(function(e1, e2) makeDir(e1 %paste0% "/" %paste0% e2), dir)
    dir
}

"%paste0%" <- function(e1, e2) paste0(e1, e2)

testInline("testFilesCreated", {
    context("testInlie")
    expect_true(file.exists("test-testFilesCreated.R"))
})

testInline("paste0", {
    context("Binary paste")
    expect_equal("a" %paste0% "b", "ab")
})
