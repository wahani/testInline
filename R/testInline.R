#' Write tests in your source
#'
#' This functions can be used in package development to keep your unit tests where your function definitions are.
#' @param expr test code
#' @param file character with name of file in which expr will be stored
#' @param dir directory where you want your tests. Allways as sub directory of tests in your package root.
#' @param eval if you want to evaluate your testcode. This will call `devtools::test`
#'
#' @export
testInline <- function(expr, file, dir = "testthat", eval = interactive()) {
   testCode <- c(
       "# This file has been generated automatically and will be replaced soon.",
       deparse(substitute(expr)))
   testFileName <- Directory(Directory("tests") %paste0% dir) %paste0% file
   writeLines(testCode, testFileName)
   if (eval && calledFromUser(parent.frame())) devtools::test(filter = basename(testFileName))
   invisible(NULL)
}

calledFromUser <- function(e) {
    identical(e, .GlobalEnv)
}

"%paste0%" <- function(e1, e2) paste0(e1, e2)

testInline(file = "test-testInline.R", {

    context("testInline")
    test_that("...", {
        tester <- testthat::expect_true
        tester(file.exists("test-testInline.R"))
    })
})

testInline(file = "test-paste.R", {
    context("Binary paste")
    expect_equal("a" %paste0% "b", "ab")
})
