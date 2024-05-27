## |
## |  *Unit tests*
## |
## |  This file is part of the R package rpact:
## |  Confirmatory Adaptive Clinical Trial Design and Analysis
## |
## |  Author: Gernot Wassmer, PhD, and Friedrich Pahlke, PhD
## |  Licensed under "GNU Lesser General Public License" version 3
## |  License text can be found here: https://www.r-project.org/Licenses/LGPL-3
## |
## |  RPACT company website: https://www.rpact.com
## |  RPACT package website: https://www.rpact.org
## |
## |  Contact us for information about our services: info@rpact.com
## |
## |  File name: test-class_dictionary.R
## |  Creation date: 23 February 2024, 07:45:41
## |  File version: $Revision$
## |  Last changed: $Date$
## |  Last changed by: $Author$
## |

test_plan_section("Testing Class 'Dictionary'")


test_that("Add values to Dictionary", {
    dict <- createDictionary("TestDictionary")
    expect_equal(class(dict)[1], "Dictionary")
    expect_error(.assertIsDictionary(dict), NA)

    addValueToDictionary(dict, "x1", 1)
    addValueToDictionary(dict, "x2", "A")
    addValueToDictionary(dict, "x3", "B")


    ## Comparison of the results of Dictionary object 'dict' with expected results
    expect_equal(dict$x1, 1, label = paste0(dict$x1))
    expect_equal(dict$x2, "A", label = paste0(dict$x2))
    expect_equal(dict$x3, "B", label = paste0(dict$x3))
})

test_that("Set values to Dictionary", {
    dict <- createDictionary("TestDictionary", list(a = 1, b = 2, c = 3))
    setValueToDictionary(dict, "b", 99)
    setValueToDictionary(dict, "d", 33)

    ## Comparison of the results of Dictionary object 'dict' with expected results
    expect_equal(dict$a, 1, label = paste0(dict$a))
    expect_equal(dict$b, 99, label = paste0(dict$b))
    expect_equal(dict$c, 3, label = paste0(dict$c))
    expect_equal(dict$d, 33, label = paste0(dict$d))
})

test_that("Get values from Dictionary", {
    dict <- createDictionary("TestDictionary", list(a = 1, b = 2, c = 3))
    expect_equal(getValueFromDictionary(dict, "a"), 1)
    expect_equal(getValueFromDictionary(dict, "b"), 2)
    expect_equal(getValueFromDictionary(dict, "c"), 3)
    expect_error(getValueFromDictionary(dict, "d"), paste0("Illegal argument: dictionary ", sQuote("TestDictionary"), " does not contain key ", sQuote("d")))
})

test_that("Get Dictionary key by value", {
    dict <- createDictionary("TestDictionary", list(a = 1, b = "B", c = 3))
    expect_equal(getDictionaryKeyByValue(dict, "B"), "b")
    expect_equal(getDictionaryKeyByValue(dict, 1), "a")
    expect_equal(getDictionaryKeyByValue(dict, 0), character(0))
})

test_that("Dictionary as vector", {
    dict <- createDictionary("TestDictionary", list(a = 1, b = 2, c = 3))
    expect_equal(as.vector(dict), c("1", "2", "3"))
})

test_that("Dictionary as list", {
    dict <- createDictionary("TestDictionary", list(a = 1, b = 2, c = 3))
    expect_equal(as.list(dict), list(a = 1, b = 2, c = 3))
})

test_that("Print Dictionary", {
    dict <- createDictionary("TestDictionary", list(a = 1, b = 2, c = 3))
    expect_equal(paste(utils::capture.output(print(dict)), collapse = "\n"), "TestDictionary \n$a\n[1] 1\n\n$b\n[1] 2\n\n$c\n[1] 3\n")
})

test_that("Get subset from Dictionary", {
    dict <- createDictionary("TestDictionary", list(a = 1, b = 2, c = 3))
    dictSub <- getDictionarySubset(dict, c("a", "c"))


    ## Comparison of the results of Dictionary object 'dictSub' with expected results
    expect_equal(dictSub$a, 1, label = paste0(dictSub$a))
    expect_equal(dictSub$c, 3, label = paste0(dictSub$c))

    ## Comparison of the results of Dictionary object 'dict' with expected results
    expect_equal(dict$a, 1, label = paste0(dict$a))
    expect_equal(dict$b, 2, label = paste0(dict$b))
    expect_equal(dict$c, 3, label = paste0(dict$c))
})
