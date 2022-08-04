essentials:::check_this(  # iris
    special = TRUE,

    check = FALSE, as.cran = TRUE,

    chdir = TRUE
)


if (FALSE) {
    testClass <- setIrisClass(
        "testClass",
        field(Class = "data.frame", name = "test", value = data.table::data.table())
    )


    x <- testClass$new()
    y <- testClass$new()


    for (x in list(iris:::IrisObject$new(),
                   testClass$new())) withAutoprint({
        x$.classDef
        x$.irisClassDef
        x$.objectPackage
        x$copy
        x$field
        x$getClass
        x$getIrisClass
        x$show
        x[[".classDef"]]
        x[[".irisClassDef"]]
        x[[".objectPackage"]]
        x[["copy"]]
        x[["field"]]
        x[["getClass"]]
        x[["getIrisClass"]]
        x[["show"]]
        identical(x, environment(x$copy        )$envir)
        identical(x, environment(x$field       )$envir)
        identical(x, environment(x$getClass    )$envir)
        identical(x, environment(x$getIrisClass)$envir)
        identical(x, environment(x$show        )$envir)
    })
}
