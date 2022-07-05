essentials:::check_this(  # iris
    build = TRUE,

    check = TRUE, as.cran = TRUE,

    chdir = TRUE
)


if (FALSE) {
    testClass <- setIrisClass(
        "testClass",
        members = list(
            field(className = "data.frame", name = "test", value = data.table::data.table())
        )
    )


    # x <- testClass$new()
    x <- new(attr(testClass, "className"))
    y <- new(attr(testClass, "className"))
}


if (FALSE) {
    x <- new(attr(iris:::IrisObject, "className"))
    x <- new(attr(testClass, "className"))
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
}
