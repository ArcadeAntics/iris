essentials:::check_this(  # iris
    build = TRUE,

    check = FALSE, as.cran = TRUE,

    chdir = TRUE
)





testClass <- setIrisClass(
    "testClass",
    members = list(
        field(className = "data.frame", name = "test", value = data.table::data.table())
    )
)


x <- testClass$new()
