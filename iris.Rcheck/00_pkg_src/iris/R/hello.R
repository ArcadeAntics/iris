

# we need 'methods' in order to create the classes and methods, weird stuff happens if we don't
library(methods)


delayedAssign(".identC", methods:::.identC)


is.activeBindingFunction <- function(x) NULL
body(is.activeBindingFunction) <- substitute(is(x, .), list(. = `packageSlot<-`("activeBindingFunction", "methods")))


as.activeBindingFunction <- function(x) NULL
body(as.activeBindingFunction) <- substitute(new(., x), list(. = `packageSlot<-`("activeBindingFunction", "methods")))


# irisFieldDef    ----


irisFieldDef <- setClass(
    Class = paste0(.packageName, "::", "irisFieldDef"),
    slots = list(
        access    = "character",
        static    = "logical"  ,
        final     = "logical"  ,
        className = "character",
        name      = "character",
        value     = `packageSlot<-`("ANY", "methods"),
        hasValue  = "logical"
    )
)
body(irisFieldDef@.Data) <- substitute(new(Class = CLASS, ...), list(CLASS = irisFieldDef@className))


tmp <- function (.Object, access = NA, static = FALSE, final = FALSE,
    className = `packageSlot<-`("ANY", "methods"), name, value,
    where = topenv(parent.frame(4)), ...)
{
    .Object <- callNextMethod(.Object = .Object, ...)


    name <- as.character(as.symbol(name))
    if (name == "super")
        stop("invalid 'name'; \"super\" is a reserved name")


    access <- as.character(access)[[1L]]
    if (is.na(access))
        access <- if (startsWith(name, ".")) "private" else "public"
    else if (!(access %in% c("private", "public", "protected")))
        stop("invalid 'access'")


    sstatic <- as.character(static)[[1L]]
    static <- if (!is.na(sstatic) && sstatic == "static")
        TRUE
    else if (static)
        TRUE
    else FALSE


    sfinal <- as.character(final)[[1L]]
    final <- if (!is.na(sfinal) && sfinal %in% c("final", "const"))
        TRUE
    else if (final)
        TRUE
    else FALSE


    classDef <- if (is.null(packageSlot(className))) {
        getClassDef(className, where = where)
    } else getClassDef(className)
    if (is.null(classDef))
        stop(sprintf("undefined class for field %s (class %s)",
            name, className))


    className <- classDef@className


    if (hasValue <- !missing(value)) {


        # taken from 'methods::checkSlotAssignment' and 'methods::checkAtAssignment'
        valueClass <- class(value)
        if (.identC(className, valueClass)) {
        } else {
            ok <- possibleExtends(valueClass, className, ClassDef2 = classDef)
            # if (isFALSE(ok))
            #     stop(gettextf("'value' of class %s is not valid; is(value, \"%s\") is not TRUE",
            #         paste(dQuote(valueClass), collapse = ", "), className))
            # else if (isTRUE(ok)) {
            # } else value <- as(value, className, strict = FALSE, ext = ok)
            if (isFALSE(ok))
                stop(gettextf("'value' of class %s is not valid; is(value, \"%s\") is not TRUE",
                    paste(dQuote(valueClass), collapse = ", "), className))
        }


    } else value <- if (!final && !isVirtualClass(className)) new(className)


    .Object@access    <- access
    .Object@static    <- static
    .Object@final     <- final
    .Object@className <- className
    .Object@name      <- name
    .Object@value     <- value
    .Object@hasValue  <- hasValue


    .Object
}
formals(tmp)[["className"]] <- eval(formals(tmp)[["className"]])
setMethod("initialize", irisFieldDef@className, tmp)
rm(tmp)


as.irisFieldDef <- function (..., where = topenv(parent.frame()))
{
    dots <- list(...)
    if ( length(dots) == 1L &&
         is.null(names(dots)) &&
         is(dots[[1L]], irisFieldDef@className) ) {
        dots[[1L]]
    } else {
        where
        irisFieldDef(..., where = where)
    }
}


# irisMethodDef   ----


irisMethodDef <- setClass(
    Class = paste0(.packageName, "::", "irisMethodDef"),
    slots = list(
        access = "character",
        static = "logical"  ,
        name   = "character",
        value  = "function"
    )
)
body(irisMethodDef@.Data) <- substitute(new(Class = CLASS, ...), list(CLASS = irisMethodDef@className))


setMethod("initialize", irisMethodDef@className,
function (.Object, access = NA, static = NA, name, value, ...)
{
    .Object <- callNextMethod(.Object = .Object, ...)


    name <- as.character(as.symbol(name))
    if (name == "super")
        stop("invalid 'name'; \"super\" is a reserved name")
    if (!is.function(value))
        value <- as.function(value)


    access <- as.character(access)[[1L]]
    if (is.na(access))
        access <- if (startsWith(name, ".")) "private" else "public"
    if (!(access %in% c("private", "public", "protected")))
        stop("invalid 'access'")


    sstatic <- as.character(static)[[1L]]
    static <- if (is.na(sstatic)) {
        f <- formals(value)
        !(length(f) >= 1 && names(f)[[1L]] %in% c(".self", "self", "this"))
    }
    else if (sstatic == "static")
        TRUE
    else if (static)
        TRUE
    else {
        f <- formals(value)
        if (!(length(f) >= 1 && names(f)[[1L]] %in% c(".self", "self", "this")))
            stop("invalid 'value' for 'static = FALSE'; must have '.self', 'self', or 'this' as the first argument")
        FALSE
    }


    .Object@access <- access
    .Object@static <- static
    .Object@name   <- name
    .Object@value  <- value


    .Object
})


as.irisMethodDef <- function (...)
{
    dots <- list(...)
    if ( length(dots) == 1L &&
         is.null(names(dots)) &&
         is(dots[[1L]], irisMethodDef@className) )
        dots[[1L]]
    else irisMethodDef(...)
}


# irisPropertyDef ----


irisPropertyDef <- setClass(
    Class = paste0(.packageName, "::", "irisPropertyDef"),
    contains = irisMethodDef@className
)
body(irisPropertyDef@.Data) <- substitute(new(Class = CLASS, ...), list(CLASS = irisPropertyDef@className))


as.irisFieldDef_or_irisPropertyDef <- function (..., where = topenv(parent.frame()))
{
    dots <- list(...)
    if ( length(dots) == 1L &&
         is.null(names(dots)) &&
         (
             is(dots[[1L]], irisFieldDef@className   ) ||
             is(dots[[1L]], irisPropertyDef@className)
         ) ) {
        dots[[1L]]
    } else {
        where
        irisFieldDef(..., where = where)
    }
}


# irisMethod      ----


irisMethod <- setClass(
    Class = paste0(.packageName, "::", "irisMethod"),
    contains = "function",
    slots = list(
        access = "character",
        static = "logical"  ,
        name   = "character",


        string  = "character",
        boundTo = `packageSlot<-`("ANY", "methods")
    )
)
body(irisMethod@.Data) <- substitute(new(Class = CLASS, ...), list(CLASS = irisMethod@className))


setMethod("show", irisMethod@className,
function (object)
{
    cat(attr(object, "string"), sep = "\n")
    print(x = if (isS4(object)) object@.Data else object, useSource = FALSE)
    invisible()
})


assign(paste0("print.", irisMethod@className),
function (x, ...)
{
    cat(attr(x, "string"), sep = "\n")
    print(x = if (isS4(x)) x@.Data else x, useSource = FALSE, ...)
    invisible(x)
})


# irisClassDef    ----


.irisClassTable <- new.env(parent = emptyenv())


irisClassDef <- setClass(
    Class = paste0(.packageName, "::", "irisClassDef"),
    contains = "environment",
    slots = list(
        className = "character",
        fields    = "list"     ,
        extends   = "character",
        methods   = "list"
    )
)
body(irisClassDef@.Data) <- substitute(new(Class = CLASS, ...), list(CLASS = irisClassDef@className))


irisRefClass <- setClass(
    Class = paste0(.packageName, "::", "irisRefClass"),
    contains = "environment",
    slots = list(
        string = "character"
    )
)
body(irisRefClass@.Data) <- substitute(new(Class = CLASS, ...), list(CLASS = irisRefClass@className))


irisClassGeneratorFunction <- function (className)
{
    generator <- function(...) NULL
    body(generator) <- substitute({
        .Object <- new(CLASS)
        if (exists("initialize", envir = .Object, inherits = FALSE)) {
            initialize <- .getWithoutCheck("initialize", .Object)
            if (is(initialize, irisMethod.className) && !initialize@static)
                initialize(...)
        }
        tmp <- .Object
        while (!identical(tmp, emptyenv())) {
            if (exists("finalize", envir = tmp, inherits = FALSE)) {
                finalize <- .getWithoutCheck("finalize", tmp)
                if (is(finalize, irisMethod.className) && !finalize@static)
                    reg.finalizer(tmp, finalize, onexit = TRUE)
            }
            tmp <- parent.env(tmp)
        }
        if (exists("validate", envir = .Object, inherits = FALSE)) {
            validate <- .getWithoutCheck("validate", .Object)
            if (is(validate, irisMethod.className) && !validate@static)
                validate()
        }
        .Object
    }, list(
        CLASS = className,
        irisMethod.className = irisMethod@className
    ))
    environment(generator) <- getNamespace(.packageName)
    return(generator)
}


assign(paste0("$.", irisRefClass@className) -> dollarGet,
function (x, name)
get(x = name, envir = x))


assign(paste0("$<-.", irisRefClass@className) -> dollarAssign,
function (x, name, value)
{
    if (exists(x = name, envir = x))
        assign(x = name, value = value, envir = x, inherits = TRUE)
    else stop(gettextf("%s is not a field or method of %s",
        sQuote(name), attr(x, "string")), call. = FALSE)
    invisible(x)
})


assign(paste0("[[.", irisRefClass@className),
function (x, i, ...)
{
    if (...length())
        stop("incorrect number of dimensions")
    get(x = i, envir = x)
})


assign(paste0("[[<-.", irisRefClass@className),
function (x, i, ..., value)
{
    if (...length())
        stop("incorrect number of dimensions")
    if (exists(x = i, envir = x))
        assign(x = i, value = value, envir = x, inherits = TRUE)
    else stop(gettextf("%s is not a field or method of %s",
        sQuote(i), attr(x, "string")), call. = FALSE)
    invisible(x)
})


copy <- function(x, shallow = FALSE) NULL
body(copy) <- substitute({
    shallow <- if (shallow) TRUE else FALSE
    .copy.check <- function(from, to) {
        if (xor(empty <- identical(from, emptyenv()),
                         identical(to  , emptyenv())))
            stop("did the class definition change?")
        if (empty)
            return()
        if (!identical(class(from), class(to)))
            stop("did the class definition change?")
        for (field in get(class(from)[[1L]], envir = .irisClassTable)@fields) {
            if (field@final && field@hasValue)
                next
            name <- field@name
            if (!exists(name, envir = from, inherits = FALSE) ||
                !exists(name, envir = to  , inherits = FALSE))
                stop("did the class definition change?")
            if (xor(active <- bindingIsActive(name, from),
                              bindingIsActive(name, to  )))
                stop("did the class definition change?")
            if (active) {
                fromEnv <- environment(activeBindingFunction(name, from))
                toEnv   <- environment(activeBindingFunction(name, to  ))
                if (shallow)
                    assign("VALUE", get("VALUE", envir = fromEnv, inherits = FALSE), envir = toEnv)
                else {
                    current <- get("VALUE", envir = fromEnv, inherits = FALSE)
                    if (is(current, irisRefClass.className) ||
                        is(current, envRefClass.className))
                        current <- current$copy(FALSE)
                    assign("VALUE", current, envir = toEnv)
                }
                if (field@final) {
                    initialized <- get("initialized", envir = fromEnv, inherits = FALSE)
                    assign("initialized", initialized, envir = toEnv)
                    if (initialized) {
                        lockEnvironment(toEnv, bindings = TRUE)
                        lockBinding(name, to)
                    }
                }
            } else {
                if (shallow)
                    assign(name, get(name, envir = from, inherits = FALSE), envir = to)
                else {
                    current <- get(name, envir = from, inherits = FALSE)
                    if (is(current, irisRefClass.className) ||
                        is(current, envRefClass.className))
                        current <- current$copy(FALSE)
                    assign(name, current, envir = to)
                }
            }
        }
        .copy.check(parent.env(from), parent.env(to))
    }
    value <- new(class(x)[[1L]])
    .copy.check(x, value)
    value
}, list(
    irisRefClass.className = irisRefClass@className,
    envRefClass.className = `packageSlot<-`("envRefClass", "methods")
))


utils::globalVariables("envir")


.copy.fun <- function (shallow = FALSE)
copy(envir, shallow)


.field.fun <- function(name, value) NULL
body(.field.fun) <- substitute({
    if (missing(value))
        dollarGet(envir, name)
    else dollarAssign(envir, name, value)
}, list(
    dollarGet = as.symbol(dollarGet),
    dollarAssign = as.symbol(dollarAssign)
))


.getClass.fun <- function (...)
if (nargs()) getClass(...) else getClass(class(envir))


.getIrisClass.fun <- function (Class = class(envir)[[1L]])
get(x = Class, envir = .irisClassTable, inherits = FALSE)


.show <- function (envir)
{
    cat(attr(envir, "string"), sep = "\n")
    tmp <- envir
    exclude <- character()
    allPublicMethods <- character()
    while (!identical(tmp, emptyenv())) {
        def <- get(x = class(tmp)[[1L]], envir = .irisClassTable, inherits = FALSE)
        fields  <- def@fields
        methods <- def@methods
        fields  <- fields [!(names(fields ) %in% exclude)]
        methods <- methods[!(names(methods) %in% exclude)]
        exclude <- c(exclude, names(fields), names(methods))
        fields  <- fields [vapply(fields , attr, "access", FUN.VALUE = "") == "public"]
        methods <- methods[vapply(methods, attr, "access", FUN.VALUE = "") == "public"]
        is.property <- vapply(methods, is, irisPropertyDef@className, FUN.VALUE = NA)
        properties <- methods[ is.property]
        methods    <- methods[!is.property]
        for (fi in fields) {
            if (fi@final && !fi@hasValue && {
                tmpEnv <- environment(activeBindingFunction(fi@name, tmp))
                !get("initialized", tmpEnv, inherits = FALSE)
            }) {
                cat("Field \"", fi@name, "\" is not initialized\n", sep = "")
            } else {
                cat("Field \"", fi@name, "\":\n", sep = "")
                show(.getWithoutCheck(fi@name, tmp))
            }
            cat("\n")
        }
        for (pr in names(properties)) {
            cat("Property \"", pr, "\":\n", sep = "")
            show(.getWithoutCheck(pr, tmp)@.Data)
            cat("\n")
        }
        # if (identical(def@className, irisRefClass@className)) {
        #     specialNames <- c(".irisClassDef", ".objectPackage")
        #     is.special <- names(methods) %in% specialNames
        #     specials <- methods[ is.special]
        #     methods  <- methods[!is.special]
        #     for (sp in specials) {
        #         cat("Field \"", sp@name, "\":\n", sep = "")
        #         show(envir[[sp@name]])
        #         cat("\n")
        #     }
        # }
        allPublicMethods <- c(allPublicMethods, names(methods))
        tmp <- parent.env(tmp)
    }
    cat("Methods:\n")
    cat(format.default(encodeString(allPublicMethods, quote = "\"")), fill = TRUE, labels = "    ")
    cat("\n")
    invisible(envir)
}


.show.fun <- function ()
.show(envir)


tmp <- irisClassDef(
    className = irisRefClass@className,
    extends = c(irisRefClass@className, "environment"),
    methods = list(
        .irisClassDef = irisMethodDef(access = "public", name = ".irisClassDef", value = as.activeBindingFunction(function(.self, value) {
            envir <- get("envir", envir = sys.frame(-1L), mode = "environment", inherits = FALSE)
            get(x = class(envir)[[1L]], envir = .irisClassTable, inherits = FALSE)
        })),
        .objectPackage = irisMethodDef(access = "public", name = ".objectPackage", value = as.activeBindingFunction(function(.self, value) {
            envir <- get("envir", envir = sys.frame(-1L), mode = "environment", inherits = FALSE)
            getClass(class(envir))@package
        })),
        copy = irisMethodDef(name = "copy", value = as.activeBindingFunction(function(.self) {
            envir <- get("envir", envir = sys.frame(-1L), mode = "environment", inherits = FALSE)
            `environment<-`(.copy.fun, environment())
        })),
        field = irisMethodDef(name = "field", value = as.activeBindingFunction(function(.self) {
            envir <- get("envir", envir = sys.frame(-1L), mode = "environment", inherits = FALSE)
            # cat("envir = ")
            # print(envir)
            `environment<-`(.field.fun, environment())
        })),
        getClass = irisMethodDef(name = "getClass", value = as.activeBindingFunction(function(.self) {
            envir <- get("envir", envir = sys.frame(-1L), mode = "environment", inherits = FALSE)
            `environment<-`(.getClass.fun, environment())
        })),
        getIrisClass = irisMethodDef(name = "getIrisClass", value = as.activeBindingFunction(function(.self) {
            envir <- get("envir", envir = sys.frame(-1L), mode = "environment", inherits = FALSE)
            `environment<-`(.getIrisClass.fun, environment())
        })),
        show = irisMethodDef(name = "show", value = as.activeBindingFunction(function(.self) {
            envir <- get("envir", envir = sys.frame(-1L), mode = "environment", inherits = FALSE)
            `environment<-`(.show.fun, environment())
        }))
    )
)
tmp$new <- irisClassGeneratorFunction(tmp@className)
assign(tmp@className, tmp, envir = .irisClassTable)
rm(tmp)


setMethod("initialize", irisClassDef@className,
function (.Object, className, fields = list(), contains = character(),
    methods = list(), where = topenv(parent.frame(4)), validity = NULL,
    ...)
{
    if (!is.character(className))
        className <- as.character(className)
    if (length(className) != 1L)
        className <- className[1L]
    if (is.na(className) || className == "")
        stop("invalid 'className'")
    className <- paste0(getPackageName(where), "::", className)


    fields <- lapply(fields, as.irisFieldDef_or_irisPropertyDef, where = where)
    fieldNames <- vapply(fields, function(xx) xx@name, "")
    if (anyDuplicated(fieldNames))
        stop("invalid 'fields', duplicated names")
    names(fields) <- fieldNames


    if (!is.character(contains))
        contains <- as.character(contains)
    if (length(contains) != 1L)
        contains <- contains[1L]
    if (is.na(contains))
        contains <- irisRefClass@className
    else if (!extends(contains, irisRefClass@className))
        stop("invalid 'contains', ", dQuote(contains), " is not a subclass of ", dQuote(irisRefClass@className))


    methods <- lapply(methods, as.irisMethodDef)
    methodNames <- vapply(methods, function(xx) xx@name, "")
    if (anyDuplicated(methodNames))
        stop("invalid 'methods', duplicated names")
    if (anyDuplicated(c(fieldNames, methodNames)))
        stop("invalid 'fields' and 'methods', duplicated names")
    names(methods) <- methodNames


    is.property <- vapply(fields, is, irisPropertyDef@className, FUN.VALUE = NA)
    if (any(is.property)) {
        methods <- c(fields[is.property], methods)
        fields <- fields[!is.property]
    }


    className <- setClass(Class = className, contains = contains, where = where, validity = validity)@className


    extends <- extends(className)
    i <- match(irisRefClass@className, extends)
    if (is.na(i))
        stop()
    extends <- c(extends[seq_len(i)], "environment")


    selfEnv <- new.env(parent = emptyenv())
    .Object@.xData    <- selfEnv
    .Object@className <- className
    .Object@fields    <- fields
    .Object@extends   <- extends
    .Object@methods   <- methods
    selfEnv$new <- irisClassGeneratorFunction(className)
    lockBinding("new", selfEnv)


    assign(className, .Object, envir = .irisClassTable)


    .Object
})


# irisRefClass    ----


.getWithoutCheck <- function (sym, env)
{
    if (bindingIsActive(sym, env))
        environment(activeBindingFunction(sym, env))$VALUE
    else get(x = sym, envir = env, inherits = FALSE)
}


getWithoutCheck <- function (sym, env)
{
    tmp <- env
    while (!identical(tmp, emptyenv())) {
        if (exists(x = sym, envir = tmp, inherits = FALSE))
            break
        tmp <- parent.env(tmp)
    }
    .getWithoutCheck(sym, tmp)
}


private.check <- function(x, n = 4L) NULL
body(private.check) <- substitute({
    # print(sys.calls())
    # print(sys.frames())
    # print(sys.parents())
    # if (!(sys.nframe() >= n))
    #     return(FALSE)
    # cat("sys.call(", -n, ") = ", sep = "")
    # tmp <- sys.call(-n) ; print(tmp)
    # cat("sys.frame(", -n, ") = ", sep = "")
    # tmp <- sys.frame(-n) ; print(tmp)
    # cat("sys.nframe() = ", sep = "")
    # tmp <- sys.nframe() ; print(tmp)
    # cat("sys.function(", -n, ") = ", sep = "")
    # tmp <- sys.function(-n) ; print(tmp)
    # for (N in 1:sys.nframe()) {
    #     cat("sys.parent(", N, ") = ", sep = "")
    #     tmp <- sys.parent(N) ; print(tmp)
    # }
    # cat("x = ")
    # print(x)
    # cat("sys.function(sys.parent(", n - 1L, ")) = ", sep = "")
    # tmp <- sys.function(sys.parent(n - 1L)) ; print(tmp)
    # for (N in seq.int(1L, length.out = sys.nframe() - 1)) {
    #     cat("sys.function(sys.parents()[[sys.nframe() - ", N, "]]) = ", sep = "")
    #     tmp <- sys.function(print(sys.parents()[[print(sys.nframe()) - N]]))
    #     print(tmp)
    # }
    (n <- sys.nframe() - n) > 0L &&
        is(sys.function(n <- sys.parents()[[n]]), .) &&
        identical(sys.function(n)@boundTo, x)
    # sys.nframe() >= n &&
    #     is(sys.function(-n), .) &&
    #     identical(sys.function(-n)@boundTo, x)
}, list(. = irisMethod@className))


protected.check <- function(x, n = 4L) NULL
body(protected.check) <- substitute({
    (n <- sys.nframe() - n) > 0L &&
        is(sys.function(n <- sys.parents()[[n]]), .) &&
        {
            env <- sys.function(n)@boundTo
            done <- FALSE
            while (!identical(env, emptyenv())) {
                if (done <- identical(env, x))
                    break
                env <- parent.env(env)
            }
            done
        }
}, list(. = irisMethod@className))


setMethod(initialize, irisRefClass@className,
function (.Object, ...)
{
    # .Object <- structure(list(), class = irisRefClass@className) ; stop("delete this")


    x <- get(x = class(.Object)[[1L]], envir = .irisClassTable, inherits = FALSE)


    getString <- function() {
        # '.Object' should format like "<environment: 0x0123456789abcdef>"
        # extract the substring '0x0123456789abcdef'
        # then write something like <"className" object at 0x0123456789abcdef>
        paste0("<\"", x@className, "\" object at ", sub("^<environment: (.*)>$", "\\1", format.default(.Object)), ">")
    }


    if (identical(x, .irisClassTable[[irisRefClass@className]])) {
        .Object <- new.env(parent = emptyenv())
        string <- getString()
        # the superclass, but since irisRefClass is the base class,
        # there is no superclass, and so we produce an error
        super <- function(value) NULL
        body(super) <- substitute(stop(msg, call. = FALSE), list(
            msg = paste0("there is no superclass of ", string)
        ))
        environment(super) <- baseenv()
        makeActiveBinding("super", super, .Object)
        lockBinding("super", .Object)
        nms <- character()
    } else {
        VALUE <- new(x@extends[[2L]])
        .Object <- new.env(parent = VALUE)
        string <- getString()
        super <- function(value) {
            if (missing(value))
                VALUE
            else if (!identical(VALUE, value))
                stop("cannot change value of locked binding for 'super'")
            else invisible(VALUE)
        }
        environment(super) <- list2env(list(VALUE = VALUE), parent = baseenv())
        makeActiveBinding("super", super, .Object)
        nms <- attr(VALUE, "field and method names")
    }


    for (method in x@methods) {


        # method <- x@methods$copy ; stop("remove this")


        VALUE <- method@value
        makeVALUEactive <- is.activeBindingFunction(VALUE)
        if (!method@static) {
            nm <- names(formals(VALUE))[[1L]]
            formals(VALUE) <- formals(VALUE)[-1L]
            env <- list2env(
                structure(list(.Object), names = nm),
                parent = environment(VALUE)
            )
            lockEnvironment(env, bindings = TRUE)
            environment(VALUE) <- env
        }
        is.property <- is(method, irisPropertyDef@className)
        VALUE <- irisMethod(VALUE,
            access = method@access,
            static = method@static,
            name   = method@name,
            string = paste0("<", if (is.property) "property" else "bound method",
                " \"", method@name, "\" of ", string, ">"),
            boundTo = .Object
        )
        needs.an.active.binding <- FALSE
        if (is.property) {
            needs.an.active.binding <- TRUE
            fun <- function(value) {
                if (missing(value))
                    VALUE()
                else VALUE(value)
            }
            do_lock <- FALSE
            parent <- baseenv()
        } else {
            fun <- function(value) VALUE
            do_lock <- TRUE
            parent <- emptyenv()
        }
        env <- list()
        switch (method@access,
        private = {
            needs.an.active.binding <- TRUE
            body(fun) <- substitute({
                if (private.check(.Object))
                    funBody
                else stop(msg, call. = FALSE)
            }, list(
                .Object = .Object,
                funBody = body(fun),
                msg = paste0("method \"", method@name, "\" of ", string, " is private")
            ))
            env$private.check <- private.check
            parent <- baseenv()
        },
        protected = {
            needs.an.active.binding <- TRUE
            body(fun) <- substitute({
                if (protected.check(.Object))
                    funBody
                else stop(msg, call. = FALSE)
            }, list(
                .Object = .Object,
                funBody = body(fun),
                msg = paste0("method \"", method@name, "\" of ", string, " is protected")
            ))
            env$protected.check <- protected.check
            parent <- baseenv()
        },
        public = {
        },
        {
            stop("invalid method@access; should never happen, please report!")
        })
        if (needs.an.active.binding) {
            env <- list2env(env, parent = parent)
            if (makeVALUEactive) {
                makeActiveBinding("VALUE", VALUE, env)
            } else env$VALUE <- VALUE
            lockEnvironment(env, bindings = TRUE)
            environment(fun) <- env
            makeActiveBinding(method@name, fun, .Object)
        } else if (makeVALUEactive) {
            makeActiveBinding(method@name, VALUE, .Object)
        } else {
            assign(method@name, VALUE, .Object)
        }
        if (do_lock)
            lockBinding(method@name, .Object)
    }


    for (field in x@fields) {
        VALUE <- field@value
        env <- new.env(parent = baseenv())
        list2env(list(VALUE = VALUE), env)
        fun <- function(value) NULL
        needs.an.active.binding <- FALSE
        if (do_lock <- field@final && field@hasValue) {
        } else {
            if (identical(field@className, `packageSlot<-`("ANY", "methods"))) {
                do_assign <- quote(VALUE <<- value)
            } else {
                needs.an.active.binding <- TRUE
                do_assign <- substitute({
                    valueClass <- class(value)
                    if (.identC(cl, valueClass)) {
                        VALUE <<- value
                    } else {
                        ok <- possibleExtends(valueClass, cl)
                        # if (isFALSE(ok))
                        #     stop(sprintf("assignment of an object of class %s is not valid for field %s in %s; is(value, \"%s\") is not TRUE",
                        #         paste(dQuote(valueClass), collapse = ", "),
                        #         sQuote(name), string, cl))
                        # else if (isTRUE(ok))
                        #     VALUE <<- value
                        # else VALUE <<- as(value, cl, strict = FALSE, ext = ok)
                        if (isFALSE(ok))
                            stop(sprintf("assignment of an object of class %s is not valid for field %s in %s; is(value, \"%s\") is not TRUE",
                                paste(dQuote(valueClass), collapse = ", "),
                                sQuote(name), string, cl))
                        VALUE <<- value
                    }
                }, list(
                    cl     = field@className,
                    name   = field@name,
                    string = string
                ))
                list2env(list(
                    .identC         = .identC,
                    possibleExtends = possibleExtends,
                    as              = as
                ), env)
                lockBinding(".identC"        , env)
                lockBinding("possibleExtends", env)
                lockBinding("as"             , env)
            }
        }
        if (field@final) {
            if (field@hasValue) {
                body(fun) <- substitute({
                    if (missing(value))
                        VALUE
                    else stop(cannot.change.value.msg, call. = FALSE)
                }, list(
                    cannot.change.value.msg = paste0("cannot change value of final field '", field@name, "'")
                ))
                lockBinding("VALUE", env)
            } else {
                needs.an.active.binding <- TRUE
                body(fun) <- substitute({
                    if (missing(value)) {
                        if (initialized)
                            VALUE
                        else stop(not.init.msg, call. = FALSE)
                    }
                    else if (initialized)
                        stop(cannot.change.value.msg, call. = FALSE)
                    else {
                        initialized <<- TRUE
                        do_assign
                        lockEnvironment(env, bindings = TRUE)
                        lockBinding(name, .Object)
                        invisible(VALUE)
                    }
                }, list(
                    do_assign = do_assign,
                    not.init.msg = paste0("field \"", field@name, "\" of ", string, " is not initialized"),
                    cannot.change.value.msg = paste0("cannot change value of final field '", field@name, "'"),
                    env = env,
                    name = field@name,
                    .Object = .Object
                ))
                list2env(list(initialized = FALSE), env)
            }
        } else {
            body(fun) <- substitute({
                if (missing(value))
                    VALUE
                else do_assign
            }, list(
                do_assign = do_assign
            ))
        }
        switch (field@access,
        private = {
            needs.an.active.binding <- TRUE
            body(fun) <- substitute({
                if (private.check(.Object))
                    funBody
                else stop(msg, call. = FALSE)
            }, list(
                .Object = .Object,
                funBody = body(fun),
                msg = paste0("field \"", field@name, "\" of ", string, " is private")
            ))
            list2env(list(private.check = private.check), env)
        },
        protected = {
            needs.an.active.binding <- TRUE
            body(fun) <- substitute({
                if (protected.check(.Object))
                    funBody
                else stop(msg, call. = FALSE)
            }, list(
                .Object = .Object,
                funBody = body(fun),
                msg = paste0("field \"", field@name, "\" of ", string, " is protected")
            ))
            list2env(list(protected.check = protected.check), env)
        },
        public = {
        },
        {
            stop("invalid field@access; should never happen, please report!")
        })
        if (needs.an.active.binding) {
            lockEnvironment(env)
            environment(fun) <- env
            makeActiveBinding(field@name, fun, .Object)
        } else {
            assign(field@name, VALUE, .Object)
        }
        if (do_lock)
            lockBinding(field@name, .Object)
    }
    lockEnvironment(.Object)
    attr(.Object, "string") <- string
    attr(.Object, "field and method names") <- unique(c(
        names(x@fields) [vapply(x@fields , attr, "access", FUN.VALUE = "") == "public"],
        names(x@methods)[vapply(x@methods, attr, "access", FUN.VALUE = "") == "public"],
        nms
    ))
    class(.Object) <- x@extends
    .Object
})


setMethod("show", irisRefClass@className,
function (object)
{
    cat(attr(object, "string"), sep = "\n")
    invisible()
})


assign(paste0("print.", irisRefClass@className),
function (x, ...)
{
    cat(attr(x, "string"), sep = "\n")
    invisible(x)
})


assign(paste0("names.", irisRefClass@className),
function(x) attr(x, "field and method names"))


setIrisClass <- function(Class, fields = list(), contains = character(),
    methods = list(), where = topenv(parent.frame()), validity = NULL) NULL
body(setIrisClass) <- substitute(new(Class = CLASS, className = Class, fields = fields,
    contains = contains, methods = methods, where = where, validity = validity),
    list(CLASS = irisClassDef@className))


# my classes      ----


testClass1 <- setIrisClass(
    Class = "testClass1",
    fields = list(
        irisFieldDef(name = ".a", value = 0:4),
        irisFieldDef(class = "numeric", name = "a", value = 5:9)
    ),
    methods = list(
        irisMethodDef(name = "testing_field", value = function(.self) {
            cat("> .self$.a\n")
            print(.self$.a)
            cat("\n")


            cat('> .self[[".a"]]\n')
            print(.self[[".a"]])
            cat("\n")


            cat('> .self$field(".a")\n')
            print(.self$field(".a"))
        }),
        irisMethodDef(access = "protected", name = "initialize", value = function(.self, .a, a, ...) {
            # .self$super$initialize(...)
            if (!missing(.a))
                .self$.a <- .a
            if (!missing(a))
                .self$a <- a
        }),
        irisMethodDef(name = "static_fun", value = function(...) {
            list(...)
        }),
        irisMethodDef(name = "print", value = function(.self, ...) {
            .self$.print()
        }),
        irisMethodDef(name = ".print", value = function(.self, ...) {
            print(.self)
        }),
        irisMethodDef(access = "protected", name = "protected_fun", value = function(.self, ...) {
            cat("in method 'protected_fun' of class 'testClass1'\n")
            invisible(.self)
        }),
        irisMethodDef(name = "setPackageSlot", value = function(.self, value) {
            cat("in method 'setPackageSlot' of class 'testClass1'\n")
            .self$.a <- "test"
            packageSlot(.self$.a) <- value
            names(packageSlot(.self$.a)) <- NULL
            invisible()
        })
     )
)
#
#
# testClass2 <- setIrisClass(
#     Class = "testClass2",
#     fields = list(
#         irisFieldDef(name = "letters", value = letters)
#     ),
#     contains = testClass1@className,
#     methods = list(
#         irisMethodDef(access = "protected", name = "initialize", value = function(.self, ...) {
#             cat("in 'initialize' of 'testClass2'\n")
#             .self$super$initialize(...)
#         })
#     )
# )
#
#
# testClass3 <- setIrisClass(
#     Class = "testClass3",
#     fields = list(
#         irisFieldDef(final = TRUE , className = "ANY"    , name = "x1", value = pi),
#         irisFieldDef(final = TRUE , className = "numeric", name = "x2", value = pi),
#         irisFieldDef(final = FALSE, className = "ANY"    , name = "x3", value = pi),
#         irisFieldDef(final = FALSE, className = "numeric", name = "x4", value = pi),
#         irisFieldDef(final = TRUE , className = "numeric", name = "x5")
#     )
# )
#
#
# obj <- testClass1$new(.a = data.frame(), a = pi)
# obj2 <- testClass2$new()
#
#
# getWithoutCheck(".a", obj)
# getWithoutCheck("a" , obj)
# getWithoutCheck(".a", obj2)
# getWithoutCheck("a" , obj2)
#
#
# obj$testing_field()
#
#
# obj
# obj$print
# obj$print()
# obj$.print
# obj$protected_fun
#
#
# obj$a
# obj$.a
# obj$a <- 7
#
#
# obj3 <- testClass3$new()
#
#
# testClass4 <- setIrisClass(
#     "testClass4",
#     methods = list(
#         irisMethodDef(access = "protected", name = "initialize", value = function(.self, ...) {
#             cat("in 'initialize' of ")
#             print(.self)
#         }),
#         irisMethodDef(access = "public", name = "validate", value = function(.self) {
#             cat("in 'validate' of ")
#             print(.self)
#         }),
#         irisMethodDef(access = "private", name = "finalize", value = function(.self, e) {
#             cat("in 'finalize' of ")
#             print(.self)
#         })
#     )
# )
#
#
# obj4 <- testClass4$new()
# rm(obj4)
# invisible(gc())
#
#
#
#
#
# testClass5 <- setIrisClass(
#     "testClass5",
#     fields = list(
#         irisFieldDef(name = ".a", value = 1),
#         irisPropertyDef(name = "a", value = function(.self, value) {
#             if (missing(value)) {
#                 cat("getter for 'a' for ")
#                 print(.self)
#                 .self$.a
#             } else {
#                 cat("setter for 'a' for ")
#                 print(.self)
#                 .self$.a <- value
#             }
#         })
#     )
# )
#
#
# obj5 <- testClass5$new()
# obj5$a
# obj5$a <- 5
# obj5$a
