R.preserves.active.bindings <- TRUE


options(keep.source = FALSE)


this.namespace <- environment()


# bquote2 <- function (expr, where = parent.frame(), splice = FALSE)
# {
#     if (!is.environment(where))
#         where <- as.environment(where)
#     eval(substitute(bquote(expr, where, splice), list(
#         expr = substitute(expr),
#         where = where,
#         splice = if (splice) TRUE else FALSE
#     )), parent.frame())
# }


bquote2 <- function (expr, where = parent.frame(), splice = TRUE, sexpr = substitute(expr))
{
    if (!is.environment(where))
        where <- as.environment(where)
    unquote <- function(e) {
        if (is.pairlist(e))
            as.pairlist(lapply(e, unquote))
        else if (is.call(e)) {
            if (is.name(e[[1L]]) && e[[1L]] == ".")
                eval(e[[2L]], where)
            else if (splice) {
                if (is.name(e[[1L]]) && e[[1L]] == "..")
                    stop("can only splice inside a call", call. = FALSE)
                else as.call(unquote.list(e))
            }
            else as.call(lapply(e, unquote))
        }
        else e
    }
    is.splice.macro <- function(e) is.call(e) && is.name(e[[1L]]) && e[[1L]] == ".."
    unquote.list <- function(e) {
        p <- Position(is.splice.macro, e, nomatch = NULL)
        if (is.null(p))
            lapply(e, unquote)
        else {
            n <- length(e)
            mexp <- eval(e[[p]][[2L]], where)
            if (is.call(mexp) && is.name(mexp[[1L]]) && mexp == "{")
                mexp <- as.list(mexp[-1L])
            else if (!is.vector(mexp) && !is.expression(mexp) && !is.null(mexp))
                stop("can only splice vectors")
            c(
                lapply(e[seq_len(p - 1L)], unquote),
                mexp,
                if (p < n) unquote.list(e[(p + 1L):n])
            )
        }
    }
    unquote(sexpr)
}


.printNames <- function (header, names, separateLine = TRUE)
{
    names <- format.default(encodeString(names, quote = "\""))
    if (separateLine) {
        cat(header, ":\n", sep = "")
        cat(names, fill = TRUE, labels = "    ")
    } else {
        cat(header, ": ", sep = "")
        cat(names, fill = TRUE)
    }
    cat("\n")
}


delayedAssign(".identC", methods:::.identC)


is.activeBindingFunction <- eval(bquote(
function(x)
is(x, .(`packageSlot<-`("activeBindingFunction", "methods")))
))


as.activeBindingFunction <- eval(bquote(
function(x)
new(.(`packageSlot<-`("activeBindingFunction", "methods")), x)
))


# FieldDef            ----


IrisFieldDef <- setClass(
    Class = paste0(.packageName, "::", "FieldDef"),
    slots = list(
        access    = "character",
        static    = "logical"  ,
        final     = "logical"  ,
        className = "character",
        name      = "character",
        value     = `packageSlot<-`("ANY", "methods"),
        hasValue  = "logical"
    ),
    sealed = TRUE
)
body(IrisFieldDef@.Data) <- bquote(new(Class = .(IrisFieldDef@className), ...))


setMethod(initialize, IrisFieldDef@className, eval(bquote(
function (.Object, access = NA, static = FALSE, final = FALSE,
    className = .(`packageSlot<-`("ANY", "methods")), name, value,
    where = topenv(parent.frame(4)), ...)
{
    .Object <- callNextMethod(.Object = .Object, ...)


    name <- as.character(as.symbol(name))
    if (name == "super")
        stop("invalid 'name'; \"super\" is a reserved name")


    access <- as.character(access)[[1L]]
    if (is.na(access))
        access <- if (startsWith(name, ".")) "private" else "public"
    else access <- match.arg(access, c("private", "public", "protected"))


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
        getClass(className, where = where)
    } else getClass(className)
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
)))


setMethod(show, IrisFieldDef@className, eval(bquote(
function (object)
{
    cat(c(
        object@access,
        if (object@static) "static",
        if (object@final) "final",
        if (!identical(object@className, .(`packageSlot<-`("ANY", "methods"))))
            encodeString(object@className, quote = "\""),
        encodeString(object@name, quote = "\""),
        "="
    ))
    if (object@final && !object@hasValue) {
        cat(" ;\n")
    } else {
        cat("\n")
        print(object@value)
    }
    invisible()
}
)))


# MethodDef           ----


IrisMethodDef <- setClass(
    Class = paste0(.packageName, "::", "MethodDef"),
    slots = list(
        access = "character",
        static = "logical"  ,
        name   = "character",
        value  = "function"
    ),
    sealed = TRUE
)
body(IrisMethodDef@.Data) <- bquote(new(Class = .(IrisMethodDef@className), ...))


setMethod(initialize, IrisMethodDef@className,
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
}
)


setMethod(show, IrisMethodDef@className,
function (object)
{
    cat(c(
        object@access,
        if (object@static) "static",
        encodeString(object@name, quote = "\"")
    ))
    cat("\n")
    print(object@value)
    invisible()
}
)


# PropertyDef         ----


IrisPropertyDef <- setClass(
    Class = paste0(.packageName, "::", "PropertyDef"),
    contains = IrisMethodDef@className,
    sealed = TRUE
)
body(IrisPropertyDef@.Data) <- bquote(new(Class = .(IrisPropertyDef@className), ...))


# MemberDef           ----


IrisMemberDef <- setClassUnion("MemberDef", c(
    IrisFieldDef@className,
    IrisMethodDef@className,
    IrisPropertyDef@className
))


# method              ----


IrisMethod <- setClass(
    Class = paste0(.packageName, "::", "method"),
    contains = "function",
    slots = list(
        access = "character",
        static = "logical"  ,
        name   = "character",


        is.active   = "logical",
        is.property = "logical",
        boundTo     = `packageSlot<-`("ANY", "methods")
    ),
    sealed = TRUE
)
body(IrisMethod@.Data) <- bquote(new(Class = .(IrisMethod@className), ...))


setMethod(initialize, IrisMethod@className, eval(bquote2(
function (.Object, method, boundTo, ...)
{
    .Object@access  <- method@access
    .Object@static  <- method@static
    .Object@name    <- method@name
    .Object@is.active   <- is.activeBindingFunction(method@value)
    .Object@is.property <- is(method, .(IrisPropertyDef@className))
    .Object@boundTo <- boundTo
    VALUE <- method@value
    if (!method@static) {
        nm <- names(formals(VALUE))[[1L]]
        formals(VALUE) <- formals(VALUE)[-1L]
        env <- list2env(
            structure(list(boundTo), names = nm),
            parent = environment(VALUE)
        )
..(
if (R.preserves.active.bindings) quote({
        makeActiveBinding("super", activeBindingFunction("super", boundTo), env)
        lockEnvironment(env)
        lockBinding(nm, env)
})
else quote({
        eval(substitute(delayedAssign(x, value, eval.env, assign.env), list(
            x = "super",
            value = substitute(get(x = "super", envir = boundTo, inherits = FALSE)(), list(boundTo = boundTo)),
            eval.env = baseenv(),
            assign.env = env
        )))
        lockEnvironment(env, bindings = TRUE)
})
)


        # lockEnvironment(env, bindings = TRUE)
        environment(VALUE) <- env
    }
    .Object@.Data <- VALUE
    .Object
}
)))


assign(paste0("toString.", IrisMethod@className),
function (x, ...)
{
    paste0("<", if (x@is.property) "property " else "bound method ", encodeString(x@name, quote = "\""), " of ", toString(x@boundTo), ">")
})


setMethod(show, IrisMethod@className,
function (object)
{
    cat(toString(object), sep = "\n")
    print(x = if (isS4(object)) object@.Data else object, useSource = FALSE)
    invisible()
})


assign(paste0("print.", IrisMethod@className),
function (x, ...)
{
    cat(toString(x), sep = "\n")
    print(x = if (isS4(x)) x@.Data else x, useSource = FALSE, ...)
    invisible(x)
})


# ClassRepresentation ----


IrisClassRepresentation <- setClass(
    Class = paste0(.packageName, "::", "ClassRepresentation"),
    contains = `packageSlot<-`("classRepresentation", "methods"),
    slots = list(
        fields  = "list",
        methods = "list"
    )
)
body(IrisClassRepresentation@.Data) <- bquote(new(Class = .(IrisClassRepresentation@className), ...))


# class               ----


IrisClassDef <- setClass(
    Class = paste0(.packageName, "::", "class"),
    contains = "environment",
    slots = list(
        className = "character",
        fields    = "list"     ,
        extends   = "character",
        methods   = "list"
    ),
    sealed = TRUE
)
body(IrisClassDef@.Data) <- bquote(new(Class = .(IrisClassDef@className), ...))


.irisClassTable <- new.env(parent = emptyenv())


irisClassGeneratorFunction <- function (className)
{
    generator <- function(...) NULL
    body(generator) <- substitute({
        .Object <- new(CLASS)
        if (exists("initialize", envir = .Object, inherits = FALSE)) {
            initialize <- .getWithoutCheck("initialize", .Object)
            if (is(initialize, IrisMethod.className) && !initialize@static)
                initialize(...)
        }
        tmp <- .Object
        while (!identical(tmp, emptyenv())) {
            if (exists("finalize", envir = tmp, inherits = FALSE)) {
                finalize <- .getWithoutCheck("finalize", tmp)
                if (is(finalize, IrisMethod.className) && !finalize@static)
                    reg.finalizer(tmp, finalize, onexit = TRUE)
            }
            tmp <- parent.env(tmp)
        }
        if (exists("validate", envir = .Object, inherits = FALSE)) {
            validate <- .getWithoutCheck("validate", .Object)
            if (is(validate, IrisMethod.className) && !validate@static)
                validate()
        }
        .Object
    }, list(
        CLASS = className,
        IrisMethod.className = IrisMethod@className
    ))
    environment(generator) <- this.namespace
    return(generator)
}


init.IrisClassDef <- function (baseCase)
{
    eval(bquote2(
function (.Object, className, members = list(), contains = character(),
    where = topenv(parent.frame(4)), validity = NULL, sealed = FALSE,
    ...)
{
    .Object <- callNextMethod(.Object = .Object, ...)


    className <- as.character(className)[[1L]]
    if (is.na(className) || className == "")
        stop("invalid 'className'")
    className <- paste0(getPackageName(where), "::", className)


    members <- lapply(members, function(xx) {
        if (!is(xx, IrisMemberDef@className))
            stop("invalid 'members', must be a list of field, method, and property definitions")
        xx
    })
    nms <- vapply(members, function(xx) xx@name, "")
    if (anyDuplicated(nms))
        stop("invalid 'members', duplicated names")
    names(members) <- nms


..(
if (baseCase) quote({
    contains <- "environment"
}) else bquote({
    if (identical(contains, character()))
        contains <- .(IrisObject@className)
    contains <- getClass(contains, where = where)@className
    if (!extends(contains, .(IrisObject@className)))
        stop("invalid 'contains', ", dQuote(contains), " is not a subclass of ", dQuote(.(IrisObject@className)))
})
)


    is.method <- vapply(members, is, IrisMethodDef@className, FUN.VALUE = NA)
    methods <- members[is.method]
    fields <- members[!is.method]


    rep <- makeClassRepresentation(name = className, superClasses = contains,
        package = getPackageName(where = where), validity = validity,
        sealed = sealed, where = where)
    rep <- IrisClassRepresentation(rep, fields = fields, methods = methods)
    className <- setClass(Class = rep@className, representation = rep,
        where = where)@className


..(
if (baseCase) quote({
    extends <- c(className, "environment")
}) else bquote({
    extends <- extends(className)
    i <- match(.(IrisObject@className), extends)
    if (is.na(i))
        stop("internal error; should never happen, please report!")
    extends <- c(extends[seq_len(i)], "environment")
})
)


    selfEnv <- new.env(parent = emptyenv())
    .Object@.xData    <- selfEnv
    .Object@className <- className
    .Object@fields    <- fields
    .Object@extends   <- extends
    .Object@methods   <- methods
    selfEnv$new <- irisClassGeneratorFunction(className)
    lockEnvironment(selfEnv, bindings = TRUE)


    assign(className, .Object, .irisClassTable)


..(
if (!baseCase) quote({
    action <- function(ns) NULL
    body(action) <- substitute({
        parent.env(selfEnv) <- get(contains, .irisClassTable)
        assign(className, .Object, .irisClassTable)
    }, list(
        className = className,
        contains = contains
    ))
    environment(action) <- list2env(list(.Object = .Object, selfEnv = selfEnv), parent = this.namespace)
    lockEnvironment(environment(action), bindings = TRUE)
    setLoadAction(action, paste0(".", className, ".fixup"), where = where)
})
)


..(
if (!baseCase) quote({
    hook <- function(ns) NULL
    body(hook) <- substitute({
        parent.env(selfEnv) <- get(contains, .irisClassTable)
        assign(className, .Object, .irisClassTable)
    }, list(
        className = className,
        contains = contains
    ))
    environment(action) <- list2env(list(.Object = .Object, selfEnv = selfEnv), parent = this.namespace)
    lockEnvironment(environment(action), bindings = TRUE)
    setLoadAction(action, paste0(".", className, ".fixup"), where = where)
})
)


    .Object
}
    ), parent.frame())
}


setMethod(initialize, IrisClassDef@className,
init.IrisClassDef(baseCase = TRUE))


setIrisClass <- eval(bquote2(
function (Class, members = list(), contains = character(), where = topenv(parent.frame()),
    validity = NULL, sealed = FALSE)
new(Class = .(IrisClassDef@className), className = Class, members = members,
    contains = contains, where = where, validity = validity,
    sealed = sealed)
))


setMethod(show, IrisClassDef@className,
function (object)
{
    # object <- get(IrisObject@className, iris:::.irisClassTable, inherits = FALSE)


    cat("<iris class ", encodeString(object@className, quote = "\""), ">\n", sep = "")
    fields <- object@fields
    if (length(fields) > 0L) {

    } else cat("No fields defined\n\n")
    methods <- object@methods
    if (length(methods)) {
        .printNames("Methods", names(methods))
    } else cat("No class methods\n\n")


})


# object              ----


get.envir.code <- if (R.preserves.active.bindings) quote({
    envir <- get("envir", envir = sys.frame(-1L), mode = "environment", inherits = FALSE)
}) else quote({
    envir <- get(".Object", envir = environment(sys.function(-1L)), mode = "environment", inherits = FALSE)
})


IrisObject <- setIrisClass(
    Class = "object",
    members = list(
        IrisMethodDef(access = "public", name = ".IrisClassDef" , value = as.activeBindingFunction(eval(bquote2(function(.self) {
            ..(get.envir.code)
            getIrisClass(envir)
        })))),
        IrisMethodDef(access = "public", name = ".objectPackage", value = as.activeBindingFunction(eval(bquote2(function(.self) {
            ..(get.envir.code)
            getClass(class(envir))@package
        })))),
        IrisMethodDef(                   name = "copy"          , value = as.activeBindingFunction(eval(bquote2(function(.self) {
            # print(names(sys.frame(-1L)))
            # cat("\n")
            # print(names(sys.frame(-2L)))
            # cat("\n")
            # print(names(sys.frame(-3L)))
            # cat("\n")
            # print(names(sys.frame(-4L)))
            # cat("\n")
            # print(sys.function(-1L))
            # cat("\n")
            # print(sys.function(-2L))
            # cat("\n")
            # print(sys.function(-3L))
            # cat("\n")
            # print(sys.function(-4L))
            # cat("\n")
            ..(get.envir.code)
            `environment<-`(.copy.fun, environment())
        })))),
        IrisMethodDef(                   name = "field"         , value = as.activeBindingFunction(eval(bquote2(function(.self) {
            ..(get.envir.code)
            `environment<-`(.field.fun, environment())
        })))),
        IrisMethodDef(                   name = "getClass"      , value = as.activeBindingFunction(eval(bquote2(function(.self) {
            ..(get.envir.code)
            `environment<-`(.getClass.fun, environment())
        })))),
        IrisMethodDef(                   name = "getIrisClass"  , value = as.activeBindingFunction(eval(bquote2(function(.self) {
            ..(get.envir.code)
            `environment<-`(.getIrisClass.fun, environment())
        })))),
        IrisMethodDef(                   name = "show"          , value = as.activeBindingFunction(eval(bquote2(function(.self) {
            ..(get.envir.code)
            `environment<-`(.show.fun, environment())
        }))))
    )
)


setMethod(initialize, IrisClassDef@className,
init.IrisClassDef(baseCase = FALSE))


classDef <- getClass(IrisObject@className)
fun <- function(...) NULL
body(fun) <- bquote(new(Class = .(classDef@className), ...))
fun <- as(fun, `packageSlot<-`("classGeneratorFunction", "methods"))
fun@className <- classDef@className
fun@package <- classDef@package
IrisObject <- fun
rm(classDef, fun)


assign(paste0("$.", IrisObject@className) -> dollarGet, eval(bquote2(
function (x, name)
.(
if (R.preserves.active.bindings) quote(
get(x = name, envir = x)
)
else quote(
get(x = name, envir = x)()
)
)
)))


assign(paste0("$<-.", IrisObject@className) -> dollarAssign, eval(bquote2(
function (x, name, value)
{
..(
if (R.preserves.active.bindings) quote({
    if (exists(x = name, envir = x))
        assign(x = name, value = value, envir = x, inherits = TRUE)
    else stop(gettextf("%s is not a field or method of %s",
        sQuote(name), toString(x)), call. = FALSE)
})
else quote({
    get(x = name, envir = x)(value)
})
)
    invisible(x)
}
)))


assign(paste0("[[.", IrisObject@className), eval(bquote2(
function (x, i, ...)
{
    if (...length())
        stop("incorrect number of dimensions")
..(
if (R.preserves.active.bindings) quote({
    get(x = i, envir = x)
})
else quote({
    get(x = i, envir = x)()
})
)
}
)))


assign(paste0("[[<-.", IrisObject@className), eval(bquote2(
function (x, i, ..., value)
{
    if (...length())
        stop("incorrect number of dimensions")
..(
if (R.preserves.active.bindings) quote({
    if (exists(x = i, envir = x))
        assign(x = i, value = value, envir = x, inherits = TRUE)
    else stop(gettextf("%s is not a field or method of %s",
        sQuote(i), toString(x)), call. = FALSE)
})
else quote({
    get(x = i, envir = x)(value)
})
)
    invisible(x)
}
)))


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
        for (field in getIrisClass(from)@fields) {
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
                    if (is(current, IrisObject.className) ||
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
                    if (is(current, IrisObject.className) ||
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
    IrisObject.className = IrisObject@className,
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


getIrisClass <- function(Class, where = topenv(parent.frame())) NULL
body(getIrisClass) <- substitute({
    if (is(Class, IrisClassRepresentation.className)) {
        classDef <- Class
        Class <- Class@className
    } else if (is.character(Class)) {
        classDef <- getClass(Class, where = where)
        if (!is(classDef, IrisClassRepresentation.className))
            stop(gettextf("class %s is defined but is not an iris class",
                dQuote(Class)))
    } else if (is(Class, IrisObject.className)) {
        Class <- class(Class)[[1L]]
        classDef <- getClass(Class, where = where)
        if (!is(classDef, IrisClassRepresentation.className))
            stop(gettextf("class %s is defined but is not an iris class",
                dQuote(Class)))
    } else stop(gettextf("class must be an iris class representation, a character string, or an iris class; got an object of class %s",
        dQuote(class(Class))))
    get(Class, envir = .irisClassTable)
}, list(
    IrisClassRepresentation.className = IrisClassRepresentation@className,
    IrisObject.className = IrisObject@className
))


.getIrisClass.fun <- function (Class = envir)
getIrisClass(Class)


.show <- function (envir)
{
    cat(toString(envir), sep = "\n")
    tmp <- envir
    exclude <- character()
    allPublicMethods <- character()
    while (!identical(tmp, emptyenv())) {
        def <- getIrisClass(tmp)
        fields  <- def@fields
        methods <- def@methods
        fields  <- fields [!(names(fields ) %in% exclude)]
        methods <- methods[!(names(methods) %in% exclude)]
        exclude <- c(exclude, names(fields), names(methods))
        fields  <- fields [vapply(fields , attr, "access", FUN.VALUE = "") == "public"]
        methods <- methods[vapply(methods, attr, "access", FUN.VALUE = "") == "public"]
        is.property <- vapply(methods, is, IrisPropertyDef@className, FUN.VALUE = NA)
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
        # if (identical(def@className, IrisObject@className)) {
        #     specialNames <- c(".IrisClassDef", ".objectPackage")
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
    .printNames("Methods", allPublicMethods)
    invisible(envir)
}


.show.fun <- function ()
.show(envir)


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


private.check <- eval(bquote(function (x, n = 4L)
{
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
        is(sys.function(n <- sys.parents()[[n]]), .(IrisMethod@className)) &&
        identical(sys.function(n)@boundTo, x)
    # sys.nframe() >= n &&
    #     is(sys.function(-n), .) &&
    #     identical(sys.function(-n)@boundTo, x)
}))


protected.check <- eval(bquote(function (x, n = 4L)
{
    (n <- sys.nframe() - n) > 0L &&
        is(sys.function(n <- sys.parents()[[n]]), .(IrisMethod@className)) &&
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
}))


assign(paste0("toString.", IrisObject@className),
function (x, ...)
paste0("<", encodeString(class(x)[[1L]], quote = "\""), " object at ", sub("^<environment: (.*)>$", "\\1", format.default(x)), ">"))


setMethod(initialize, IrisObject@className, eval(bquote2(
function (.Object, ...)
{
    # .Object <- structure(list(), class = IrisObject@className) ; stop("delete this")


    x <- getIrisClass(.Object)


    if (identical(x, .irisClassTable[[.(IrisObject@className)]])) {
        .Object <- new.env(parent = emptyenv())
        class(.Object) <- x@extends
        # the superclass, but since IrisObject is the base class,
        # there is no superclass, and so we produce an error
        super <- function(value) stop("there is no superclass of ", toString(.Object), call. = FALSE)
        environment(super) <- list2env(list(.Object = .Object), parent = this.namespace)
        lockEnvironment(environment(super), bindings = TRUE)
..(
if (R.preserves.active.bindings) quote({
        makeActiveBinding("super", super, .Object)
})
else quote({
        assign("super", super, envir = .Object)
})
)
        lockBinding("super", .Object)
        nms <- character()
    } else {
        VALUE <- new(x@extends[[2L]])
        .Object <- new.env(parent = VALUE)
        class(.Object) <- x@extends
        super <- function(value) {
            if (missing(value))
                VALUE
            else if (!identical(VALUE, value))
                stop("cannot change value of locked binding for 'super'")
            else invisible(VALUE)
        }
        environment(super) <- list2env(list(VALUE = VALUE), parent = this.namespace)
        lockEnvironment(environment(super), bindings = TRUE)
..(
if (R.preserves.active.bindings) quote({
        makeActiveBinding("super", super, .Object)
})
else quote({
        assign("super", super, envir = .Object)
        lockBinding("super", .Object)
})
)
        nms <- attr(VALUE, "field and method names")
    }


    for (method in x@methods) {


        # method <- x@methods$copy ; stop("remove this")
        method <- IrisMethod(method = method, boundTo = .Object)
..(
if (R.preserves.active.bindings) quote({
        needs.an.active.binding <- FALSE
        if (method@is.property) {
            needs.an.active.binding <- TRUE
            fun <- function(value) {
                if (missing(value))
                    method()
                else method(value)
            }
            do_lock <- FALSE
        } else {
            fun <- function(value) VALUE
            do_lock <- TRUE
        }
        switch (method@access,
        private = {
            needs.an.active.binding <- TRUE
            body(fun) <- substitute({
                if (private.check(.Object))
                    funBody
                else stop(toString(method), " is private", call. = FALSE)
            }, list(funBody = body(fun)))
        },
        protected = {
            needs.an.active.binding <- TRUE
            body(fun) <- substitute({
                if (protected.check(.Object))
                    funBody
                else stop(toString(method), " is protected", call. = FALSE)
            }, list(funBody = body(fun)))
        },
        public = {
        },
        {
            stop("invalid method@access; should never happen, please report!")
        })
        if (needs.an.active.binding) {
            env <- list2env(list(.Object = .Object, method = method), parent = this.namespace)
            if (method@is.active) {
                makeActiveBinding("VALUE", method, env)
            } else env$VALUE <- method
            lockEnvironment(env, bindings = TRUE)
            environment(fun) <- env
            makeActiveBinding(method@name, fun, .Object)
        } else if (method@is.active) {
            makeActiveBinding(method@name, method, .Object)
        } else {
            assign(method@name, method, envir = .Object)
        }
        if (do_lock)
            lockBinding(method@name, .Object)
})
else quote({
        if (method@is.property || method@is.active) {
            fun <- function(value) {
                if (missing(value))
                    VALUE()
                else VALUE(value)
            }
        } else {
            fun <- function(value) NULL
            body(fun) <- substitute({
                if (missing(value))
                    VALUE
                else stop(msg, call. = FALSE)
            }, list(
                msg = paste0("cannot change value of locked binding for '", method@name, "'")
            ))
        }
        switch (method@access,
        private = {
            body(fun) <- substitute({
                if (private.check(.Object))
                    funBody
                else stop(toString(method), " is private", call. = FALSE)
            }, list(funBody = body(fun)))
        },
        protected = {
            body(fun) <- substitute({
                if (protected.check(.Object))
                    funBody
                else stop(toString(method), " is protected", call. = FALSE)
            }, list(funBody = body(fun)))
        },
        public = {
        },
        {
            stop("invalid method@access; should never happen, please report!")
        })
        env <- list2env(list(.Object = .Object, VALUE = method), parent = this.namespace)
        lockEnvironment(env, bindings = TRUE)
        environment(fun) <- env
        assign(method@name, fun, envir = .Object)
        lockBinding(method@name, .Object)
})
)

    }


    for (field in x@fields) {
        VALUE <- field@value
        env <- list2env(list(.Object = .Object, VALUE = VALUE), parent = this.namespace)
        lockBinding(".Object", env)
        fun <- function(value) NULL
        needs.an.active.binding <- FALSE


        # if we're locking the field, we don't need do_assign
        if (do_lock <- field@final && field@hasValue) {


        # if we're not locking the field, we need do_assign
        } else {
            if (identical(field@className, .(`packageSlot<-`("ANY", "methods")))) {
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
                                sQuote(name), toString(.Object), cl))
                        VALUE <<- value
                    }
                }, list(
                    cl     = field@className,
                    name   = field@name
                ))
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
                        else stop(not.init.msg, toString(.Object), "> is not initialized", call. = FALSE)
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
                    not.init.msg = paste0("<field \"", field@name, "\" of "),
                    cannot.change.value.msg = paste0("cannot change value of final field '", field@name, "'"),
                    env = env,
                    name = field@name
                ))
                env$initialized <- FALSE
            }
        } else {
            body(fun) <- substitute({
                if (missing(value))
                    VALUE
                else do_assign
            }, list(do_assign = do_assign))
        }
        switch (field@access,
        private = {
            needs.an.active.binding <- TRUE
            body(fun) <- substitute({
                if (private.check(.Object))
                    funBody
                else stop(msg, toString(.Object), "> is private", call. = FALSE)
            }, list(
                funBody = body(fun),
                msg = paste0("<field \"", field@name, "\" of ")
            ))
        },
        protected = {
            needs.an.active.binding <- TRUE
            body(fun) <- substitute({
                if (protected.check(.Object))
                    funBody
                else stop(msg, toString(.Object), "> is protected", call. = FALSE)
            }, list(
                funBody = body(fun),
                msg = paste0("<field \"", field@name, "\" of ")
            ))
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
    attr(.Object, "field and method names") <- unique(c(
        names(x@fields) [vapply(x@fields , attr, "access", FUN.VALUE = "") == "public"],
        names(x@methods)[vapply(x@methods, attr, "access", FUN.VALUE = "") == "public"],
        nms
    ))
    .Object
}
, splice = TRUE)))


setMethod(show, IrisObject@className,
function (object)
{
    cat(toString(object), sep = "\n")
    invisible()
})


assign(paste0("print.", IrisObject@className),
function (x, ...)
{
    cat(toString(x), sep = "\n")
    invisible(x)
})


assign(paste0("names.", IrisObject@className),
function(x) attr(x, "field and method names"))


# other names         ----


field <- IrisFieldDef
method <- IrisMethodDef
property <- IrisPropertyDef
