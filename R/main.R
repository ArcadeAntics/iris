

# this package was originally made using active bindings
#
# unfortunately, I found out later that R does not preserve active bindings
# between sessions, meaning tons of complications for defining iris classes
# within packages and when saving an iris object then reloading it
#
# I didn't want to throw out my work though, so I introduced this flag that
# will compile the code differently depending on its value
R.preserves.active.bindings <- FALSE


options(keep.source = FALSE)


this.namespace <- environment()


as.className <- function (Class, where = topenv(parent.frame()))
paste0(getPackageName(where), "::", as.symbol(Class))


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
    Class = as.className("field"),
    slots = list(
        access    = "character",
        static    = "logical"  ,
        final     = "logical"  ,
        className = "character",
        name      = "character",
        value     = `packageSlot<-`("ANY", "methods"),
        hasValue  = "logical"
    ),
    sealed = FALSE  # in setClass
)
IrisFieldDef@.Data <- eval(bquote2(
function (access = NA, static = FALSE, final = FALSE, Class = .(`packageSlot<-`("ANY", "methods")),
    name, value, where = topenv(parent.frame()), ...)
{
    if (missing(value))
        new(Class = .(IrisFieldDef@className), access = access, static = static,
            final = final, className = Class, name = name, value = ,
            where = where, ...)
    else {
        new(Class = .(IrisFieldDef@className), access = access, static = static,
            final = final, className = Class, name = name, value = value,
            where = where, ...)
    }
}
))


setMethod(initialize, IrisFieldDef@className, eval(bquote(
function (.Object, access = NA, static = FALSE, final = FALSE,
    className = .(`packageSlot<-`("ANY", "methods")), name, value,
    where = topenv(parent.frame(3)), ...)
{
    .Object <- callNextMethod(.Object = .Object, ...)


    name <- as.character(as.symbol(name))
    if (name == "super")
        stop("invalid 'name'; \"super\" is a reserved name")


    access <- as.character(access)[[1L]]
    if (is.na(access))
        access <- if (startsWith(name, ".")) "private" else "public"
    else access <- match.arg(access, c("public", "private", "protected"))


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


    # if (hasValue <- !missing(value)) {
    if (hasValue <- !identical(environment()$value, quote(expr = ))) {


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
    Class = as.className("method"),
    slots = list(
        access = "character",
        static = "logical"  ,
        final  = "logical"  ,
        name   = "character",
        value  = "function"
    ),
    sealed = FALSE  # in setClass
)
IrisMethodDef@.Data <- eval(bquote2(
function (access = NA, static = NA, final = FALSE, name, value,
    ...)
new(Class = .(IrisMethodDef@className), access = access, static = static,
    final = final, name = name, value = value, ...)
))


setMethod(initialize, IrisMethodDef@className,
function (.Object, access = NA, static = NA, final = FALSE, name, value, ...)
{
    .Object <- callNextMethod(.Object = .Object, ...)


    name <- as.character(as.symbol(name))
    if (name == "super")
        stop("invalid 'name'; \"super\" is a reserved name")


    if (!is.function(value))
        stop("invalid 'value'; must be a function")


    access <- as.character(access)[[1L]]
    if (is.na(access))
        access <- if (startsWith(name, ".")) "private" else "public"
    else access <- match.arg(access, c("public", "private", "protected"))


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


    sfinal <- as.character(final)[[1L]]
    final <- if (!is.na(sfinal) && sfinal %in% c("final", "const"))
        TRUE
    else if (final)
        TRUE
    else FALSE


    .Object@access <- access
    .Object@static <- static
    .Object@final  <- final
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
    Class = as.className("property"),
    contains = IrisMethodDef@className,
    sealed = FALSE  # in setClass
)
IrisPropertyDef@.Data <- eval(bquote2(
function (access = NA, static = NA, final = FALSE, name, value,
    ...)
new(Class = .(IrisPropertyDef@className), access = access, static = static,
    final = final, name = name, value = value, ...)
))


# MemberDef           ----


IrisMemberDef <- setClassUnion(
    name = as.className("member"),
    members = c(
        IrisFieldDef   @className,
        IrisMethodDef  @className,
        IrisPropertyDef@className
    )
)


# method              ----


IrisMethod <- setClass(
    Class = as.className("BoundMethod"),
    contains = "function",
    slots = list(
        access = "character",
        static = "logical"  ,
        name   = "character",


        is.active   = "logical",
        is.property = "logical",
        boundTo     = `packageSlot<-`("ANY", "methods")
    ),
    sealed = FALSE  # in setClass
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
    Class = as.className("ClassRepresentation"),
    contains = `packageSlot<-`("classRepresentation", "methods"),
    slots = list(
        extends = "character",
        fields  = "list",
        methods = "list"
    )
)
body(IrisClassRepresentation@.Data) <- bquote(new(Class = .(IrisClassRepresentation@className), ...))


# class               ----


IrisClass.className <- as.className("class")
.IrisObject.className <- "object"
IrisObject.className <- as.className(.IrisObject.className)


.setClassPortion <- function (baseCase)
{
    eval(bquote2(
function (Class, ..., contains = character(), where = topenv(parent.frame()),
    validity = NULL, sealed = FALSE)
{
    Class <- as.className(Class, where)


    members <- c(list(...), recursive = TRUE, use.names = FALSE)
    members <- lapply(members, function(xx) {
        if (!is(xx, .(IrisMemberDef@className)))
            stop("invalid 'members', must be a set of fields, methods, and properties")
        xx
    })
    nms <- vapply(members, function(xx) xx@name, "")
    if (anyDuplicated(nms))
        stop("invalid 'members', duplicated names")
    names(members) <- nms


..(
if (baseCase) quote({
    contains <- "environment"
    extends <- c(Class, "environment")
}) else bquote({
    if (length(contains) <= 0)
        contains <- .(attr(IrisObject, "className"))
    if (is(contains, .(IrisClass.className)))
        contains <- attr(contains, "className")
    def <- getClass(contains, where = where)
    if (!is(def, .(IrisClassRepresentation@className)))
        stop("invalid 'contains', must be a subclass of ", dQuote(.(attr(IrisObject, "className"))))
    contains <- def@className
    extends <- c(Class, def@extends)
})
)


    is.method <- vapply(members, is, .(IrisMethodDef@className), FUN.VALUE = NA)
    fields <- members[!is.method]
    methods <- members[is.method]


    rep <- makeClassRepresentation(name = Class, superClasses = contains,
        package = getPackageName(where = where), validity = validity,
        sealed = sealed, where = where)
    rep <- IrisClassRepresentation(rep, extends = extends, fields = fields,
        methods = methods)
    setClass(Class = rep@className, representation = rep, where = where)
}
    ), parent.frame())
}


setClassPortion <- .setClassPortion(TRUE)


who.called <- eval(bquote2(
function ()
{
..(
if (R.preserves.active.bindings) quote({
    if ((n <- sys.parents()[[sys.nframe()]] - 1L) > 0L)
        get("envir", envir = sys.frame(n), mode = "environment", inherits = FALSE)
    else stop("'who.called' used in an inappropriate fashion")
})
else quote({
    if ((n <- sys.parents()[[sys.nframe()]] - 2L) > 0L)
        get("x", envir = sys.frame(n), mode = "environment", inherits = FALSE)
    else stop("'who.called' used in an inappropriate fashion")
})
)
}
))


# who.called <- function ()
# {
#     # N <- sys.nframe()
#     # nseq <- N - seq_len(N - 1L)
#     # calls <- sys.calls()
#     # frames <- sys.frames()
#     # functions <- lapply(nseq, sys.function)
#     # parents <- sys.parents()
#     # print(parents)
#     # cat("\n\n")
#     # l <- lapply(nseq, function(n) {
#     #     list(
#     #         call = calls[[n]],
#     #         frame = frames[[n]],
#     #         names = names(frames[[n]]),
#     #         `function` = functions[[n]]
#     #     )
#     # })
#     # names(l) <- nseq - N
#     # print(l)
#     # for (n in nseq) {
#     #     print()
#     #     # print(calls[[n]])
#     #     # print(frames[[n]])
#     #     # print(names(frames[[n]]))
#     #     # print(functions[[n]])
#     #     cat("\n\n")
#     # }
#     # print(lapply(parents - 1L, function(i) if (i > 0) calls[[i]]))
#     # cat("\n\n")
#     # print(lapply(parents - 1L, function(i) if (i > 0) functions[[i]]))
#     # cat("\n\n")
#     # for (n in sys.parents()[[sys.nframe()]] - seq_len(N - 1L)) {
#     #     # print(calls[[n]])
#     #     # print(frames[[n]])
#     #     print(names(frames[[n]]))
#     #     # print(functions[[n]])
#     #     cat("\n\n")
#     # }
#     # for (n in nseq) {
#     #     cat(sprintf("sys.function(sys.parents()[[sys.nframe() - %d]]) = ", n))
#     #     fun <- sys.function(parents[[N - n]])
#     #     print(fun)
#     #     cat("\n\n\n\n\n")
#     # }
#
#     # print(names(parent.frame(1)))
#     # print(names(parent.frame(2)))
#     # print(names(parent.frame(3)))
#
# }


IrisObject <- setClassPortion(
    Class = .IrisObject.className,
    IrisMethodDef  (                   name = "new"           , value = as.activeBindingFunction(eval(bquote2(function() {
        envir <- who.called()
        if (is(envir, .(IrisClass.className)))
            Class <- attr(envir, "className")
        else Class <- class(envir)
        function(...) {
            .Object <- new(Class)
            if (exists("initialize", envir = .Object, inherits = FALSE)) {
                initialize <- .getWithoutCheck("initialize", .Object)
                if (is(initialize, .(IrisMethod@className)) && !initialize@static)
                    initialize(...)

                stop()
                .Object <- obj2
methods <- getClass(class(.Object))@methods
if ("initialize" %in% names(methods) && !methods[["initialize"]]@static) {
    initialize <- environment(get("initialize", envir = .Object, inherits = FALSE))$VALUE
    if (initialize@is.active || initialize@is.property)
        initialize <- initialize()
}
initialize
            }
            tmp <- .Object
            while (!identical(tmp, emptyenv())) {
                if (exists("finalize", envir = tmp, inherits = FALSE)) {
                    finalize <- .getWithoutCheck("finalize", tmp)
                    if (is(finalize, .(IrisMethod@className)) && !finalize@static)
                        reg.finalizer(tmp, finalize, onexit = TRUE)
                }
                tmp <- parent.env(tmp)
            }
            if (exists("validate", envir = .Object, inherits = FALSE)) {
                validate <- .getWithoutCheck("validate", .Object)
                if (is(validate, .(IrisMethod@className)) && !validate@static)
                    validate()
            }
            .Object
        }
    })))),
    IrisPropertyDef(access = "public", name = ".classDef"     , value = function(.self) getClass(class(who.called()))        ),
    IrisPropertyDef(access = "public", name = ".irisClassDef" , value = function(.self) getIrisClass(who.called())           ),
    IrisPropertyDef(access = "public", name = ".objectPackage", value = function(.self) getClass(class(who.called()))@package),


    IrisMethodDef  (                   name = "copy"          , value = as.activeBindingFunction(function(.self) {
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
        envir <- who.called()
        function(shallow = FALSE) copy(envir, shallow)
    })),
    IrisMethodDef  (                   name = "field"         , value = as.activeBindingFunction(eval(bquote2(function(.self) {
        envir <- who.called()
        function(name, value) {
            if (missing(value))
                .(as.symbol(paste0("$.", IrisObject.className)))(envir, name)
            else .(as.symbol(paste0("$<-.", IrisObject.className)))(envir, name, value)
        }
    })))),
    IrisMethodDef  (                   name = "getClass"      , value = as.activeBindingFunction(function(.self) {
        envir <- who.called()
        function(...) if (nargs()) getClass(...) else getClass(class(envir))
    })),
    IrisMethodDef  (                   name = "getIrisClass"  , value = as.activeBindingFunction(function(.self) {
        envir <- who.called()
        function(Class = class(envir)[[1L]]) getIrisClass(Class)
    })),
    IrisMethodDef  (                   name = "show"          , value = as.activeBindingFunction(eval(bquote2(function(.self) {
        envir <- who.called()
        function() .show(envir)
    })))),
    sealed = FALSE  # in setClass
)


IrisClass <- setClass(
    Class = IrisClass.className,
    contains = attr(IrisObject, "className"),
    slots = list(
        className = "character",
        package   = "character"
    ),
    sealed = FALSE  # in setClass
)
body(IrisClass@.Data) <- bquote(new(Class = .(IrisClass@className), ...))


.irisClassTable <- new.env(parent = emptyenv())


fix.class <- function (x)
{
    parent.env(x) <- .getIrisClass(getClass(attr(x, "className"))@extends[[2L]])
    assign(attr(x, "className"), x, .irisClassTable)
    invisible(x)
}


giveBoundMethod <- eval(bquote2(
function (.Object, method)
{
    method <- IrisMethod(method = method, boundTo = .Object)
..(
if (R.preserves.active.bindings) bquote2({
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
else bquote2({
    env <- list2env(list(.Object = .Object, VALUE = method), parent = this.namespace)
    lockEnvironment(env, bindings = TRUE)
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
            else stop(toString(VALUE), " is private", call. = FALSE)
        }, list(funBody = body(fun)))
    },
    protected = {
        body(fun) <- substitute({
            if (protected.check(.Object))
                funBody
            else stop(toString(VALUE), " is protected", call. = FALSE)
        }, list(funBody = body(fun)))
    },
    public = {
    },
    {
        stop("invalid method@access; should never happen, please report!")
    })
    environment(fun) <- env
    assign(method@name, fun, envir = .Object)
    lockBinding(method@name, .Object)
})
)
}
))


giveField <- eval(bquote2(
function (.Object, field)
{
    VALUE <- field@value
    env <- list2env(list(.Object = .Object, VALUE = VALUE, access = field@access), parent = this.namespace)
    lockBinding(".Object", env)
    lockBinding("access", env)
..(
if (R.preserves.active.bindings) bquote2({
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
})
else bquote2({


    if (field@final && field@hasValue) {
        do_assign <- function(value) NULL
        body(do_assign) <- substitute({
            stop(cannot.change.value.msg, call. = FALSE)
        }, list(
            cannot.change.value.msg = paste0("cannot change value of final field '", field@name, "'")
        ))
        lockBinding("VALUE", env)
    } else {
        if (identical(field@className, .(`packageSlot<-`("ANY", "methods")))) {
            do_assign <- function(value) VALUE <<- value
        } else {
            do_assign <- function(value) NULL
            body(do_assign) <- substitute({
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
    environment(do_assign) <- env


    if (field@final && !field@hasValue) {


        do_get <- function() NULL
        body(do_get) <- substitute({
            stop(msg, toString(.Object), "> is not initialized", call. = FALSE)
        }, list(
            msg = paste0("<field \"", field@name, "\" of ")
        ))


        new_do_assign <- function(value) NULL
        body(new_do_assign) <- substitute({
            stop(msg, call. = FALSE)
        }, list(
            msg = paste0("cannot change value of final field '", field@name, "'")
        ))
        environment(new_do_assign) <- env


        new_do_get <- function() VALUE
        environment(new_do_get) <- env


        body(do_assign) <- substitute({
            body.do_assign
            do_assign <<- new_do_assign
            do_get <<- new_do_get
            lockEnvironment(parent.env(environment()), bindings = TRUE)
            invisible(VALUE)
        }, list(
            body.do_assign = body(do_assign),
            new_do_assign  = new_do_assign,
            new_do_get     = new_do_get
        ))


    } else {
        do_get <- function() VALUE
    }
    environment(do_get) <- env
    env$do_assign <- do_assign
    env$do_get <- do_get


    fun <- function(value) {
        if (missing(value))
            do_get()
        else do_assign(value)
    }
    switch (field@access,
    private = {
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
    lockEnvironment(env)
    environment(fun) <- env
    assign(field@name, fun, envir = .Object)
    lockBinding(field@name, .Object)
})
)
}
))


.setIrisClassPortion <- function (baseCase)
{
    eval(bquote2(
function (Class, where = topenv(parent.frame()))
{
    def <- if (is.null(packageSlot(Class))) {
        getClass(Class, where = where)
    } else getClass(Class)
    if (!is(def, .(IrisClassRepresentation@className)))
        stop("invalid 'Class', must be a subclass of ", dQuote(.(attr(IrisObject, "className"))))


..(
if (baseCase) quote({
    .Object <- new.env(parent = emptyenv())
    # super <- function(value) stop("there is no superclass of ", toString(.Object), call. = FALSE)
    # environment(super) <- list2env(list(.Object = .Object), parent = this.namespace)
    # lockEnvironment(environment(super), bindings = TRUE)
    # assign("super", super, envir = .Object)
    # lockBinding("super", .Object)
})
else quote({
    .Object <- new.env(parent = .getIrisClass(def@extends[[2L]]))
    # super <- function(value) {
    #     if (missing(value))
    #         parent.env(.Object)
    #     else if (!identical(parent.env(.Object), value))
    #         stop("cannot change value of locked binding for 'super'")
    #     else invisible(parent.env(.Object))
    # }
    # environment(super) <- list2env(list(.Object = .Object), parent = this.namespace)
    # lockEnvironment(environment(super), bindings = TRUE)
    # assign("super", super, envir = .Object)
    # lockBinding("super", .Object)
})
)


    class(.Object) <- c(.(IrisClass@className), .(attr(IrisObject, "className")), "environment")
    attr(.Object, "className") <- def@className
    attr(.Object, "package") <- def@package


    for (method in def@methods) {
        if (!method@static)
            next
        giveBoundMethod(.Object, method)
    }


    for (field in def@fields) {
        if (!field@static)
            next
        giveField(.Object, field)
    }
    # selfEnv$new <- irisClassGeneratorFunction(className)
    # lockEnvironment(selfEnv, bindings = TRUE)


    assign(def@className, .Object, .irisClassTable)


..(
if (!baseCase) quote({
    action <- function(ns) fix.class(.Object)
    environment(action) <- list2env(list(.Object = .Object), parent = this.namespace)
    lockEnvironment(environment(action), bindings = TRUE)
    setLoadAction(action, paste0(".", def@className, ".fixup"), where = where)
})
)


    .Object
}
    ), parent.frame())
}


setIrisClassPortion <- .setIrisClassPortion(TRUE)


IrisObject <- setIrisClassPortion(IrisObject@className)


setClassPortion <- .setClassPortion(FALSE)
setIrisClassPortion <- .setIrisClassPortion(FALSE)


setMethod(initialize, IrisClass@className, eval(bquote2(
function (.Object, className, ..., contains = character(),
    where, validity = NULL, sealed = FALSE)
{
    where
    fun <- setClassPortion(Class = className, ..., contains = contains,
        where = where, validity = validity, sealed = sealed)
    setIrisClassPortion(Class = fun@className, where = where)
}
)))


setMethod(show, IrisClass@className,
function (object)
{
    # object <- get(attr(IrisObject, "className"), iris:::.irisClassTable, inherits = FALSE)


    cat("<iris class ", encodeString(attr(object, "className"), quote = "\""), ">\n", sep = "")
    # fields <- object@fields
    # if (length(fields) > 0L) {
    #
    # } else cat("No fields defined\n\n")
    # methods <- object@methods
    # if (length(methods)) {
    #     .printNames("Methods", names(methods))
    # } else cat("No class methods\n\n")


}
)


assign(paste0("toString.", IrisClass@className), function (x, ...)
paste0("<iris class \"", attr(x, "className"), "\">"))


setIrisClass <- eval(bquote2(
function (Class, ..., contains = character(), where = topenv(parent.frame()),
    validity = NULL, sealed = FALSE)
new(Class = .(IrisClass@className), className = Class, ..., contains = contains,
    where = where, validity = validity, sealed = sealed)
))


# object              ----


assign(paste0("$.", attr(IrisObject, "className")), eval(bquote2(
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


assign(paste0("$<-.", attr(IrisObject, "className")), eval(bquote2(
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


assign(paste0("[[.", attr(IrisObject, "className")), eval(bquote2(
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


assign(paste0("[[<-.", attr(IrisObject, "className")), eval(bquote2(
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
    IrisObject.className = attr(IrisObject, "className"),
    envRefClass.className = `packageSlot<-`("envRefClass", "methods")
))


.getIrisClass <- function (Class)
get(Class[[1L]], envir = .irisClassTable, mode = "environment", inherits = FALSE)


getIrisClass <- eval(bquote(
function (Class, where = topenv(parent.frame()))
{
    if (is(Class, .(IrisClassRepresentation@className))) {
        classDef <- Class
        Class <- Class@className
    } else if (is.character(Class)) {
        classDef <- getClass(Class, where = where)
        Class <- classDef@className
        if (!is(classDef, .(IrisClassRepresentation@className)))
            stop(gettextf("class %s is defined but is not an iris class",
                dQuote(Class)))
    } else if (is(Class, .(attr(IrisObject, "className")))) {
        Class <- class(Class)[[1L]]
        classDef <- getClass(Class, where = where)
        if (!is(classDef, .(IrisClassRepresentation@className)))
            stop(gettextf("class %s is defined but is not an iris class",
                dQuote(Class)))
    } else stop(gettextf("class must be an iris class representation, a character string, or an iris class; got an object of class %s",
        dQuote(class(Class))))
    .getIrisClass(Class)
}
))


.show <- eval(bquote(
function (envir)
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
        is.property <- vapply(methods, is, .(IrisPropertyDef@className), FUN.VALUE = NA)
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
        # if (identical(def@className, attr(IrisObject, "className"))) {
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
))


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


private.check <- eval(bquote(
function (x, n = .(if (R.preserves.active.bindings) 4L else 3L))
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
    #     tmp <- sys.function(print(sys.parents()[[sys.nframe() - N]]))
    #     print(tmp)
    # }
    (n <- sys.nframe() - n) > 0L &&
        is(sys.function(n <- sys.parents()[[n]]), .(IrisMethod@className)) &&
        identical(sys.function(n)@boundTo, x)
    # sys.nframe() >= n &&
    #     is(sys.function(-n), .) &&
    #     identical(sys.function(-n)@boundTo, x)
}
))


protected.check <- eval(bquote(
function (x, n = .(if (R.preserves.active.bindings) 4L else 3L))
{
    (n <- sys.nframe() - n) > 0L &&
        is(sys.function(n <- sys.parents()[[n]]), .(IrisMethod@className)) &&
        {
            env <- sys.function(n)@boundTo
            valid <- FALSE
            while (!identical(env, emptyenv())) {
                if (valid <- identical(env, x))
                    break
                env <- parent.env(env)
            }
            valid
        }
}
))


assign(paste0("toString.", attr(IrisObject, "className")),
function (x, ...)
paste0("<", encodeString(class(x)[[1L]], quote = "\""), " object at ", sub("^<environment: (.*)>$", "\\1", format.default(x)), ">"))


setMethod(initialize, attr(IrisObject, "className"), eval(bquote2(
function (.Object, ...)
{
    def <- getClass(class(.Object))
    irisDef <- getIrisClass(class(.Object))
    if (identical(class(.Object), .(attr(IrisObject, "className")))) {
        .Object <- new.env(parent = emptyenv())
        class(.Object) <- def@extends
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
    } else {
        VALUE <- new(def@extends[[2L]])
        .Object <- new.env(parent = VALUE)
        class(.Object) <- def@extends
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
    }


    for (method in def@methods) {


        if (method@static) {
            fun <- function(value) NULL
            body(fun) <- substitute({
                x <- .getIrisClass(Class)
                if (missing(value))
                    x$name
                else x$name <- value
            }, list(
                Class = class(.Object)[[1L]],
                name = as.symbol(method@name)
            ))
            environment(fun) <- this.namespace
            assign(method@name, fun, envir = .Object)
            lockBinding(method@name, .Object)
            next
        }


        giveBoundMethod(.Object, method)
    }


    for (field in def@fields) {


        if (field@static) {
            fun <- function(value) NULL
            body(fun) <- substitute({
                irisClass <- .getIrisClass(Class)
                envir <- environment(get(name, envir = irisClass, inherits = FALSE))
                switch(envir$access,
                private = {
                    if (!private.static.check(Class)) {
                        stop(msg, toString(irisClass), "> is private", call. = FALSE)
                    }
                },
                protected = {
                    if (!protected.static.check(Class)) {
                        stop(msg, toString(irisClass), "> is protected", call. = FALSE)
                    }
                },
                public = {
                },
                {
                    stop("invalid 'access'; should never happen, please report!")
                })
                if (missing(value))
                    envir$do_get()
                else envir$do_assign(value)
            }, list(
                Class = class(.Object)[[1L]],
                name = field@name,
                msg = paste0("<field \"", field@name, "\" of "),
                funBody = body(fun)
            ))
            environment(fun) <- this.namespace
            assign(field@name, fun, envir = .Object)
            lockBinding(field@name, .Object)
            next
        }


        giveField(.Object, field)
    }
    lockEnvironment(.Object)
    .Object
}
)))


setMethod(show, attr(IrisObject, "className"),
function (object)
{
    cat(toString(object), sep = "\n")
    invisible()
})


assign(paste0("print.", attr(IrisObject, "className")),
function (x, ...)
{
    cat(toString(x), sep = "\n")
    invisible(x)
})


# other names         ----


field <- IrisFieldDef
method <- IrisMethodDef
property <- IrisPropertyDef


rm(R.preserves.active.bindings,  IrisFieldDef, IrisMethodDef, IrisPropertyDef, IrisMemberDef,
    .setClassPortion, .setIrisClassPortion, IrisClass.className, .IrisObject.className, IrisObject.className)
