\name{member}
\alias{member}
\alias{field}
\alias{method}
\alias{property}
\title{
Create a Member Definition for an Iris Class
}
\description{
Define a member for use within an \code{\link[=setIrisClass]{iris class}}.
}
\usage{
field(access = NA, static = FALSE, final = FALSE, className = "ANY",
    name, value, where = topenv(parent.frame()), ...)

method(access = NA, static = NA, final = FALSE, name, value,
    ...)

property(access = NA, static = NA, final = FALSE, name, value,
    ...)
}
\arguments{
  \item{access}{
\code{"public"}, \code{"private"}, or \code{"protected"}; the accessability of
the field / / method / / property. See \strong{Details}.
  }
  \item{static}{
\code{TRUE}, \code{FALSE}, or \code{"static"}; is this field / / method / /
property a class member or an instance member? See \strong{Details}.
  }
  \item{final}{
\code{TRUE}, \code{FALSE}, \code{"final"}, or \code{"const"}; is this field / /
method / / property final? A final field is a field whose value cannot be
changed once it's been initialized. A final method is a method whose definition
cannot be re-defined in a sub-class.
  }
  \item{className}{
the class of the field. This must be a formal class as defined by
\code{\link[methods]{setClass}}.
  }
  \item{name}{
character string; the name of the field / / method / / property.
  }
  \item{value}{
default value for the field or function definition of the method / / property.
Must be of the same class, or an appropriate sub-class, as \code{className}.
  }
  \item{where}{
environment from which to begin the search for the \code{className} definition.
This is not needed when \code{className} has a
\code{\link[methods]{packageSlot}}.
  }
  \item{\dots}{
further arguments passed to or from other methods.
  }
}
\details{
The various tyoes of \code{access} are \code{"public"}, \code{"private"}, and
\code{"protected"}.\verb{
}Public access means that the member can be accessed from anywhere; within any
method, within any function, within any package.\verb{
}Private access means that the member can only be accessed by methods / /
properties of the corresponding iris class.\verb{
}Protected access means that the member can only be accessed by methods / /
properties of the corresponding iris class and its' sub-classes.

By default, if \code{name} starts with a dot, the member has private access.
Otherwise, it has public access.

Static and non-static members, also called class and instance members, control
whether the member is defined within the class or within each instance of the
class.

By default, fields are non-static. You must explicitly set \code{static = TRUE}
or \code{static = "static"} for the field to be static. Methods / / properties
are non-static if their first argument is \code{.self}, \code{self}, or
\code{this}, and static otherwise.
}
\value{
An iris field, method, or property definition.
}
\section{Properties}{
A property is usually used to avoid writing \emph{getter and setter methods}
for a field. Getting the property calls \code{value} with no arguments, and
assigning calls \code{value} with one argument, the value to be assigned.
}
