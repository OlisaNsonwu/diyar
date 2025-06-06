% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/number_line.R
\name{number_line}
\alias{number_line}
\alias{as.number_line}
\alias{is.number_line}
\alias{left_point}
\alias{left_point<-}
\alias{right_point}
\alias{right_point<-}
\alias{start_point}
\alias{start_point<-}
\alias{end_point}
\alias{end_point<-}
\alias{number_line_width}
\alias{reverse_number_line}
\alias{shift_number_line}
\alias{expand_number_line}
\alias{invert_number_line}
\alias{number_line_sequence}
\title{\code{number_line}}
\usage{
number_line(l, r, id = NULL, gid = NULL)

as.number_line(x)

is.number_line(x)

left_point(x)

left_point(x) <- value

right_point(x)

right_point(x) <- value

start_point(x)

start_point(x) <- value

end_point(x)

end_point(x) <- value

number_line_width(x)

reverse_number_line(x, direction = "both")

shift_number_line(x, by = 1)

expand_number_line(x, by = 1, point = "both")

invert_number_line(x, point = "both")

number_line_sequence(
  x,
  by = NULL,
  length.out = 1,
  fill = TRUE,
  simplify = FALSE
)
}
\arguments{
\item{l}{\code{[numeric-based]}. Left point of the \code{number_line}.}

\item{r}{\code{[numeric-based]}. Right point of the \code{number_line}. Must be able to be coerced to a \code{numeric} object.}

\item{id}{\code{[integer]}. Unique element identifier. Optional.}

\item{gid}{\code{[integer]}. Unique group identifier. Optional.}

\item{x}{\code{[number_line]}}

\item{value}{{[\code{numeric} based]}}

\item{direction}{\code{[character]}. Type of \code{number_line} reverse.
Options are; \code{"increasing"}, \code{"decreasing"} or \code{"both"} (default).}

\item{by}{\code{[integer]}. Increment or decrement. Passed to \code{seq()} in \code{number_line_sequence()}.}

\item{point}{\code{[character]}. \code{"start"}, \code{"end"}, \code{"left"} or \code{"right"} point.}

\item{length.out}{\code{[integer]}. Number of splits. For example, \code{1} for two parts or \code{2} for three parts. Passed to \code{seq()}.}

\item{fill}{\code{[logical]}. Retain (\code{TRUE}) or drop (\code{FALSE}) the remainder of an uneven split.}

\item{simplify}{\code{[logical]}. If \code{TRUE}, returns a sequence of finite numbers.}
}
\value{
\code{number_line}
}
\description{
A range of \code{numeric} values.
}
\details{
A \code{number_line} object represents a range of numbers.
It is made up of a \code{start} and \code{end} point as the lower and upper ends of the range respectively.
The location of the \code{start} point - \code{left} or \code{right},
determines whether it is an \code{"increasing"} or \code{"decreasing"} \code{number_line}.
This is the \code{direction} of the \code{number_line}.

\bold{\code{reverse_number_line()}} - reverse the direction of a \code{number_line}.
A reversed \code{number_line} has its \code{left} and \code{right} points swapped.
The \code{direction} argument specifies which type of \code{number_line} will be reversed.
\code{number_line} with non-finite \code{start} or \code{end} points (i.e. \code{NA}, \code{NaN} and \code{Inf}) can't be reversed.

\bold{\code{shift_number_line()}} - Shift a \code{number_line} towards the positive or negative end of the number line.

\bold{\code{expand_number_line()}} - Increase or decrease the width of a \code{number_line}.

\bold{\code{invert_number_line()}} - Change the \code{left} or \code{right} points from a negative to positive value or vice versa.

\bold{\code{number_line_sequence()}} - Split a \code{number_line} into equal parts (\code{length.out}) or by a fixed recurring width (\code{by}).
}
\examples{
number_line(-100, 100)

# Also compatible with other numeric based object classes
number_line(as.POSIXct("2019-05-15 13:15:07", tz = "UTC"),
            as.POSIXct("2019-05-15 15:17:10", tz = "UTC"))

# Coerce compatible object classes to `number_line` objects
as.number_line(5.1); as.number_line(as.Date("2019-10-21"))

# A test for number_line objects
a <- number_line(as.Date("2019-04-25"), as.Date("2019-01-01"))
is.number_line(a)

# Structure of a number_line object
left_point(a); right_point(a); start_point(a); end_point(a)

# Reverse number_line objects
reverse_number_line(number_line(as.Date("2019-04-25"), as.Date("2019-01-01")))
reverse_number_line(number_line(200, -100), "increasing")
reverse_number_line(number_line(200, -100), "decreasing")

c <- number_line(5, 6)
# Shift number_line objects towards the positive end of the number line
shift_number_line(x = c(c, c), by = c(2, 3))
# Shift number_line objects towards the negative end of the number line
shift_number_line(x = c(c, c), by = c(-2, -3))

# Change the duration, width or length of a number_line object
d <- c(number_line(3, 6), number_line(6, 3))

expand_number_line(d, 2)
expand_number_line(d, -2)
expand_number_line(d, c(2,-1))
expand_number_line(d, 2, "start")
expand_number_line(d, 2, "end")

# Invert `number_line` objects
e <- c(number_line(3, 6), number_line(-3, -6), number_line(-3, 6))
e
invert_number_line(e)
invert_number_line(e, "start")
invert_number_line(e, "end")

# Split number line objects
x <- number_line(Sys.Date() - 5, Sys.Date())
x
number_line_sequence(x, by = 2)
number_line_sequence(x, by = 4)
number_line_sequence(x, by = 4, fill = FALSE)
number_line_sequence(x, length.out = 2)
}
\seealso{
\code{\link{overlaps}}; \code{\link{set_operations}}; \code{\link{episodes}}; \code{\link{links}}
}
