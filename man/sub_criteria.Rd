% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/sub_criteria.R
\name{sub_criteria}
\alias{sub_criteria}
\alias{attrs}
\alias{eval_sub_criteria}
\alias{print.sub_criteria}
\alias{format.sub_criteria}
\alias{eval_sub_criteria.sub_criteria}
\title{Match criteria}
\usage{
sub_criteria(
  ...,
  match_funcs = c(exact = exact_match),
  equal_funcs = c(exact = exact_match),
  operator = "or"
)

attrs(..., .obj = NULL)

eval_sub_criteria(x, ...)

\method{print}{sub_criteria}(x, ...)

\method{format}{sub_criteria}(x, show_levels = FALSE, ...)

\method{eval_sub_criteria}{sub_criteria}(
  x,
  x_pos = seq_len(max(attr_eval(x))),
  y_pos = rep(1L, length(x_pos)),
  check_duplicates = TRUE,
  depth = 0,
  ...
)
}
\arguments{
\item{...}{\code{[atomic]} Attributes passed to or \code{eval_sub_criteria()} or \code{eval_sub_criteria()}

Arguments passed to methods for \code{eval_sub_criteria()}}

\item{match_funcs}{\code{[function]}. User defined logical test for matches.}

\item{equal_funcs}{\code{[function]}. User defined logical test for identical record sets (all attributes of the same record).}

\item{operator}{\code{[character]}. Options are \code{"and"} or \code{"or"}.}

\item{.obj}{\code{[data.frame|list]}. Attributes.}

\item{x}{\code{[sub_criteria]}. Attributes.}

\item{show_levels}{\code{[logical]}. If \code{TRUE}, show recursive depth for each logic statement of the match criteria.}

\item{x_pos}{\code{[integer]}. Index of one half of a record pair.}

\item{y_pos}{\code{[integer]}. Index of one half of a record pair.}

\item{check_duplicates}{\code{[logical]}. If \code{FALSE}, does not check duplicate values. The result of the initial check will be recycled.}

\item{depth}{\code{[integer]}. First order of recursion.}
}
\value{
\code{\link{sub_criteria}}
}
\description{
Match criteria for record linkage with \bold{\code{\link{links}}} and \bold{\code{\link{episodes}}}
}
\details{
\bold{\code{sub_criteria()}} - Create a match criteria as a \code{sub_criteria} object.
A \code{sub_criteria} object contains attributes to be compared,
logical tests for the comparisons (see \bold{\code{\link{predefined_tests}}} for examples) and
another set of logical tests to determine identical records.

\bold{\code{attrs()}} - Create a \code{d_attribute} object - a collection of atomic objects that can be passed to \bold{\code{sub_criteria()}} as a single attribute.

\bold{\code{eval_sub_criteria()}} - Evaluates a \code{sub_criteria} object.

At each iteration of \code{\link{links}} or \code{\link{episodes}}, record-pairs are created from each attribute of a \code{sub_criteria} object.
\code{eval_sub_criteria()} evaluates each record-pair using the \code{match_funcs} and \code{equal_funcs} functions of a \code{sub_criteria} object.
See \bold{\code{\link{predefined_tests}}} for examples of \code{match_funcs} and \code{equal_funcs}.

User-defined functions are also permitted as \code{match_funcs} and \code{equal_funcs}.
Such functions must meet three requirements:
\enumerate{
\item It must be able to compare the attributes.
\item It must have two arguments named \code{`x`} and \code{`y`}, where \code{`y`} is the value for one observation being compared against all other observations (\code{`x`}).
\item It must return a \code{logical} object i.e. \code{TRUE} or \code{FALSE}.
}

\code{attrs()} is useful when the match criteria requires an interaction between the multiple attributes. For example, attribute 1 + attribute 2 > attribute 3.

Every attribute, including those in \code{attrs()}, must have the same length or a length of 1.
}
\examples{
# Attributes
attr_1 <- c(30, 28, 40, 25, 25, 29, 27)
attr_2 <- c("M", "F", "U", "M", "F", "U", "M")

# A match criteria
## Example 1 - A maximum difference of 10 in attribute 1
s_cri1 <- sub_criteria(attr_1, match_funcs = range_match)
s_cri1

# Evaluate the match criteria
## Compare the first element of 'attr_1' against all other elements
eval_sub_criteria(s_cri1)
## Compare the second element of 'attr_1' against all other elements
x_pos_val <- seq_len(max(attr_eval(s_cri1)))
eval_sub_criteria(s_cri1,
                  x_pos = x_pos_val,
                  y_pos = rep(2, length(x_pos_val)))

## Example 2 - `s_cri1` AND an exact match on attribute 2
s_cri2 <- sub_criteria(
  s_cri1,
  sub_criteria(attr_2, match_funcs = exact_match),
  operator = "and")
s_cri2

## Example 3 - `s_cri1` OR an exact match on attribute 2
s_cri3 <- sub_criteria(
  s_cri1,
  sub_criteria(attr_2, match_funcs = exact_match),
  operator = "or")
s_cri3

# Evaluate the match criteria
eval_sub_criteria(s_cri2)
eval_sub_criteria(s_cri3)

# Alternatively, using `attr()`
AND_func <- function(x, y) range_match(x$a1, y$a1) & x$a2 == y$a2
OR_func <- function(x, y) range_match(x$a1, y$a1) | x$a2 == y$a2

## Create a match criteria
s_cri2b <- sub_criteria(attrs(.obj = list(a1 = attr_1, a2 = attr_2)),
                        match_funcs = AND_func)
s_cri3b <- sub_criteria(attrs(.obj = list(a1 = attr_1, a2 = attr_2)),
                        match_funcs = OR_func)

# Evaluate the match criteria
eval_sub_criteria(s_cri2b)
eval_sub_criteria(s_cri3b)

}
\seealso{
\code{\link{predefined_tests}}; \code{\link{links}}; \code{\link{episodes}}; \code{\link{eval_sub_criteria}}
}
