% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/classes.R
\docType{class}
\name{number_line-class}
\alias{number_line-class}
\alias{show,number_line-method}
\alias{rep,number_line-method}
\alias{[,number_line-method}
\alias{[[,number_line-method}
\alias{[<-,number_line,ANY,ANY,ANY-method}
\alias{[<-,number_line-method}
\alias{[[<-,number_line,ANY,ANY,ANY-method}
\alias{[[<-,number_line-method}
\alias{$,number_line-method}
\alias{$<-,number_line-method}
\alias{c,number_line-method}
\alias{unique.number_line}
\alias{seq.number_line}
\alias{sort.number_line}
\alias{format.number_line}
\alias{as.list.number_line}
\alias{as.data.frame.number_line}
\title{\code{number_line} object}
\usage{
\S4method{show}{number_line}(object)

\S4method{rep}{number_line}(x, ...)

\S4method{[}{number_line}(x, i, j, ..., drop = TRUE)

\S4method{[[}{number_line}(x, i, j, ..., exact = TRUE)

\S4method{[}{number_line,ANY,ANY,ANY}(x, i, j, ...) <- value

\S4method{[[}{number_line,ANY,ANY,ANY}(x, i, j, ...) <- value

\S4method{$}{number_line}(x, name)

\S4method{$}{number_line}(x, name) <- value

\S4method{c}{number_line}(x, ...)

\method{unique}{number_line}(x, ...)

\method{seq}{number_line}(x, precision = NULL, fill = FALSE, ...)

\method{sort}{number_line}(x, decreasing = FALSE, ...)

\method{format}{number_line}(x, ...)

\method{as.list}{number_line}(x, ...)

\method{as.data.frame}{number_line}(x, ...)
}
\arguments{
\item{object}{object}

\item{x}{x}

\item{...}{...}

\item{i}{i}

\item{j}{j}

\item{drop}{drop}

\item{exact}{exact}

\item{value}{value}

\item{name}{slot name}

\item{precision}{Round precision}

\item{fill}{\code{[logical]}. Retain (\code{TRUE}) or
drop (\code{FALSE}) the remainder of an uneven split.}

\item{decreasing}{If \code{TRUE}, sort in descending order.}
}
\description{
S4 objects representing a range of numeric values
}
\section{Slots}{

\describe{
\item{\code{start}}{First value in the range.}

\item{\code{id}}{Unique element id. Optional.}

\item{\code{gid}}{Unique group id. Optional.}

\item{\code{.Data}}{Length, duration or width of the range.}
}}

