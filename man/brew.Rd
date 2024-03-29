% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/brew.R
\name{brew}
\alias{brew}
\alias{brew_package}
\alias{brew_interactive}
\title{Set up potions for easy data retrieval}
\usage{
brew(
  ...,
  file,
  .slot,
  .pkg,
  method = c("modify", "merge", "overwrite", "leaves")
)

brew_package(..., file, .pkg, method)

brew_interactive(..., file, .slot, method)
}
\arguments{
\item{...}{One or named arguments giving attributes to be stored; or
alternatively a \code{list} containing the same.}

\item{file}{string: optional file containing data to be stored via \code{options()}.
Valid formats are \code{.yml} or \code{.json}.}

\item{.slot}{string: optional name to mandate where data is stored. Defaults
to a random string generated by \code{stringi::stri_rand_strings()}.}

\item{.pkg}{string: package name that \code{potions} is being used within.
Typically only used during \code{onLoad()}, after which later calls do not
require this argument to be set.}

\item{method}{string: How should new data be written to \code{options()}? See
details for specifics.}
}
\value{
This function never returns an object; it is called for its' side-
effect of caching data using \code{options()}.
}
\description{
Function to place a list into \code{options()}, or to update previously-stored
data.
}
\details{
The default method is to use \code{brew} without setting either \code{.pkg} or \code{.slot}
(but not both), and letting \code{potions} determine which slot to use. If greater
control is needed, you can use \code{brew_package()} or \code{brew_interactive()}.
Note that if neither \code{.slot} or \code{.pkg} are set, \code{potions} defaults to \code{.slot}
, unless \code{.pkg} information has previously been supplied (and \code{.slot}
information has not). This might be undesirable in a package development
situation.

If both \code{...} and \code{file} arguments are empty, this function sets up an
empty \code{potions} object in \code{options("potions-pkg")}; See \code{potions-class} for
more information on this data type. If \code{...} and \code{file} arguments
are provided, they will be amalgamated using \code{purrr::list_modify()}. If there
are identical names in both lists, those in \code{...} are chosen.

If the user repeatedly calls \code{brew()}, later list entries overwrite early
entries. Whole lists are not overwritten unless all top-level entry names
match, or \code{method} is set to \code{"overwrite"}, which is a shortcut to using
\code{drain()} before \code{brew()}. The default behaviour is \code{method = "modify"},
which uses \code{purrr::list_modify()} to do the joining. Similarly \code{"merge"} uses
\code{purrr::list_merge()}. \code{method = "leaves"} only overwrites terminal nodes,
leaving the structure of the list otherwise unaffected. For non-nested lists,
this behaviour is identical to \code{"modify"}, but for nested lists it can be a
useful shortcut.
}
\examples{
# basic usage is to pass arguments using `=`
brew(x = 1)

# lists are also permitted
list(x = 2) |> brew()

# as are passing lists as objects
my_list <- list(x = 3)
my_list |> brew()

# or within a function
my_fun <- function(){list(x = 1, y = 2)}
my_fun() |> brew()

# optional clean-up
drain()
}
