## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## -----------------------------------------------------------------------------
library(potions)

brew(x = 1)

paste0("The value of x is ", pour("x"))

drain()

## -----------------------------------------------------------------------------
options("digits") # set to 7 by default
print(pi)

## -----------------------------------------------------------------------------
library(potions)
brew(digits = 3)

print(pi, digits = pour("digits")) # using potions
print(pi) # default is unaffected

## ----eval = FALSE-------------------------------------------------------------
#  # start of script
#  brew(list("my-secret-key" = "123456")) # shares secret information

## ----eval = FALSE-------------------------------------------------------------
#  brew(file = "config.yml") # hides secret information

## ---- eval=FALSE--------------------------------------------------------------
#  .onLoad <- function(libname, pkgname) {
#    if(pkgname == "packagenamehere") {
#      potions::brew(.pkg = "packagenamehere")
#    }
#  }

## ---- eval=FALSE--------------------------------------------------------------
#  .onLoad <- function(libname, pkgname) {
#    if(pkgname == "packagenamehere") {
#      potions::brew(
#        n_attempts == 5,
#        verbose == TRUE,
#        .pkg = "packagenamehere")
#    }
#  }

## ---- eval = FALSE------------------------------------------------------------
#  packagename_config <- function(fontsize = 10){
#    if(!is.numeric(fontsize)){
#      rlang::abort("Argument `fontsize` must be a number")
#    }
#    brew(list(fontsize = fontsize))
#  }

## ---- eval = FALSE------------------------------------------------------------
#  packagename_config <- function(file = NULL){
#    if(!is.null(file)){
#      brew(file = file)
#    }
#  }

## ---- eval = FALSE------------------------------------------------------------
#  packagename_config <- function(file = NULL){
#    if(!is.null(file)){
#      config_data <- potions::read_config(x)
#      # add any checks to `data` that are needed here
#      if(length(names(data)) != length(data)){
#        rlang::abort("Not all entries are named!")
#      }
#      # pass to `brew`
#      brew(config_data)
#    }
#  }

