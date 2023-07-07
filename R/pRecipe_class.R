#' An S4 class that extends data.table for pRecipe data
#'
#' @importFrom methods new
#' @slot info character with data set name and type
#' @keywords internal
pRecipe <- setClass("pRecipe", slots = c(id = "character"),
                    contains = "data.table")