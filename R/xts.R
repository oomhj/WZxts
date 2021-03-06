library(R6)
Person <- R6Class("Person",
                  public = list(
                    name = NA,
                    hair = NA,
                    initialize = function(name, hair) {
                      if (!missing(name)) self$name <- name
                      if (!missing(hair)) self$hair <- hair
                      self$greet()
                    },
                    set_hair = function(val) {
                      self$hair <- val
                    },
                    greet = function() {
                      cat(paste0("Hello, my name is ", self$name, ".\n"))
                    }
                  )
)
parent.call <- function(e) {
  print(e)
  print("====")
  if (is.environment(e) & !identical(emptyenv(), e)) {
    parent.call(parent.env(e))
  }
}
parent.call(environment())

ann <- Person$new("Ann", "black")
#> Hello, my name is Ann.
ann
