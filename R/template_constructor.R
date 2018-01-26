
#' eml_creator <- template_constructor("creator")
template_constructor <- function(object){
  args <- emld::template(object)
  f <- paste0("function(",
         paste(names(args), "=", args, collapse = ", "),
         "){ list(",
         paste(names(args), "=", names(args), collapse = ","),
         ")}")
  eval(parse(text = f))
}


#who <- readLines(system.file("extdata/complexTypes.txt", package="eml2"))
#names(who) <- who
#construct <- lapply(who, template_constructor)

#' construct
#'
#' @docType data
#' @keywords datasets
#' @name construct
#' @usage construct$creator
#' @format A list with constructor functions
NULL
