
#' read_eml
#'
#' read_eml
#' @param x path to an EML file
#' @return an emld object (list / S3 object)
#' @export
#' @examples
#' f <- system.file("extdata", "example.xml", package = "emld")
#' eml <- read_eml(f)
#' @export
#' @importFrom emld as_emld
read_eml <- function(x){
  emld::as_emld(x)
}


