#' @describeIn render_ render_
#' @inheritDotParams rmarkdown::render
#' @returns Path to rendered CV.
#' 
#' @export
#' @importFrom rmarkdown render
#' @examples
#' \dontrun{
#' file <- get_template()
#' file_html <- render_cv(file = file)
#' }
render_cv <- function(file,
                      as_pdf=FALSE,
                      ...){
  if(!file.exists(file)){
    stopper("File not found:",file)
  }
  if(as_pdf){
    txt <- readLines(file)
    txt <- gsub("# knit: pagedown::chrome_print",
                "knit: pagedown::chrome_print",txt)
    writeLines(txt,file)
  }
  rmarkdown::render(file,...)
}
