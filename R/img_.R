#' @describeIn img_ img_
#' @export
img_get <- function(name,
                    dir=file.path("img"),
                    width=NULL,
                    height=NULL,
                    as_html=TRUE,
                    style=NULL,
                    collapse=" ",
                    default="",
                    verbose=FALSE){
  
  #### Create img dict ####
  files <- list.files(dir, full.names = TRUE)
  img_dict <- stats::setNames(files,nm = basename(files))
  img_dict <- c(img_dict,
                stats::setNames(
                  files,
                  gsub("\\.*","",basename(files)))
  )
  #### Iterate over names ####
  name <- trimws(strsplit(name,",")[[1]])
  sapply(name, function(nm){
    #### Get file path ####
    if(nm %in% names(img_dict)){
      f <- img_dict[nm]
    } else {
      f <- file.path(dir,nm)
    }
    #### Check file exists ####
    if(file.exists(f)){
      if(isTRUE(as_html)){
        return(paste0("<img src=",shQuote(f),
                      if(!is.null(width))paste0("width=",shQuote(width)),
                      if(!is.null(height))paste0("height=",shQuote(height)),
                      if(!is.null(style))paste0("style=",shQuote(style)),
                      " alt=",shQuote(nm),
                      ">"))
      } else {
        return(f)
      }
    } else {
      if(verbose){
        warning("File does not exist: ",shQuote(f))      
      }
      return("")
    }
  }) |> paste(collapse = collapse)
}

#' @describeIn img_ img_
#' @export
img_link <- function(link,
                     img,
                     alt=NULL,
                     width,
                     class="image",
                     id=NULL,
                     text_before=NULL,
                     text_after=NULL, 
                     style=NULL){
  
  if(!file.exists(img)) {
    return("")
  }
 txt <- paste0(
    "<a href=",shQuote(link),
    " target='_blank'",
    " class=",shQuote(class),">",
    text_before,
    "<img src=",shQuote(img), 
    if(!is.null(alt)) paste0(" alt=",shQuote(alt)), 
    if(!is.null(id)) paste0(" id=",shQuote(id)), 
    if(!is.null(width)) paste0(" width=",shQuote(width)),
    if(!is.null(style)) paste0(" style=",shQuote(style)),
    ">",
    text_after,
    "</a>"
  )
 #### Fix windows HTML strings ####
 if(.Platform$OS.type == "windows"){
   # eg = gsub("'","\"",logo)
   txt <-  gsub("\"","'",txt)
 }
 return(txt)
}