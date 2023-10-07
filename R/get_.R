#' @describeIn get_ get_
#' @export 
#' @param save_dir Directory to save template file to.
get_template <- function(save_dir=here::here()){
  
  f <- system.file("templates","autoCV.Rmd",package = "autoCV")
  if(!is.null(save_dir)){
    f2 <- file.path(save_dir,basename(f)) 
    file.copy(f, f2)
    return(f2)
  }else {
    return(f)
  }
}



#' @describeIn get_ get_
#' @export 
get_data <- function(dir_only=TRUE,
                     dir_manual=NULL){ 
  
  if(!is.null(dir_manual)) dir_only <- FALSE
  d <- system.file("cv_data",package = "autoCV")
  if(dir_only) return(d)
  f <- list.files(d, full.names = TRUE)
  f <- as.list(f) |> `names<-`(gsub(".csv$","",basename(f)))
  
  if(!is.null(dir_manual)){
    f <- lapply(f,function(f){file.path(dir_manual,basename(f))})
  }
  return(f)
}

#' @describeIn get_ get_
#' @export 
get_css <- function(){
  list.files(system.file("css", package = "autoCV"), full.names = TRUE)[1]
}

#' @describeIn get_ get_
#' @export 
get_logo <- function(width = "40px",
                     id = "header_img",
                     text_before = NULL,
                     text_after = NULL, 
                     link = "https://github.com/bschilder/autoCV",
                     img = system.file("hex","hex.png", package = "autoCV"),
                     style = "border-radius:5%;"){
  img_link(
    link = link,
    img = img,
    id = id, 
    width = width, 
    alt = id,
    style = style,
    text_before = text_before,
    text_after = text_after)
}