#' @describeIn get_ get_
#' @export 
#' @param save_dir Directory to save template file to.
#' @param force_new Overwrite existing template file.
#' @param template Template to use.
#' @param wd Directory where "cv_data" and "img" subdirectories are stored.
get_template <- function(name = "Leonardo da Vinci",
                         tagline = "Simplicity is the ultimate sophistication.",
                         extra = list("### Studio of Andrea del Verrocchio",
                                       "### Painter, draughtsman, engineer, scientist",
                                       "### MD, PhD"
                         ),
                         logo = get_logo(img = system.file("img","davinci.png",
                                                           package = "autoCV"),
                                          width = "60px"),
                         wd = get_data(),
                         template = "CV.Rmd",
                         save_dir = "./",
                         force_new = FALSE){
  # devoptera::args2vars(get_template)
  
  f <- system.file("templates",template,package = "autoCV")
  if(!file.exists(f)){
    stopper("Template file not found:",f)
  }
  if(!is.null(save_dir)){
    f2 <- file.path(save_dir,basename(f)) 
    if(file.exists(f2) && !force_new){
      message(paste0("File already exists: ",f2,".",
                     " Set force_new = TRUE to overwrite.")) 
      return(f2)
    } else {
      yml <- readLines(f)
      yml <- gsub("\\{name\\}",name,yml)
      yml <- gsub("\\{tagline\\}",tagline,yml)
      yml <- gsub("\\{extra\\}",paste(extra,collapse = "\n"),yml)
      yml <- gsub("\\{logo\\}",logo,yml)
      yml <- gsub("\\{wd\\}",wd,yml)
      message(paste0("Saving template to: ",f2))
      writeLines(yml,f2)
    }
    return(f2)
  }else {
    return(f)
  }
}

 
#' @describeIn get_ get_
#' @param save_dir If not NULL, copy example 
#' data ("cv_data/"), 
#' images ("img/"),
#' and CSS ("css/") to this directory.
#' @param subdir Subdirectories to return (when \code{dir_only=FALSE}).
#' @export 
#' @examples
#' wd <-get_data(save_dir = file.path(tempdir(),"example_data"))
get_data <- function(dir_only = TRUE,
                     dir_manual = NULL,
                     save_dir = NULL,
                     subdir = c("cv_data","img","css"),
                     verbose = TRUE){ 
  
  if(!is.null(dir_manual) && 
     dir.exists(dir_manual)) {
    dir_only <- FALSE
    wd <- dir_manual
  } else {
    ## Seems to be returning a local path instead...
    # d <- system.file("cv_data",package = "autoCV")
    lib.loc <- grep("Desktop",.libPaths(), value = TRUE, invert = TRUE)[1]
    wd <- file.path(lib.loc,"autoCV")
    if(!dir.exists(wd)){
      stopper("CV data directory not found:",wd)
    } 
  } 
  #### Return ####
  if(!is.null(save_dir)){ 
    dir.create(save_dir, recursive = TRUE, showWarnings = FALSE)
    messager("Copying example data -->",save_dir,v=verbose)
    for(sd in subdir){
      file.copy(file.path(wd,sd),
                save_dir,
                recursive = TRUE,
                overwrite = TRUE) 
    } 
    return(save_dir)
  } else if(dir_only) {
    return(wd)
  } else { 
    f <- unlist(lapply(file.path(wd,subdir), list.files, full.names = TRUE)) 
    f <- as.list(f) |>
      `names<-`(gsub("\\.*","",basename(f)))
    return(f)
  }
}

#' @describeIn get_ get_
#' @inheritParams base::list.files
#' @export
get_data_list <- function(wd, 
                          full.names = FALSE){
  f <- sapply(list.files(wd, full.names = TRUE),
              list.files,
              recursive = TRUE,
              full.names = full.names)
  names(f) <- basename(names(f))
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
