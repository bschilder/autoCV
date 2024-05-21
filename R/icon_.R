


#' @describeIn icon_ icon_
#' @export
icon_link <- function(link,
                      text_before=NULL,
                      text_after=link,
                      icon){
  paste0("<a href=",shQuote(link)," target='_blank'>",
         text_before,
         "<i class=",shQuote(icon),"></i> ",
         text_after,
         "</a>")
}

#' @describeIn icon_ icon_
#' @export
icon_dict <- function(items=NULL,
                      dict=list(education="graduation-cap",
                                skills="check",
                                expertise="check",
                                publications="file-text",
                                preprints="file-edit",
                                acknowledgements="file",
                                reviewerships="file",
                                internal_talks="person-chalkboard",
                                invited_talks="person-chalkboard",
                                conference_talks="person-chalkboard",
                                posters="person-chalkboard",
                                experience="suitcase",
                                teaching="chalkboard-teacher",
                                packages="wrench",
                                # web_apps="desktop",
                                websites="computer",
                                databases="database",
                                grants="dollar",
                                awards="award",
                                affiliations="building-columns",
                                data_visualisation="circle-nodes",
                                extracurricular="icons"
                      ),
                      as_fa=FALSE,
                      as_icon=FALSE,
                      as_toc=FALSE,
                      collapse="<br>",
                      icn_width="12px"){
  
  # fontawesome:::alias_tbl[grepl("file",fontawesome:::alias_tbl$name),]
  if(isTRUE(as_toc) && isFALSE(as_icon)){
    message("as_fa and as_icon must both be TRUE when as_toc is TRUE. ",
            "Setting as_fa=TRUE and as_icon=TRUE.")
  }
  if(isTRUE(as_fa)){
    dict <- lapply(dict,function(x){ paste0("fa fa-",x)})
  }
  if(isTRUE(as_icon)){
    # dict <- lapply(dict,function(x){paste0("<i class=",shQuote(x),"></i>")})
    dict <- lapply(dict, function(x){fontawesome::fa(x,width = icn_width)})
    
  }
  if(isTRUE(as_toc)){
    # <i class='fa fa-award'></i> [Awards](#awards)
    dict <- lapply(stats::setNames(names(dict),
                                   names(dict)),
                   function(nm){
                     paste0(dict[[nm]]," [",
                            gsub("_"," ",stringr::str_to_title(nm)),"](#",nm,")")
                   })
  }
  if(!is.null(items)){
    dict <- dict[items]
  }
  if(!is.null(collapse)){
    return(paste(dict,collapse = collapse))
  } else {
    return(dict)
  }
}

#' @describeIn icon_ icon_
#' @export
#' @param text Text to run substitution on.
icon_sub <- function(text){
  # txt <- "berjbgnerg @[file-text] 3gemrg @[book]"
  sapply(text, function(txt){
    splt <- strsplit(txt," ")[[1]]
    tags <- grep("^@\\[*.*\\]",splt, value = TRUE)
    if(length(tags)>0){
      for(tag in tags){
        icon <- gsub("@|\\[|\\]","",tag)
        txt <- gsub(tag,fontawesome::fa(icon),txt, fixed = TRUE)
      }
    }
    return(txt)
  })
}
