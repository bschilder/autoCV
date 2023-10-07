





#' @describeIn parse_ parse_
#' @export
parse_location <- function(r,
                           add_link=TRUE){
  
  txt <- paste0(
    if(not_empty(r$City)) paste0(r$City,", "),
    if(not_empty(r$State)) paste0(r$State,", "),
    if(not_empty(r$Country)) r$Country
  )
  baseurl <- "https://www.google.com/maps/place/"
  if(isTRUE(add_link)){
    if(r$Country=="Earth"){
      link <-  paste0("https://www.google.com/maps/",
                      "@47.8287853,41.5077906,22973464m/data=!3m1!1e3")
    } else {
      link <- gsub(" +","+",paste0(baseurl,txt))
    }
    txt2 <- paste0("<a href=",shQuote(link),">",txt,"</a>")
    return(txt2)
  } else {
    return(txt)
  }
}

#' @describeIn parse_ parse_
#' @export
parse_daterange <- function(r){
  if(!not_empty(r$StartYear)){
    r$EndYear
  } else if(!not_empty(r$EndYear)){
    r$StartYear
  } else if(r$EndYear==r$StartYear){
    return(r$EndYear)
  } else {
    paste(r$EndYear,"-",r$StartYear)
  }
}

#' @describeIn parse_ parse_
#' @export
parse_bullets <- function(r,
                          concise=FALSE,
                          check_icons=TRUE){
  
  bullet_cols <- grep("Bullet_",names(r), value = TRUE)
  bullets <- as.list(r[,bullet_cols,with=FALSE])
  bullets <- bullets[bullets!="" & !is.na(bullets)]
  if(length(bullets)==0) return(NULL)
  if(isTRUE(check_icons)){
    bullets <- icon_sub(text = bullets)
  }
  paste0(if(isTRUE(concise))"::: concise\n",
         paste("-",bullets, collapse = "\n"),
         if(isTRUE(concise))"\n:::"
  )
}

#' @describeIn parse_ parse_
#' @export
parse_news <- function(r,
                       title="News",
                       icon="newspaper"){
  # news <- dt$Comments[[16]]
  ctxt <- paste("-",trimws(strsplit(r,";")[[1]]),collapse = "<br>")
  if(!is.null(title)){
    ctxt <- paste0("**",
                   if(!is.null(icon)) fontawesome::fa(icon)," ",
                   title,
                   "**","<br>\n",ctxt)
  }
  return(ctxt)
}


#' @describeIn parse_ parse_
#' @export
parse_education <- function(file=here::here("cv_data","education.csv"),
                            concise=FALSE){
  # ### Beijing University of Chemical Technology
  #
  # B.S. in Information and Computing Sciences
  #
  # Beijing, China
  #
  # 2010
  #
  # Thesis: Dyadic wavelet and its application in edge detection
  
  dt <- data.table::fread(file)
  txt <- lapply(seq_len(nrow(dt)), function(i){
    r <- dt[i,]
    paste(
      paste("### ",
            if(r$Logo!="") img_get(name = r$Logo,
                                   height = "30px",
                                   style="border-radius: 3px"),
            r$Institution
      ),
      paste0("**",r$Degree,"**: ",r$Program,"; ",r$Focus),
      parse_location(r=r),
      r$EndYear,
      paste(
        if(r$Supervisors!="") paste("**Supervisors**:",r$Supervisors),
        # if(r$Group!="") r$Group,
        sep = "; "
      ),
      if(r$Thesis!="") paste("**Thesis**:",r$Thesis),
      parse_bullets(r = r,
                    concise = concise),
      sep = "\n\n"
    )
  }) |> paste(collapse ="\n\n")
  cat(txt)
}

#' @describeIn parse_ parse_
#' @export
parse_publications <- function(file=here::here("cv_data","publications.csv"),
                               name="Leonardo da Vinci",
                               types=NULL){
  # ### ESCRT-0 complex modulates Rbf mutant cell survival...
  #
  # J Cell Sci. 2016 May 15;129(10):2075-84.
  #
  # N/A
  #
  # 2016
  #
  # Sheng Z, **Yu L**, Zhang T, Pei X, Li X, Zhang Z and Du W.
  Type <- NULL;
  dt <- data.table::fread(file)
  if(!is.null(types)){
    dt <- dt[Type %in% types,]
  }
  txt <- lapply(seq_len(nrow(dt)), function(i){
    r <- dt[i,]
    paste(
      paste("###",r$Title),
      paste0("*",r$Journal,"* (",r$Year,") ",
             r$Volume,
             if(not_empty(r$Number)) paste0("(",r$Number,")"),
             if(not_empty(r$Pages)) paste0(":",r$Pages),
             if(any(r[,c("Volume","Number","Pages")]!=""))"; ",
             r$Link
      ),
      "N/A",
      r$Year,
      paste(
        gsub(name,paste0("**",name,"**"),r$Authors),
        if(not_empty(r$Comments)) parse_news(r$Comments),
        sep="<br>"
      ),
      sep = "\n\n"
    )
  })
  cat(paste(txt, collapse ="\n\n"))
}

#' @describeIn parse_ parse_
#' @export
parse_talks <- function(file=here::here("cv_data","talks.csv"),
                        types=NULL){
  # ### ESCRT-0 complex modulates Rbf mutant cell survival...
  #
  # J Cell Sci. 2016 May 15;129(10):2075-84.
  #
  # N/A
  #
  # 2016
  #
  # Sheng Z, **Yu L**, Zhang T, Pei X, Li X, Zhang Z and Du W.
  Type <- NULL;
  dt <- data.table::fread(file)
  if(!is.null(types)){
    dt <- dt[Type %in% types,]
  }
  txt <- lapply(seq_len(nrow(dt)), function(i){
    r <- dt[i,]
    paste(
      paste("###",r$Title),
      paste(if(not_empty(r$Event)) r$Event,
            if(not_empty(r$Department)) r$Department,
            if(not_empty(r$Institution)) r$Institution,
            sep="<br>"
      ),
      "N/A",
      r$Year,
      if(not_empty(r$Comments)) r$Comments,
      sep = "\n\n"
    )
  })
  cat(paste(txt, collapse ="\n\n"))
}

#' @describeIn parse_ parse_
#' @export
parse_experience <- function(file=here::here("cv_data","experience.csv"),
                             types=NULL,
                             concise=FALSE){
  # (Research)
  # ### Data Scientist, intern
  #
  # SupStat Inc.
  #
  # Beijing, China
  #
  # 2011 - 2014
  #
  # ::: concise
  # - Taught R language to beginners.
  # - Wrote Shiny app demos.
  # - Converted statistical tutorials from SPSS to R language.
  # :::
  
  # (Teaching)
  # ### Introduction to R Language for Beginners.
  #
  # Instructor of R and Data Mining Training Courses at SupStat Inc.
  #
  # Beijing, China
  #
  # 2014
  
  
  Type <- NULL;
  dt <- data.table::fread(file)
  if(!is.null(types)){
    dt <- dt[Type %in% types,]
  }
  txt <- lapply(seq_len(nrow(dt)), function(i){
    r <- dt[i,]
    if(r$Type=="teaching"){
      paste(
        paste("###",r$Position),"\n",
        paste0(if(not_empty(r$Institution)) r$Institution else "-",
               if(not_empty(r$Department)) paste0(" (",r$Department,")")
        ),
        parse_location(r = r),
        parse_daterange(r = r),
        parse_bullets(r = r,
                      concise = concise),
        sep = "\n\n"
      )
    }else if(r$Type=="extracurricular"){
      paste(
        paste("###",r$Position),
        parse_bullets(r = r,
                      concise = concise),
        parse_location(r=r),
        ".",
        parse_daterange(r=r),
        sep = "\n\n"
      )
    } else if(r$Type=="data visualisation"){
      paste(
        paste0("### ",
               "[",r$Position,
               # " <img src=",shQuote(r$Link)," height='50' style='valign:top;'>",
               "](",r$Link,")"
        ),
        paste0(if(not_empty(r$Institution)) r$Institution else "N/A",
               if(not_empty(r$Department))paste0(" (",r$Department,")")
        ),
        parse_location(r=r),
        parse_daterange(r=r),
        parse_bullets(r = r,
                      concise = concise),
        sep = "\n\n"
      )
    }else {
      paste(
        paste("###",r$Position),
        paste0(if(not_empty(r$Institution)) r$Institution else "N/A",
               if(not_empty(r$Department))paste0(" (",r$Department,")")
        ),
        parse_location(r=r),
        parse_daterange(r=r),
        parse_bullets(r = r,
                      concise = concise),
        sep = "\n\n"
      )
    }
  }) |> gsub(pattern = "\n\n\n\n",replacement = "\n\n")
  cat(paste(txt, collapse ="\n\n"))
}


#' @describeIn parse_ parse_
#' @export
parse_tools <- function(file=here::here("cv_data","tools.csv"),
                        types=NULL,
                        add_index=TRUE,
                        add_logos=TRUE){
  # ### Data Scientist, intern
  #
  # SupStat Inc.
  #
  # Beijing, China
  #
  # 2014
  #
  # ::: concise
  # - Taught R language to beginners.
  # - Wrote Shiny app demos.
  # - Converted statistical tutorials from SPSS to R language.
  # :::
  
  Type <- NULL;
  dt <- data.table::fread(file)
  if(!is.null(types)){
    dt <- dt[Type %in% types,]
  }
  txt <- lapply(seq_len(nrow(dt)), function(i){
    r <- dt[i,]
    paste(
      paste("###",r$Name,
            if(isTRUE(add_logos)){
              img_get(name = r$Language,
                      height="15px")
            }
      ),
      paste(
        r$Title,
        if(not_empty(r$GitHub)){
          icon_link(link = r$GitHub,
                    icon="fa fa-github")
        },
        if(not_empty(r$Link)){
          icon_link(link = r$Link,
                    icon="fa fa-link")
        },
        if(not_empty(r$PaperLink)){
          icon_link(link = r$PaperLink,
                    icon="fa fa-file")
        },
        sep="<br>"
      ),
      "N/A",
      if(isTRUE(add_index)) paste0(i,"\\.") else "N/A",
      sep = "\n\n"
    )
  }) |> gsub(pattern = "<br><br>",replacement = "<br>")
  cat(paste(txt, collapse ="\n\n"))
}


#' @describeIn parse_ parse_
#' @export
parse_profile <- function(file=here::here("cv_data","profile.csv"),
                          types=NULL,
                          concise=FALSE,
                          sep="\n\n",
                          collapse = "<br>",
                          div="h4",
                          img_width="100px",
                          icn_width="12px",
                          prefix=NULL){
  # - <i class="fa fa-envelope"></i> brian_schilder@alumni.brown.edu
  # - <i class="fa fa-linkedin"></i> [LinkedIn](https://twitter.com/BMSchilder)
  # - <i class="fa fa-github"></i> [GitHub](https://github.com/bschilder)
  # - <i class="fa fa-twitter"></i> [Twitter](https://www.linkedin.com/in/brian-schilder)
  # - <i class="fa fa-globe"></i> [Professional Website](https://bschilder.github.io/BMSchilder)
  # - <i class="fa fa-globe"></i> [Lab Website](https://www.neurogenomics.co.uk/)
  # - <i class="fa fa-phone"></i> **US**: <a href="tel:+1 908-268-9859">+1 908-268-9859</a>
  #  - <i class="fa fa-phone"></i> **UK**: <a href="tel:+44 073-0653-7736">+44 073-0653-7736</a>
  Type <- NULL;
  dt <- data.table::fread(file)
  if(!is.null(types)){
    dt <- dt[Type %in% types,]
  }
  txt <- lapply(seq_len(nrow(dt)), function(i){
    r <- dt[i,]
    if(!grepl("^\\./",r$Icon)){
      paste(
        fontawesome::fa(r$Icon,width=icn_width),
        if(r$Type=="phone"){
          paste0(r$Text,
                 "<br>",
                 "<a href=",shQuote(paste0("tel:",r$Link)),">",
                 r$Link,
                 "</a>")
        } else if(not_empty(r$Text)){
          paste0("[",r$Text,"](",r$Link,")")
        } else {
          r$Link
        }
      )
    } else {
      paste0(
        prefix,
        img_link(link = r$Link,
                 img = r$Icon,
                 alt = r$Text,
                 width = img_width,
                 class = "affiliate"),
        sep,
        "\n",
        parse_bullets(r = r, concise = concise),
        paste(c(" "," ","N/A"), collapse = collapse),
        sep
      )
    }
  }) |> paste(collapse = collapse)
  if(isTRUE(concise)){
    return(
      cat(
        paste(
          "::: concise\n",
          txt,
          ":::"
        )
      )
    )
  } else {
    if(!is.null(div)){
      txt <- paste0("<",div,">",txt,"</",div,">")
    }
    return(cat(txt))
  }
  
}

#' @describeIn parse_ parse_
#' @export
parse_grants_totals <- function(dt){
  unit <- Amount <- Amount2 <- NULL;
  gbp <- "\U00A3"
  dt[,unit:=ifelse(grepl(gbp,Amount),gbp,
                   ifelse(grepl("\\$",Amount),"$",NA)
  )]
  suppressWarnings(
    dt[,Amount2:=as.integer(gsub(paste0(gbp,"|\\$|,"),"",Amount))]
  )
  #### Convert currency ####
  dt[,Amount2:=ifelse(unit=="$",Amount2,Amount2*1.24)]
  sum(dt$Amount2, na.rm = TRUE)
}

#' @describeIn parse_ parse_
#' @export
parse_grants <- function(file=here::here("cv_data","grants.csv"),
                         types=NULL,
                         add_totals=FALSE){
  # ### ###Imperial UK Research Institute Impact Acceleration Account
  #
  # SupStat Inc.
  #
  # Beijing, China
  #
  # 2014
  #
  # ::: concise
  # - Taught R language to beginners.
  # - Wrote Shiny app demos.
  # - Converted statistical tutorials from SPSS to R language.
  # :::
  
  Type <- Role <- NULL;
  dt <- data.table::fread(file)
  if(isTRUE(add_totals)){
    txt_totals <-
      paste(
        paste(
          paste0("### **Total (all grants)**: ",
                 "$",
                 formatC(
                   parse_grants_totals(dt = dt[Type=="grant",]),
                   big.mark = ",",
                   format = "d"
                 )),
          paste0("**Total (as primary applicant)**: ",
                 "$",
                 formatC(
                   parse_grants_totals(dt = dt[Type=="grant" &
                                                 Role=="Primary applicant",]),
                   big.mark = ",",
                   format = "d"
                 )
          ),
          sep="<br>"
        ),
        paste(c(rep("N/A",2),"<hr>",rep("N/A",1)),collapse = "\n\n"),
        sep="\n\n"
      )
    cat(txt_totals,"\n\n")
  }
  if(!is.null(types)){
    dt <- dt[Type %in% types,]
  }
  txt <- lapply(seq_len(nrow(dt)), function(i){
    r <- dt[i,]
    paste(
      paste(
        "###",
        paste(
          if(not_empty(r$GrantName)) r$GrantName,
          if(not_empty(r$Source)) r$Source,
          sep = ",<br>"
        )|> gsub(pattern="^[,<br>]",replacement="",)
      ),
      if(not_empty(r$Project))paste0("**Project**: ",shQuote(r$Project)) else "N/A",
      "N/A",
      parse_daterange(r=r),
      paste(
        ":::  concise",
        if(not_empty(r$Comments)) paste0(parse_news(r$Comments),"\n"),
        if(not_empty(r$Role))paste0("- **Role**: ",r$Role),
        if(not_empty(r$PI) && r$Type!="award")paste0("- **PI**: ",r$PI),
        if(not_empty(r$Amount))paste0("- **Amount**: ",r$Amount),
        ":::",
        sep = "\n"
      ),
      sep = "\n\n"
    )
  }) |>
    gsub(pattern="<br><br>",replacement="<br>") |>
    gsub(pattern="### <br>",replacement="### ")
  
  cat(paste(txt, collapse ="\n\n"))
}

#' @describeIn parse_ parse_
#' @export
parse_skills <- function(file=here::here("cv_data","skills.csv"),
                         types=NULL){
  
  Title <- Type <- Level <- LevelMax <- Percent <- Group <- NULL;
  dt <- data.table::fread(file)
  dt <- dt[,Percent:=round(100*Level/LevelMax)]
  if(!is.null(types)){
    dt <- dt[Type %in% types,]
  }
  txt <- lapply(unique(dt$Group), function(group){
    rg <- dt[Group==group,]
    summary <- rg[Title=="Summary",]$Description
    rg <- rg[Title!="Summary",]
    mean_percent <- round(mean(rg$Percent,na.rm = TRUE),1)
    paste(
      paste("###",group),
      paste(summary,
            # build_skill_bars(percent = mean_percent,
            #                  title = group)
            sep="<br>"
      ),
      "N/A",
      c("::: concise",
        lapply(seq_len(nrow(rg)), function(i){
          r <- rg[i,]
          # build_skill_bars(dt = r)
          paste0("- **",r$Title,"**: ",r$Description)
        }),
        ":::"
      )|> paste(collapse = "\n"),
      paste(rep("N/A",1),collapse = "\n\n"),
      sep = "\n\n",
      collapse = "\n\n"
    )
  })
  #### Add special variables ####
  txt <- gsub("{n_years_experience}",
              paste0("[**",n_years_experience(),"**](#experience)"),
              txt, fixed=TRUE)
  txt <- gsub("{n_rpackages}",
              paste0("[**",n_rpackages(),"**](#packages)"),
              txt, fixed=TRUE)
  txt <- gsub("{n_publications}",
              paste0("[**",n_publications(),"**](#publications)"),
              txt, fixed=TRUE)
  txt <- gsub("{n_posters}",
              paste0("[**",n_posters(),"**](#posters)"),
              txt, fixed=TRUE)
  txt <- gsub("{n_grants}",
              paste0("[**",n_grants(),"**](#grants)"),
              txt, fixed=TRUE)
  txt <- gsub("{n_websites}",
              paste0("[**",n_websites(),"**](#websites)"),
              txt, fixed=TRUE)
  txt <- gsub("{n_webapps}",
              paste0("[**",n_webapps(),"**](#databases)"),
              txt, fixed=TRUE)
  txt <- gsub("{n_web}",
              paste0("[**",n_web(),"**](#databases)"),
              txt, fixed=TRUE)
  cat(paste(txt,collapse = "\n\n"))
}

#' @describeIn build_ build_
#' @export
build_toc <- function(items=NULL,
                      div="h4",
                      collapse="<br>"){
  dict <- icon_dict(items = items,
                    as_icon = TRUE,
                    as_toc = TRUE,
                    collapse = collapse)
  if(!is.null(div)){
    dict <- paste0("<",div,">",dict,"</",div,">")
  }
  cat(dict)
}
