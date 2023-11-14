#' @describeIn build_ build_
#' @export
build_title <- function(name="Leonardo da Vinci",
                        logo=get_logo(),
                        tagline="Curriculum Vitae",
                        extra = list(
                          "### My Affiliation",
                          "### My Current Job",
                          "### My Degrees"),
                        logo_position=c("right","left","bottom")
                        ){ 
  # devoptera::args2vars(build_title)
  logo_position <- logo_position[1]
  cat(
    paste(
      if(logo_position=="right"){
        paste("##",name,logo,"{#title}")  
      } else if (logo_position=="left"){
        paste("##",logo,name,"{#title}")
      } else if (logo_position=="bottom"){
        paste(
          paste("##",name,"{#title}"),
          logo,
          sep="\n"
        )
      } else {
        paste("##",name,"{#title}")
      },
      tagline,
      paste(extra,collapse = "\n"),
      sep = "\n\n"
    )  
  )
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

#' @describeIn build_ build_
#' @export
build_footer <- function(add_github="https://github.com/bschilder/autoCV",
                         add_pagedown=FALSE,
                         add_date=TRUE,
                         img_width="15px",
                         sep="<br>"){
  
  # *This CV was made with [`pagedown`](https://github.com/rstudio/pagedown).*
  #
  # *Last updated: `r format(Sys.Date(),"%b-%d-%Y")`*
  txt <- paste(
    if(isTRUE(add_date)){
      paste0(fontawesome::fa("calendar", width = img_width),
             " *Updated ",format(Sys.Date(),"%b-%d-%Y"),"*")
    },
    if(!is.null(add_github)){ 
      get_logo(width = img_width,
               text_after = " *Made with autoCV*")
    },
    if(isTRUE(add_pagedown)){
      paste0("*Made with",
             "[`pagedown`](https://github.com/rstudio/pagedown).*")
    },
    sep=sep
  )
  # txt <- gsub("[<br>]+","<br>",txt)
  txt <- paste0("<p style='color: rgba(0,0,0,0.5)'>",txt,"</p>")
  cat(txt)
}

#' @describeIn build_ build_
#' @export
#' @param files Named list containing paths to CV data files.
build_summary <- function(wd="./",
                          files = get_data(dir_manual = wd,
                                           subdir = "cv_data"),
                          items=c("n_years_experience_research",
                                  "n_publications",
                                  "n_preprints",
                                  "n_packages",
                                  "n_databases",
                                  "n_talks",
                                  "n_years_experience_teaching"),
                          plus = list(n_years_experience_research="+",
                                      n_publications="",
                                      n_preprints="",
                                      n_packages="",
                                      n_databases="",
                                      n_talks="",
                                      n_years_experience_teaching="+"),
                          collapse = "<br>"
                          ){
  # <i class='fa fa-suitcase'></i> [`r n_years_experience(types="research")`+ years of research experience.](#experience)
  # <i class='fa fa-file'></i> [`r n_publications()` peer-reviewed publications to date.](#publications)
  # <i class='fa fa-desktop'></i> [`r n_packages()` bioinformatics tools developed.](#software)
  # <i class='fa fa-chalkboard-teacher'></i> [`r n_years_experience(types = "teaching")`+ years of teaching/supervising experience.](#teaching)
  # devoptera::args2vars(build_summary)
  
  res <- lapply(stats::setNames(items,items),
                function(x){
                  if(x=="n_years_experience_research"){
                    paste(
                      icon_dict(items = "experience",
                                as_icon = TRUE),
                      paste0(
                        "[",n_years_experience(file = files$experience,
                                               types="research"),
                        plus[[x]]
                      ),
                      "years of research](#experience)"
                    )
                  } else if (x=="n_publications"){
                    paste(
                      icon_dict(items = "publications",
                                as_icon = TRUE),
                      paste0(
                        "[",n_publications(file = files$publications),
                        plus[[x]],
                        " publications",
                        "](#publications)"
                      )
                    )
                  } else if (x=="n_preprints"){
                    paste(
                      icon_dict(items = "preprints",
                                as_icon = TRUE),
                      paste0(
                        "[",n_publications(file = files$publications,
                                           types = "preprint"),
                        plus[[x]],
                        " preprints",
                        "](#preprints)"
                      )
                    )
                  } else if(x=="n_packages"){
                    paste(
                      icon_dict(items = "packages",
                                as_icon = TRUE),
                      paste0(
                        "[",n_tools(file = files$tools,
                                    types = "package"),
                        plus[[x]]
                      ),
                      "software packages](#packages)"
                    )
                  } else if(x=="n_databases"){
                    paste(
                      icon_dict(items = "databases",
                                as_icon = TRUE),
                      paste0(
                        "[",n_tools(file = files$tools,
                                    types = c("database","web app")),
                        plus[[x]]
                      ),
                      "databases & apps](#databases)"
                    )
                  } else if(x=="n_talks"){
                    paste(
                      icon_dict(items = "conference_talks",
                                as_icon = TRUE),
                      "[",n_talks(file = files$talks),
                      "talks](#talks)"
                    )
                  } else if(x=="n_years_experience_teaching"){
                    paste(
                      icon_dict(items = "teaching",
                                as_icon = TRUE),
                      paste0(
                        "[",n_years_experience(file = files$experience,
                                               types='teaching'),
                        plus[[x]]
                      ),
                      "years of teaching & team management](#teaching)"
                    )
                  }
                })
  if(!is.null(collapse)){
    cat(paste(res,collapse = collapse))
  } else {
    return(res)
  }
}

#' @describeIn build_ build_
#' @export
build_network <- function(files=list.files(path = file.path("cv_data"),
                                           pattern = ".csv$",
                                           full.names = TRUE),
                          min_count = 2,
                          min_nchar = 2,
                          layout = "layout_as_star",
                          center = tolower("BM Schilder"),
                          shape = "hexagon",
                          randomSeed = 2023,
                          save_path = NULL,
                          show_plot = TRUE,
                          height = 700,
                          width = 1200,
                          export_type="png"){
  
  # templateR:::source_all(path = "code")
  # templateR:::args2vars(build_network)
  
  requireNamespace("textnets")
  requireNamespace("igraph")
  requireNamespace("visNetwork")
  
  
  count <- NULL;
  
  #### Convert CSVs to text ####
  dt <- lapply(stats::setNames(files,
                               gsub("\\.csv","",basename(files))),
               function(f){
                 txt <- gsub("_"," ",unname(unlist(as.list(data.table::fread(f)))))
                 data.table::data.table(text=paste(txt[txt!="" & !is.na(txt)],
                                                   collapse = " "))
               }) |> data.table::rbindlist(use.names = TRUE,
                                           idcol = 'file')
  #### Prepare text ####
  ## WARNING: extnets::PrepText downloads a file and stores is in your wd. 
  ## Can't change the download path due to "model_dir" arg being internal only.
  prepped <- textnets::PrepText(dt,
                                groupvar = "file",
                                textvar = "text",
                                node_type ="words",
                                tokenizer = "words",
                                pos = "nouns",
                                remove_stop_words = TRUE,
                                compound_nouns = TRUE)
  #### Filter words ####
  prepped <- subset(prepped,
                    count>min_count &
                      nchar(prepped$lemma)>min_nchar)
  g <- textnets::CreateTextnet(tidytextobject = prepped)
  #### Add connectivity ####
  igraph::V(g)$degree <- igraph::degree(g, mode = "all")
  #### Cluster ####
  clusters <- igraph::cluster_louvain(g, resolution = 5)
  igraph::V(g)$cluster <- clusters$membership
  #### Node size #####
  size_var <- "degree"
  igraph::V(g)$value <- igraph::vertex_attr(g,size_var)
  if(center %in% names(igraph::V(g))){
    igraph::V(g)$value[which(names(igraph::V(g))==center)] <-
      max(igraph::V(g)$value)*100
  }
  #### Node color ####
  color_var <- "cluster"
  ncolors <- length(unique(igraph::vertex_attr(g,color_var)))
  my_palette <- pals::ocean.thermal(ncolors)
  igraph::V(g)$color <- my_palette[
    cut(igraph::vertex_attr(g,color_var) ,ncolors)
  ]
  #### Plot ####
  # net <- textnets::VisTextNet(text_network = g,
  #                             label_degree_cut = 0)
  # netd3 <- textnets::VisTextNetD3(text_network = g)
  visnet <- visNetwork::visIgraph(g,
                                  layout = layout,
                                  randomSeed = randomSeed,
                                  center = center) |>
    visNetwork::visNodes(opacity = .5,
                         shape = shape,
                         shadow = list(enabled=TRUE,
                                       size=10),
                         font = list(strokeWidth=20,
                                     strokeColor="rgba(255,255,255,.5)"),
                         scaling=list(max=200,
                                      label=list(min=30,
                                                 maxVisible=2000,
                                                 max=100))) |>
    visNetwork::visEdges(color = list(opacity=.7),
                         # width=3,
                         # smooth = list(enabled=TRUE,
                         #               type="continuous",
                         #               roundness=.5
                         #               ),
                         dashes = FALSE) |>
    visNetwork::visExport(type = export_type,
                          name = paste0(gsub(" ","_",center),"_network"))|>
    visNetwork::visOptions(height = height,
                           width = width)
  if(!is.null(save_path)){
    dir.create(dirname(save_path), showWarnings = FALSE, recursive = TRUE)
    visNetwork::visSave(graph = visnet,
                        file = save_path,
                        background = "transparent",#"rgba(0,0,0,0.9)",
                        selfcontained = TRUE)
  }
  if(isTRUE(show_plot)){
    methods::show(visnet)
  }
  return(visnet)
}

#' @describeIn build_ build_
#' @export
build_skill_bars <- function(percent,
                             title=""){
  bar_color <- "#969696"
  bar_background <- "#d9d9d9"
  paste(
    "<div class = 'skill-bar'",
    "style = \"background:linear-gradient(to right,",
    paste0("{",bar_color,"}"),
    paste0("{",percent,"}%,"),
    paste0("{",bar_background,"}"),
    paste0("{",percent,"}%"),
    "100%)\" >",
    paste0("{",title,"}"),
    "</div>"
  )
}

#' @describeIn build_ build_
#' @export
build_skills_plot <- function(wd="./",
                              file=file.path(wd,"cv_data","skills.csv"),
                              types=NULL){
  
  Title <- Type <- Level <- LevelMax <- Percent <- Group <- NULL;
  dt <- data.table::fread(file)
  dt <- dt[,Percent:=round(100*Level/LevelMax)]
  if(!is.null(types)){
    dt <- dt[Type %in% types,]
  }
  
  dt_plot <- data.table::copy(dt)[Title!="Summary",] |>
    data.table::setorderv(cols = c("Group","Title"))
  dt_plot$Title <- factor(dt_plot$Title,unique(dt_plot$Title), ordered = TRUE)
  ggplot2::ggplot(dt_plot,
                  ggplot2::aes(x=Title, y=Percent,
                               fill=Group)) +
    ggplot2::geom_bar(stat = "identity")  +
    ggplot2::coord_polar() +
    ggplot2::theme_bw() +
    ggplot2::scale_fill_viridis_d(alpha = 0.8)
}
