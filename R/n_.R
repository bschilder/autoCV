


#' @describeIn n_ n_
#' @export
n_years_experience <- function(file=here::here("cv_data","experience.csv"),
                               types="research"){
  
  Type <- NULL;
  dt <- data.table::fread(file)
  if(!is.null(types)){
    dt <- dt[Type %in% types,]
  }
  as.integer(format(Sys.Date(),"%Y")) - min(dt$StartYear, na.rm = TRUE)
}

#' @describeIn n_ n_
#' @export
n_tools <- function(file=here::here("cv_data","tools.csv"),
                    types=NULL){
  Type <- NULL;
  dt <- data.table::fread(file)
  if(!is.null(types)){
    dt <- dt[Type %in% types,]
  }
  nrow(dt)
}

#' @describeIn n_ n_
#' @export
n_packages <- function(file=here::here("cv_data","tools.csv"),
                       types=c("package","web app")){
  n_tools(file = file,
          types = types)
}

#' @describeIn n_ n_
#' @export
n_webapps <- function(file=here::here("cv_data","tools.csv"),
                      types="web app"){
  n_tools(file = file,
          types = types)
}

#' @describeIn n_ n_
#' @export
n_websites <- function(file=here::here("cv_data","tools.csv"),
                       types="website"){
  n_tools(file = file,
          types = types)
}

#' @describeIn n_ n_
#' @export
n_web <- function(){
  sum(n_webapps(), n_websites(), na.rm = TRUE)
}

#' @describeIn n_ n_
#' @export
n_databases <- function(file=here::here("cv_data","tools.csv"),
                        types="database"){
  n_tools(file = file,
          types = types)
}

#' @describeIn n_ n_
#' @export
n_rpackages <- function(file=here::here("cv_data","tools.csv"),
                        types="package",
                        languages=c("^R$","^R,*")){
  Type <- Language <- NULL;
  dt <- data.table::fread(file)
  if(!is.null(types)){
    dt <- dt[Type %in% types,]
  }
  # languages <- strsplit(languages,",")[[1]]
  if(!is.null(languages)){
    dt <- dt[grepl(paste(languages,collapse = "|"),Language),]
  }
  nrow(dt)
}

#' @describeIn n_ n_
#' @export
n_publications <- function(file=here::here("cv_data","publications.csv"),
                           types="publication"){
  Type <- NULL;
  dt <- data.table::fread(file)
  if(!is.null(types)){
    dt <- dt[Type %in% types,]
  }
  nrow(dt)
}

#' @describeIn n_ n_
#' @export
n_posters <- function(file=here::here("cv_data","publications.csv"),
                      types="poster"){
  Type <- NULL;
  dt <- data.table::fread(file)
  if(!is.null(types)){
    dt <- dt[Type %in% types,]
  }
  nrow(dt)
}

#' @describeIn n_ n_
#' @export
n_talks <- function(file=here::here("cv_data","talks.csv"),
                    types=NULL){
  Type <- NULL;
  dt <- data.table::fread(file)
  if(!is.null(types)){
    dt <- dt[Type %in% types,]
  }
  nrow(dt)
}

#' @describeIn n_ n_
#' @export
n_grants <- function(file=here::here("cv_data","grants.csv"),
                     types="grant",
                     roles=c("Primary applicant","Co-applicant")){
  Type <- Role <- NULL;
  dt <- data.table::fread(file)
  if(!is.null(types)){
    dt <- dt[Type %in% types,]
  }
  if(!is.null(roles)){
    dt <- dt[tolower(Role) %in% tolower(roles),]
  }
  nrow(dt)
}

