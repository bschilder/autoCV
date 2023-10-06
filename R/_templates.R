#' @title Count functions
#' 
#' @description
#' Functions to compute counts from various aspects of your CV data.
#' @family n_
#' @param file Data file path.
#' @param types Subset the `Type` column.
#' @param roles Subset the `Role` column.
#' @param languages Subset the `Language` column.
#' @returns Formatted data.
#' 
#' @name n_
NULL


#' @title Parse functions
#' 
#' @description
#' Functions to parse rows from your CV data.
#' @family parse_
#' @inheritParams n_
#' @inheritParams base::paste
#' @param r One row from a \link[data.table]{data.table}.
#' @param name Your name to put on the CV.
#' @param concise Use the concise layout.
#' @param title Title.
#' @param icon Icon.
#' @param check_icons Check icons.
#' @param add_link Add link.
#' @param add_index Add index.
#' @param add_logos Add logos.
#' @param div HTML div class.
#' @param img_width Image width. 
#' @param icn_width Icon width. 
#' @param prefix Prefix.
#' @param dt Grants \link[data.table]{data.table}.
#' @returns Formatted data.
#' 
#' @name parse_
NULL