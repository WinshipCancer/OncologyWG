#' @name NAACCR_to_df
#' @description This function
#' @param file_path
#' character. file path of the txt file. It can be null if it's s3 file.
#' @param is_s3_file
#' logic. whether or not read data from s3.
#' @param s3_bucket
#' character. name of s3 bucket.
#' @param s3_file
#' character. name of s3 file.
#' @param use_full_data
#' logic. indicator of whether or not to want a full data table. Default is TRUE to keep full data. If it's FALSE, then filter the data by hist3, site, and dxDate.
#' @return dataframe. 
#' @import
#' tidyverse
#' DatabaseConnector
#' SqlRender
#' rstudioapi
#' aws.s3
#' aws.signature
#' @examples
#' \donttest{
#' df <- NAACCR_to_df(file_path = "",
#' is_s3_file = TRUE,
#' s3_bucket = "***/***/***",
#' s3_file = "***.txt",
#' use_full_data = TRUE)
#' 
#' df <- NAACCR_to_df(file_path = "C:\\..\\**.txt",
#'                   use_full_data = FALSE)
#' }
NAACCR_to_df <- function(file_path = "",
                         record_id_prefix = NULL,
                         is_s3_file = FALSE,
                         s3_bucket = "",
                         s3_file = "",
                         use_full_data = TRUE){
  col_nms <- c("person_id","record_id","mrn","histology_site", "naaccr_item_number","naaccr_item_name","naaccr_item_value")
  out <- data.frame(matrix(nrow= 0, ncol = length(col_nms)))
  names(out) <- col_nms
  
  # Get NAACCR version
  # 160 = v16, 170 = v17, etc
  if(is_s3_file) { 
    if (s3_bucket == "" | s3_file == ""){
      print("please specify s3 bucket loaction and file name.")
      cat("\n")
    }
    cat("please make sure you are connected to S3 bucket.")
    cat("\n")
    fl <- s3read_using(FUN = readLines,
                       bucket = s3_bucket,
                       object = s3_file)
    
    wid <- unique(nchar(fl))
    curr <- fl %>% data.frame()
    naaccr_version <- substr(fl[1], 17,19)
  } else {
    naaccr_version <- substr(readLines(file_path, n=1), 17,19)
    # Get file width
    wid <- nchar(readLines(file_path, n=1))
    # Load file as rows
    curr  <- data.frame(readLines(file_path), stringsAsFactors = FALSE)
  }
  
  # Load record layout
  if(naaccr_version == "150"){
    record_layout <- NAACCR_RL_v15
  } else if (naaccr_version == "160"){
    record_layout <- NAACCR_RL_v16
  } else if (naaccr_version == "180"){
    record_layout <- NAACCR_RL_v18
  } else {
    stop("Unrecognized version.")
  }
  # Filter out text fields. TODO: Parse into notes
  record_layout <- record_layout[record_layout$length < 70,]
  
  # Change name of text blob column
  names(curr)[1] <- "raw"
  
  # Add record index
  record_index <- as.character(c(1:nrow(curr)))
  
  if (missing(record_id_prefix) || is.null(record_id_prefix)) {
    record_index <- paste0(tools::file_path_sans_ext(basename(file_path)), "/", record_index)
  } else{
    record_index <- paste0(record_id_prefix, "/", tools::file_path_sans_ext(basename(file_path)), "/", record_index)
  }
  
  curr <- cbind(curr, record_index)
  
  for (i in 1:nrow(curr)){
    curr_row <- curr[i,]
    tmp_df <- data.frame(matrix(nrow= nrow(record_layout), ncol = length(col_nms)))
    names(tmp_df) <- col_nms
    
    # loop through Get all (naaccr_item_number, naaccr_item_value) pairs
    for(j in 1:nrow(record_layout)){
      curr_item <- record_layout[j,]
      tmp_df$naaccr_item_number[j] <- curr_item$Item_Num
      tmp_df$naaccr_item_name[j] <- curr_item$Item_Name
      tmp_df$naaccr_item_value[j] <- trimws(substr(curr_row$raw, curr_item$col_start, curr_item$col_end))
    }
    
    # Restrict to records with specific fields populated (for efficiency)
    hist3 <- tmp_df$naaccr_item_value[tmp_df$naaccr_item_number == 521]
    site <- tmp_df$naaccr_item_value[tmp_df$naaccr_item_number == 400]
    dxDate <- tmp_df$naaccr_item_value[tmp_df$naaccr_item_number == 390]
    
    tmp_df <- tmp_df %>%
              mutate(record_id = curr_row$record_index,
                     mrn = naaccr_item_value[tmp_df$naaccr_item_number == 2300],
                     histology_site = paste0(substr(hist3, 0, 4),"/",substr(hist3, 5, 6),"-",substr(site, 0,3),".",substr(site, 4,5)),
                     person_id = NA) %>%
                       filter(nchar(naaccr_item_value) > 0) %>% # Remove empty fields 
              select(all_of(col_nms))
    
    if(!use_full_data) {
       tmp_df <- tmp_df %>%
                  filter(nchar(hist3) > 3 & nchar(site) > 3 & nchar(dxDate) > 7)
    }
    
    if(nrow(tmp_df) > 0) {
      out <- rbind(out,tmp_df)
    }
  }
  return(out)
}
#****** end of the function ******#

#****** int_s3() ******#
# function to interact with aws-s3 buckets
int_s3 <- function(region = "us-east-1"){
  aws <- use_credentials(
    profile = Sys.getenv("AWS_PROFILE", "default"),
    file = Sys.getenv("AWS_SHARED_CREDENTIALS_FILE", default_credentials_file())
  )
  
  Sys.setenv(
    AWS_ACCESS_KEY_ID = aws[[1]][1]$AWS_ACCESS_KEY_ID,
    AWS_SECRET_ACCESS_KEY = aws[[1]][2]$AWS_SECRET_ACCESS_KEY,
    AWS_SESSION_TOKEN = aws[[1]][3]$AWS_SESSION_TOKEN,
    AWS_DEFAULT_REGION = region
  )
}

#****** end of the function ******#



