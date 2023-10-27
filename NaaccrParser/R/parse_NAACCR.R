#' @name NAACCR_to_db
#' @description This is the first step of an ETL process by the OHDSI Oncology WG to translate NAACCR data into OHDSI CDM tables.
#' @param file_path
#' character file path of the txt file. It can be null if it's s3 file
#' @param conn_detail
#' database connector object.
#' @param col_nms
#' character vector. names of parsed field.
#' @param db_shc
#' database schema of the EAV table write to.
#' @param tbl_nm
#' table name of the result table.
#' @param is_s3_file
#' logic. whether or not the data source is from s3.
#' @param s3_bucket
#' character. name of s3 bucket.
#' @param s3_file
#' character. name of s3 file
#' @import
#' tidyverse
#' DatabaseConnector
#' SqlRender
#' rstudioapi
#' aws.s3
#' aws.signature
#' @examples
#' \donttest{
#' conn <- DatabaseConnector::connect()
#' NAACCR_to_db(file_path = "path.txt",
#' conn_detail = conn,
#' db_shc = "public",
#' tbl_nm = "test",
#' col_nms = c("person_id","record_id","mrn","histology_site","naaccr_item_number",
#' "naaccr_item_name","naaccr_item_value" ))
#' }
NAACCR_to_db <- function(file_path = "",
                         conn_detail,
                         col_nms,
                         db_shc,
                         tbl_nm,
                         record_id_prefix = "",
                         is_s3_file = FALSE,
                         s3_bucket = "",
                         s3_file = ""){
  # Load record layout
  record_layout <- NAACCR_RL_v18

  # Filter out text fields. TODO: Parse into notes
  # record_layout <- record_layout[record_layout$length < 70,]

  if(is_s3_file) {
    if (s3_bucket == "" | s3_file == ""){
      print("please spcify s3 bucket loaction and file name.")
      cat("\n")
    }
    cat("please make sure you are connected to S3 bucket.")
    cat("\n")

    # wid <- get_object(file_path) %>%
    #          rawConnection %>%
    #          readLines(n=2) %>%
    #          nchar()
    #
    #  curr <- get_object(file_path) %>%
    #            rawConnection %>%
    #            readLines() %>%
    #            data.frame()

    fl <- s3read_using(FUN = readLines,
                       bucket = s3_bucket,
                       object = s3_file)

    wid <- unique(nchar(fl))

    curr <- fl %>% data.frame()

  } else {
    # Get file width
    wid <- nchar(readLines(file_path, n=1))

    # Load file as rows
    curr  <- data.frame(readLines(file_path), stringsAsFactors = FALSE)
  }

  # Change name of text blob column
  names(curr)[1] <- "raw"

  # Add record index
  record_index <- as.character(c(1:nrow(curr)))

  if (missing(record_id_prefix) || is.null(record_id_prefix)) {
    record_index <- paste0( tools::file_path_sans_ext(basename(file_path))
                            ,"/"
                            ,record_index)
  } else{
    record_index <- paste0( record_id_prefix
                            ,"/"
                            ,tools::file_path_sans_ext(basename(file_path))
                            ,"/"
                            ,record_index)
  }

  curr <- cbind(curr, record_index)

  # Create result dataframe
  # ret_df <- data.frame(matrix(nrow= 0, ncol = length(col_nms)))
  # names(ret_df) <- col_nms

  # Loop through each row
  for (i in 1:nrow(curr)){
    # for (i in 1:2){
    print(paste0("row # ", i))
    cat("\n")

    curr_row <- curr[i,]

    tmp_df <- data.frame(matrix(nrow= nrow(record_layout), ncol = length(col_nms)))
    names(tmp_df) <- col_nms

    # Get all (naaccr_item_number, naaccr_item_value) pairs
    for(j in 1:nrow(record_layout)){
      curr_item <- record_layout[j,]

      tmp_df$naaccr_item_number[j] <- curr_item$Item_Num
      tmp_df$naaccr_item_name[j] <- curr_item$Item_Name
      tmp_df$naaccr_item_value[j] <- trimws(substr(curr_row$raw, curr_item$col_start, curr_item$col_end))
    }

    # Restrict to records with specific fields populated (for effiency)
    hist3 <- tmp_df$naaccr_item_value[tmp_df$naaccr_item_number == 521]
    site <- tmp_df$naaccr_item_value[tmp_df$naaccr_item_number == 400]
    dxDate <- tmp_df$naaccr_item_value[tmp_df$naaccr_item_number == 390]

    if(
      # If hist/grade is populated, and site is populated, and diag date is populated
      nchar(hist3) > 3 & nchar(site) > 3 & nchar(dxDate) > 7
    )
    {
      # Populate static rows

      # Record_id
      tmp_df$record_id <- curr_row$record_index

      # MedRedNum
      tmp_df$mrn <- tmp_df$naaccr_item_value[tmp_df$naaccr_item_number == 2300]
      tmp_df$person_id <- tmp_df$naaccr_item_value[tmp_df$naaccr_item_number == 2300]

      # histology_site
      tmp_df$histology_site <- paste0(
        paste0(substr(hist3, 0, 4),"/",substr(hist3, 5, 6))
        ,"-"
        ,substr(site, 0,3)
        ,"."
        ,substr(site, 4,5)
      )

      # Remove empty fields
      tmp_df <- tmp_df[nchar(tmp_df$naaccr_item_value) > 0,]
      print(dim(tmp_df))

      if(nrow(tmp_df) > 0){
        # Append rows to result dataframe

        DatabaseConnector::insertTable(connection = conn,
                                       databaseSchema = db_shc,
                                       tableName = tbl_nm,
                                       data = tmp_df,
                                       dropTableIfExists = FALSE,
                                       createTable = FALSE,
                                       tempTable = FALSE,
                                       bulkLoad = TRUE)
      }
    }
  }
  cat("\n")
  print("Completed")
}



assign_person_id <- function(connectionDetails
                             ,person_map_table
                             ,person_map_field = "MRN"){

  # requires DB.schema.table format
  curr_sql <- SqlRender::render("UPDATE naaccr_data_points SET person_id = x.person_id FROM @table x WHERE naaccr_data_points.mrn = x.@field;"
                                ,field = person_map_field
                                ,table = person_map_table)

  conn <- DatabaseConnector::connect(connectionDetails)
  person_map <- DatabaseConnector::executeSql(connection = conn
                                            ,sql = curr_sql)
  DatabaseConnector::disconnect(conn)
}





