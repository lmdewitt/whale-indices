library(jsonlite)

# Read CSV and load data into matrix/data frame structure
csv_raw <- read.csv("data/indicators.csv", header = FALSE, stringsAsFactors = FALSE)
labels <- unlist(csv_raw[1, -1])  # Exclude first element ("group")
data_rows <- csv_raw[-1, ]

# Parse data rows into named lists
datalist_raw <- apply(data_rows, 1, function(row) {
  row_data <- row[-1]
  d <- as.list(row_data)
  names(d) <- labels
  return(d)
})

groups <- unique(data_rows[, 1])
nlines <- length(datalist_raw)

# Organize items into group buckets
datalist <- list()
for (k in seq_along(groups)) {
  grp <- groups[k]
  matching_indices <- which(data_rows[, 1] == grp)
  datalist[[k]] <- datalist_raw[matching_indices]
}

groupitems <- list()

for (k in seq_along(groups)) {
  current_group_data <- datalist[[k]]
  n_items <- length(current_group_data)
  
  title <- vector("character", n_items)
  viz <- vector("logical", n_items)
  dset <- vector("character", n_items)
  color <- vector("character", n_items)
  name <- vector("character", n_items)
  query <- vector("character", n_items)
  ylabel <- vector("character", n_items)
  table_vec <- vector("character", n_items)
  minT <- vector("character", n_items)
  maxT <- vector("character", n_items)
  inst <- vector("character", n_items)
  units <- vector("character", n_items)
  summary <- vector("character", n_items)
  
  alltimes <- character(0)
  ind <- list()
  series <- list()
  options <- list()
  
  for (j in seq_len(n_items)) {
    item <- current_group_data[[j]]
    
    title[j] <- ifelse(is.null(item$title), "", item$title)
    viz[j]   <- as.logical(item$viz)
    dset[j]  <- ifelse(is.null(item$dset), "", item$dset)
    color[j] <- ifelse(is.null(item$color), "", item$color)
    name[j]  <- ifelse(is.null(item$name), "", item$name)
    query[j] <- paste0(item$query_parameter, '="', item$query_value, '"')
    ylabel[j] <- ifelse(is.null(item$ylabel), "", item$ylabel)
    table_vec[j] <- ifelse(is.null(item$table), "", item$table)
    
    # Fetch Min/Max Times
    url_minmax <- paste0('https://oceanview.pfeg.noaa.gov/erddap/tabledap/allDatasets.json?minTime,maxTime&datasetID=%22', dset[j], '%22')
    minmax_json <- tryCatch(fromJSON(url_minmax), error = function(e) NULL)
    
    if (!is.null(minmax_json) && length(minmax_json$table$rows) > 0) {
      # Use matrix indexing [row, column] instead of list indexing [[1]]
      minT[j] <- minmax_json$table$rows[1, 1]
      maxT[j] <- minmax_json$table$rows[1, 2]
    } else {
      minT[j] <- ""
      maxT[j] <- ""
    }
    
    # Fetch ERDDAP dataset CSV values
    url_csv <- paste0('https://oceanview.pfeg.noaa.gov/erddap/tabledap/', dset[j], '.csv?time,', name[j])
    if (!is.null(item$query_parameter) && item$query_parameter != "") {
      url_csv <- paste0(url_csv, URLencode(paste0('&', item$query_parameter, '="', item$query_value, '"'), reserved = TRUE))
    }
    if (dset[j] == "cciea_OC_UI_day") {
      url_csv <- paste0(url_csv, URLencode('&time>=now-90days', reserved = TRUE))
    }
    
    datavals <- tryCatch(readLines(url_csv, warn = FALSE), error = function(e) character(0))
    times <- character(0)
    ind[[j]] <- list()
    
    if (length(datavals) >= 3) {
      rows <- datavals[3:length(datavals)] # Skip first 2 header lines
      for (row in rows) {
        rarr <- unlist(read.csv(text = row, header = FALSE, stringsAsFactors = FALSE))
        t_val <- rarr[1]
        
        if (!is.null(t_val) && t_val != "") {
          if (dset[j] != "cciea_OC_MHW" && dset[j] != "cciea_OC_UI_day") {
            # Replace characters at position 9-10 with "01" and 12-13 with "00"
            substr(t_val, 9, 10) <- "01"
            substr(t_val, 12, 13) <- "00"
          }
          times <- c(times, t_val)
          ind[[j]][[t_val]] <- rarr[2]
        }
      }
    }
    alltimes <- c(alltimes, times)
    
    # Metadata Fetching
    if (dset[j] != "cciea_OC_UI_day") {
      murl <- "https://oceanview.pfeg.noaa.gov/erddap/tabledap/CCIEA_metadata.csv?source_data_summary,additional_calculations,principal_investigator,contact,institution,units"
      mquery <- URLencode(paste0('&erddap_dataset_id="', dset[j], '"&erddap_variable_name="', name[j], '"'), reserved = TRUE)
      if (!is.null(item$query_parameter) && item$query_parameter != "") {
        mquery <- paste0(mquery, URLencode(paste0('&erddap_query_parameter="', item$query_parameter, '"&erddap_query_value="', item$query_value, '"'), reserved = TRUE))
      }
      murl <- paste0(murl, mquery)
      
      meta_df <- tryCatch(read.csv(murl, skip = 2, header = FALSE, stringsAsFactors = FALSE), error = function(e) NULL)
      meta_headers <- tryCatch(names(read.csv(murl, nrows = 1)), error = function(e) NULL)
      
      if (!is.null(meta_df) && !is.null(meta_headers) && nrow(meta_df) > 0) {
        metarray <- as.list(meta_df[1, ])
        names(metarray) <- meta_headers
        
        inst[j]    <- ifelse(is.null(metarray$institution), "", metarray$institution)
        units[j]   <- ifelse(is.null(metarray$units), "", metarray$units)
        summary[j] <- paste(ifelse(is.null(metarray$source_data_summary), "", metarray$source_data_summary),
                            ifelse(is.null(metarray$additional_calculations), "", metarray$additional_calculations))
      } else {
        inst[j] <- ""; units[j] <- ""; summary[j] <- ""
      }
    } else {
      inst[j]    <- "NOAA/SWFSC/ERD"
      units[j]   <- "m^3/s/100m coastline"
      summary[j] <- "Upwelling index computed from 1-degree 6-hourly FNMOC sea level pressure. The coastal Upwelling Index is an index of the strength of the wind forcing on the ocean which has been used in many studies of the effects of ocean variability on the reproductive and recruitment success of many fish and invertebrate species."
    }
    
    # Handle Dygraphs right axis dynamic attributes
    if (!is.null(item$right_axis) && item$right_axis == "yes") {
      thisser <- list(
        axis = "y2",
        showInRangeSelector = "true",
        independentTicks = "true"
      )
      thissername <- title[j]
      if (units[j] != "") thissername <- paste0(thissername, " (", units[j], ")")
      series[[thissername]] <- thisser
      options$y2label <- item$y2label
    }
  }
  
  # Format plot matrix rows by unique timestamp
  alltimes <- sort(unique(alltimes))
  data_matrix <- list()
  
  for (time_val in alltimes) {
    row <- list(time_val)
    notnullrow <- FALSE
    
    for (j in seq_len(n_items)) {
      val <- ind[[j]][[time_val]]
      if (!is.null(val) && val != "NaN") {
        row <- c(row, list(val))
        notnullrow <- TRUE
      } else {
        row <- c(row, list(NULL))
      }
    }
    if (notnullrow) {
      data_matrix[[length(data_matrix) + 1]] <- row
    }
  }
  
  # Construct label list
  label_list <- c("Time (UTC)")
  for (j in seq_len(n_items)) {
    lab <- title[j]
    if (units[j] != "") lab <- paste0(lab, " (", units[j], ")")
    label_list <- c(label_list, lab)
  }
  
  # Dygraphs Options array mapping
  options$labels <- label_list
  options$ylabel <- datalist[[k]][[1]]$ylabel
  options$visibility <- viz
  options$colors <- color
  options$strokeWidth <- "3"
  options$pointSize <- "4"
  options$highlightCircleSize <- "5"
  options$xRangePad <- "5"
  options$yRangePad <- "5"
  options$legend <- "always"
  options$series <- series
  options$labelsUTC <- "true"
  options$labelsSeparateLines <- "true"
  options$rightGap <- "20"
  options$showRangeSelector <- "true"
  options$connectSeparatedPoints <- "true"
  
  # Date window calculations (milliseconds)
  days <- 1000 * 3600 * 24
  years <- days * 365
  now <- as.numeric(Sys.time()) * 1000
  
  window_start <- now - (years * 5)
  if (grepl("Daily", groups[k], fixed = TRUE)) {
    window_start <- now - (days * 90)
  }
  options$labelsDiv <- paste0("legdiv", k - 1) # Match 0-indexed string key pattern
  options$dateWindow <- c(window_start, now)
  options$width <- "700"
  options$height <- "300"
  
  # Construct group item structure
  groupitems[[k]] <- list(
    title = groups[k],
    titles = as.list(title),
    options = options,
    dsets = as.list(dset),
    names = as.list(name),
    query = as.list(query),
    minT = as.list(minT),
    maxT = as.list(maxT),
    inst = as.list(inst),
    summary = as.list(summary),
    units = as.list(units),
    data = data_matrix
  )
}

# Wrap output into root object and export formatted JSON
grouplist <- list(
  name = "groups",
  title = "Groups",
  items = groupitems
)

write(toJSON(grouplist, auto_unbox = TRUE, pretty = TRUE, null = "null"), file = "data/items_whale.json")
