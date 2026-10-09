library(jsonlite)
library(httr)

outfile <- "data/test_items_whale.json"
indicatorfile <- "data/indicators.csv"
annotationsfile <- "data/annotations.csv"

# 1. Read indicators.csv
if (!file.exists(indicatorfile)) {
  stop("indicators file not found.")
}

datain <- read.csv(indicatorfile, stringsAsFactors = FALSE, check.names = FALSE)

# Preserve original column names as keys
keys <- names(datain)[-1]  # Exclude first column (Group)
nlabels <- length(keys)

# Identify unique groups preserving original order of appearance
groups_unique <- unique(datain[[1]])

grouplist <- list(
  name = "groups",
  title = "Groups"
)

groupitems <- list()

# Process each group
for (k in seq_along(groups_unique)) {
  grp_name <- groups_unique[k]
  
  # Get all rows matching this group
  grp_rows <- datain[datain[[1]] == grp_name, ]
  
  # Subtitle comes from the last column of the first item in the group
  grp_subtitle <- grp_rows[1, ncol(grp_rows)]
  if (is.na(grp_subtitle)) grp_subtitle <- ""
  
  # Initialize vectors/lists for options
  titles <- c()
  viz <- c()
  colors <- c()
  dsets <- c()
  names_vec <- c()
  queries <- c()
  ylabels <- c()
  minT <- c()
  maxT <- c()
  inst <- c()
  units <- c()
  summary <- c()
  stats <- list()
  
  ind <- list()       # Named list of time -> value maps for each series
  alltimes <- c()    # Store all unique timestamps across series
  
  series <- list()   # Object for dygraphs series configuration
  
  for (j in 1:nrow(grp_rows)) {
    row <- grp_rows[j, ]
    
    title_j <- row[["title"]]
    viz_j   <- as.logical(row[["viz"]])
    dset_j  <- row[["dset"]]
    color_j <- row[["color"]]
    name_j  <- row[["name"]]
    q_param <- row[["query_parameter"]]
    q_val   <- row[["query_value"]]
    ylabel_j<- row[["ylabel"]]
    r_axis  <- as.character(row[["right_axis"]])
    
    titles    <- c(titles, title_j)
    viz       <- c(viz, viz_j)
    colors    <- c(colors, color_j)
    dsets     <- c(dsets, dset_j)
    names_vec <- c(names_vec, name_j)
    ylabels   <- c(ylabels, ylabel_j)
    
    # Construct query string representation
    queries <- c(queries, paste0(q_param, '="', q_val, '"'))
    
    # --- Fetch min/max time from ERDDAP ---
    minmax_url <- paste0(
      'https://oceanview.pfeg.noaa.gov/erddap/tabledap/allDatasets.json?minTime,maxTime&datasetID=%22',
      dset_j,
      '%22'
    )
    
    resp_minmax <- tryCatch({
      res <- GET(minmax_url, timeout(15))
      if (status_code(res) == 200) {
        fromJSON(content(res, as = "text", encoding = "UTF-8"))
      } else {
        NULL
      }
    }, error = function(e) NULL)
    
    if (!is.null(resp_minmax) && !is.null(resp_minmax$table$rows) && length(resp_minmax$table$rows) > 0) {
      minT <- c(minT, resp_minmax$table$rows[1, 1])
      maxT <- c(maxT, resp_minmax$table$rows[1, 2])
    } else {
      minT <- c(minT, "")
      maxT <- c(maxT, "")
    }
    
    # --- Fetch Series Data from ERDDAP ---
    data_url <- paste0(
      'https://oceanview.pfeg.noaa.gov/erddap/tabledap/',
      dset_j,
      '.csv?time,',
      name_j
    )
    
    if (!is.na(q_param) && q_param != "") {
      data_url <- paste0(data_url, URLencode(paste0('&', q_param, '="', q_val, '"')))
    }
    
    # Download raw CSV content
    data_resp <- tryCatch({
      res <- GET(data_url, timeout(15))
      content(res, as = "text", encoding = "UTF-8")
    }, error = function(e) "")
    
    series_ind <- list()
    times_j <- c()
    
    if (data_resp != "") {
      lines <- unlist(strsplit(data_resp, "\n"))
      # Skip header lines (skip 2 lines equivalent)
      if (length(lines) > 2) {
        for (line in lines[3:length(lines)]) {
          if (nchar(trimws(line)) == 0) next
          fields <- read.csv(text = line, header = FALSE, stringsAsFactors = FALSE)
          t_val <- as.character(fields[1, 1])
          v_val <- fields[1, 2]
          
          if (!is.na(t_val) && t_val != "") {
            if (nchar(t_val) >= 12) {
              substr(t_val, 9, 10) <- "01"
              substr(t_val, 12, 13) <- "00"
            }
            times_j <- c(times_j, t_val)
            series_ind[[t_val]] <- v_val
          }
        }
      }
    }
    
    ind[[j]] <- series_ind
    alltimes <- c(alltimes, times_j)
    
    # --- Calculate Statistics ---
    vals <- unlist(series_ind)
    vals <- as.numeric(vals[!is.na(vals) & vals != "NaN"])
    
    if (length(vals) > 0) {
      n_val <- length(vals)
      mean_val <- mean(vals)
      # Population standard deviation matching PHP formula
      std_val <- sqrt(sum((vals - mean_val)^2) / n_val)
      min_val <- min(vals)
      max_val <- max(vals)
      stats[[j]] <- c(mean_val, std_val, min_val, max_val)
    } else {
      stats[[j]] <- c(0, 0, 0, 0)
    }
    
    # --- Fetch Metadata from ERDDAP ---
    murl <- "https://oceanview.pfeg.noaa.gov/erddap/tabledap/CCIEA_metadata.csv?source_data_summary,additional_calculations,principal_investigator,contact,institution,units"
    mquery <- URLencode(paste0('&erddap_dataset_id="', dset_j, '"&erddap_variable_name="', name_j, '"&erddap_query_parameter="', q_param, '"&erddap_query_value="', q_val, '"'))
    
    meta_resp <- tryCatch({
      res <- GET(paste0(murl, mquery), timeout(15))
      content(res, as = "text", encoding = "UTF-8")
    }, error = function(e) "")
    
    inst_val <- ""
    units_val <- ""
    summary_val <- ""
    
    if (meta_resp != "") {
      meta_df <- tryCatch({
        read.csv(text = meta_resp, stringsAsFactors = FALSE, check.names = FALSE)
      }, error = function(e) NULL)
      
      if (!is.null(meta_df) && nrow(meta_df) >= 2) {
        meta_row <- meta_df[2, ]
        inst_val <- ifelse("institution" %in% names(meta_row), meta_row[["institution"]], "")
        units_val <- ifelse("units" %in% names(meta_row), meta_row[["units"]], "")
        
        sum_part <- ifelse("source_data_summary" %in% names(meta_row), meta_row[["source_data_summary"]], "")
        calc_part <- ifelse("additional_calculations" %in% names(meta_row), meta_row[["additional_calculations"]], "")
        summary_val <- trimws(paste(sum_part, calc_part))
      }
    }
    
    inst <- c(inst, inst_val)
    units <- c(units, units_val)
    summary <- c(summary, summary_val)
    
    # Handle right_axis and special series configurations
    thissername <- title_j
    if (!is.na(units_val) && units_val != "") {
      thissername <- paste0(thissername, " (", units_val, ")")
    }
    
    if (r_axis == "TRUE") {
      series[[thissername]] <- list(
        axis = "y2",
        showInRangeSelector = "true",
        independentTicks = "true"
      )
      options$y2label <- ylabel_j
    }
    
    if (name_j == "nph_area") {
      series[[thissername]] <- list(
        pointSize = "6",
        highlightCircleSize = "7"
      )
    }
  }
  
  # Sort all combined timestamps
  alltimes <- sort(unique(alltimes))
  
  # Build consolidated time series matrix
  data_list <- list()
  for (time_str in alltimes) {
    row_vec <- list(time_str)
    notnullrow <- FALSE
    
    for (j in 1:nrow(grp_rows)) {
      val <- ind[[j]][[time_str]]
      if (!is.null(val) && val != "NaN") {
        row_vec <- c(row_vec, list(val))
        notnullrow <- TRUE
      } else {
        row_vec <- c(row_vec, list(NULL))
      }
    }
    
    if (notnullrow) {
      data_list[[length(data_list) + 1]] <- row_vec
    }
  }
  
  # Construct dygraph options object
  labels <- c("Time (UTC)")
  for (j in seq_along(titles)) {
    lab <- titles[j]
    if (!is.na(units[j]) && units[j] != "") {
      lab <- paste0(lab, " (", units[j], ")")
    }
    labels <- c(labels, lab)
  }
  
  # Calculate dateWindow timestamps in milliseconds
  now_ms <- as.numeric(Sys.time()) * 1000
  whaleicontime <- as.numeric(as.POSIXct("2015-01-06 00:00:00", tz = "UTC")) * 1000
  
  options <- list(
    labels = I(labels),
    ylabel = grp_rows[1, "ylabel"],
    visibility = I(viz),
    colors = I(colors),
    strokeWidth = "3",
    pointSize = "4",
    highlightCircleSize = "5",
    xRangePad = "5",
    yRangePad = "5",
    legend = "always",
    series = series,
    labelsUTC = "true",
    labelsSeparateLines = "true",
    rightGap = "20",
    showRangeSelector = "true",
    connectSeparatedPoints = "true",
    labelsDiv = paste0("legdiv", k - 1),
    dateWindow = c(whaleicontime, now_ms),
    width = "700",
    height = "300"
  )
  
  # Assembly item object for group (using I() to force JSON arrays for length 1)
  item <- list(
    title    = grp_name,
    subtitle = grp_subtitle,
    titles   = I(titles),
    options  = options,
    dsets    = I(dsets),
    names    = I(names_vec),
    query    = I(queries),
    minT     = I(minT),
    maxT     = I(maxT),
    stats    = stats,
    inst     = I(inst),
    summary  = I(summary),
    units    = I(units),
    data     = data_list
  )
  
  groupitems[[k]] <- item
}

grouplist$items <- groupitems

# 2. Read annotations.csv if present
if (file.exists(annotationsfile)) {
  ann_df <- read.csv(annotationsfile, stringsAsFactors = FALSE, check.names = FALSE)
  
  ann_list <- lapply(seq_len(nrow(ann_df)), function(i) {
    as.list(ann_df[i, ])
  })
  grouplist$annotations <- ann_list
} else {
  grouplist$annotations <- list()
}

# Ensure directory exists before writing
outdir <- dirname(outfile)
if (!dir.exists(outdir)) {
  dir.create(outdir, recursive = TRUE)
}

# 3. Write out to JSON file
json_output <- toJSON(grouplist, auto_unbox = TRUE, pretty = TRUE, null = "null")
writeLines(json_output, outfile)

cat("Successfully generated", outfile, "\n")
