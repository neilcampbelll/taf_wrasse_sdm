## HABITAT DATA

# NOTE - THIS SCRIPT TAKES A LONG TIME TO RUN. 
# I SUGGEST YOU USE IT ONCE AND THEN HASH IT OUT

#Before - Nothing
#After - MSFB Broad Benthic Habitat Data

## get the data from ICES
## (it's originally an EMODnet layer, 2023 but we've hosted it here)

download_large_file_curl_A <- function(url, dest_file) {
  
  # Create directory if it doesn't exist
  dir.create(dirname(dest_file), showWarnings = FALSE, recursive = TRUE)
  
  tryCatch({
    message(paste("Starting download of", basename(dest_file), "to", dest_file))
    curl_download(url, dest_file, quiet = FALSE, mode = "wb", # quiet = FALSE provides basic curl progress
                  handle = new_handle(
                    # CURLOPT_TIMEOUT: total time for the transfer to complete
                    timeout_ms = 7200000, # 2 hours in milliseconds (7200 * 1000)
                    # CURLOPT_CONNECTTIMEOUT: time to establish connection
                    connecttimeout_ms = 30000, # 30 seconds
                    # CURLOPT_LOW_SPEED_TIME / LOW_SPEED_LIMIT: if speed drops below X for Y seconds, abort
                    low_speed_time = 60, # 60 seconds
                    low_speed_limit = 1024 # 1 KB/s, if speed is below 1KB/s for 60s, it will timeout
                  )
    )
    message("\nDownload complete!") # Newline after progress
  }, error = function(e) {
    message(paste("Download failed:", e$message))
    # Only try to remove if the file exists and is potentially partial
    if (file.exists(dest_file)) {
      file.remove(dest_file)
    }
    stop(e) # Re-throw the error
  })
  
  return(invisible(dest_file))
}

if(!file.exists("data/hab_and_bathy_layers.zip")){
# file size: 1227481372 bytes (~1.2 GB)
shared_link <- "https://icesit.sharepoint.com/:u:/g/Efh5rtBiIhFPsnFcWXH-khYBKRBEHkEDjLHh4OFrMX68Vw?e=cubybi&download=1"
local_path <- "data/hab_and_bathy_layers.zip"

download_large_file_curl_A(shared_link, local_path)

unzip(zipfile = paste0("data/hab_and_bathy_layers.zip"), overwrite = TRUE)

}

# Load the habitat layer
eusm <- readRDS("eusm.rds")
eusm <- eusm %>% st_transform(4326) %>%
  st_make_valid()

  save_to_geopackage(eusm, "MSFD_Habitats", GEOPACKAGE_PATH)
  
  rm(shared_link, local_path, eusm)
  