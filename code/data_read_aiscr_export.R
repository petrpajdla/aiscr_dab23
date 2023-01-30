# read data exported from backend and return a list object
# export from backend > https://backend.aiscr.cz

#' Read AMCR data
#' 
#' Reads data exported from AMCR backend saved in folder ./data/aiscr/.
#'
#' @return A list with tibbles.
#' @export
#'
#' @examples
read_amcr <- function(x = NA) {
  p <- list.files(here::here("data/aiscr"))
  p <- p[stringr::str_detect(p, "^export")]
  
  f <- dplyr::tibble(path = p) |> 
    dplyr::mutate(datestamp = stringr::str_extract(path, pattern = "\\d{4}-\\d{2}-\\d{2}"),
                  datestamp = lubridate::ymd(datestamp),
                  file = stringr::str_remove(path, "export_\\d{4}-\\d{2}-\\d{2}_"),
                  table = stringr::str_remove(file, "\\..*$")) |> 
    dplyr::group_by(file) |> 
    dplyr::arrange(desc(datestamp)) |> 
    dplyr::filter(dplyr::row_number() == 1) %>% 
    dplyr::arrange(table)
  
  if (!all(is.na(x))) {
    message("Reading tables: ", paste(x, collapse = ", "))
    f <- f %>% 
      dplyr::filter(table %in% x)
  } else {
    message("Reading all available tables: ", 
            paste(f$table, collapse = ", "))
  }
  
  res <- vector("list", nrow(f)) |> 
    setNames(nm = f$table)
  
  res$meta <- f[, c("table", "datestamp")]
  
  pbar <- txtProgressBar(min = 0, max = nrow(f), style = 3)
  
  for (i in 1:nrow(f)) {
    
    res[[i]] <- data.table::fread(here::here("data/aiscr", f$path[i]), 
                                  sep = "#", 
                                  encoding = "UTF-8", 
                                  quote = "", 
                                  na.strings = "") |>
      dplyr::tibble()
    
    setTxtProgressBar(pbar, i)
  }
  
  close(pbar)
  return(res)
  
}

# read data
# aiscr <- read_input()


