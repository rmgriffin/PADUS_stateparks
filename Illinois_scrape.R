# Setup -------------------------------------------------------------------
rm(list=ls()) # Clears workspace

# # Install/call libraries
# install.packages("renv") # Run if you have cloned repository and don't already have renv installed
# renv::restore() # Run once after cloning repository
# renv::install("package") # Run to install new packages
# renv::snapshot() # Run after installing new packages (need to be referenced in code to get written - run renv::snapshot(type = "all") to avoid this)
# renv::init() # Only run when the repository is first created, don't run on cloning an existing repository

# Data for this repository is at https://drive.google.com/file/d/1UiExxgfnedtRNp76G2G7-sPGcHTkZguD/view?usp=sharing

# Checks that required packages are installed, stops if not, loads them if they are
pkgs<-c("tidyverse","rvest","pdftools","fs","httr2","tesseract","magick")
missing<-pkgs[!vapply(pkgs, requireNamespace, logical(1), quietly=TRUE)]
if (length(missing)>0) {
  stop("Missing packages: ", paste(missing, collapse=", "),
       "\nRun renv::restore()")
}
invisible(lapply(pkgs, library, character.only=TRUE))
rm(pkgs, missing)


# Settings ----------------------------------------------------------------
source_page<-"https://dnr.illinois.gov/orep/land-and-water-reports.html"

base_dir<-"Data/illinois_land_water_reports"
dir_create(base_dir)
dir_create(file.path(base_dir,"pdf"))

if (dir.exists("Out")) unlink("Out", recursive = TRUE, force = TRUE) # Deletes output folder
dir.create("Out")
dir.create("Out/IL")


# Helper functions --------------------------------------------------------
clean_num<-function(x){
  
  x %>%
    str_replace_all(",", "") %>%
    str_replace_all("[^0-9]", "") %>%
    na_if("")
}

is_attendance_number<-function(x){
  
  str_detect(x,"^\\d{1,3}(,\\d{3})*$|^\\d+$")
}

collapse_tokens<-function(df){
  
  df %>%
    arrange(x) %>%
    pull(text) %>%
    str_c(collapse=" ") %>%
    str_squish()
}

get_page_words<-function(pdf){
  
  pages_text<-pdftools::pdf_data(pdf)
  
  purrr::imap(pages_text,function(pg,i){
    
    # ---- digital PDF ----
    if(nrow(pg)>0){
      return(pg)
    }
    
    # ---- scanned PDF ----
    img<-pdftools::pdf_render_page(pdf,page=i,dpi=400)
    img<-magick::image_read(img)
    
    tess<-tesseract::tesseract(options=list(tessedit_pageseg_mode=6))
    
    ocr<-tesseract::ocr_data(img,engine=tess)
    
    if(is.null(ocr) || nrow(ocr)==0){
      return(NULL)
    }
    
    # ---- case 1: modern tesseract (x,y columns exist) ----
    if(all(c("x","y","width","height") %in% names(ocr))){
      
      out<-tibble::tibble(
        text=ocr$word,
        x=as.numeric(ocr$x),
        y=as.numeric(ocr$y),
        width=as.numeric(ocr$width),
        height=as.numeric(ocr$height)
      )
      
    } else {
      
      # ---- case 2: bbox column ----
      bbox<-stringr::str_extract_all(ocr$bbox,"\\d+") |> 
        purrr::map(~as.numeric(.x))
      
      bbox<-do.call(rbind,bbox)
      
      out<-tibble::tibble(
        text=ocr$word,
        x=bbox[,1],
        y=bbox[,2],
        width=bbox[,3]-bbox[,1],
        height=bbox[,4]-bbox[,2]
      )
    }
    
    out |>
      dplyr::filter(!is.na(x),!is.na(y),text!="")
  })
}

is_scanned_pdf<-function(pdf){
  txt<-pdftools::pdf_text(pdf)
  mean(nchar(txt)) < 50
}

# Discover and download PDFs -----------------------------------------------------------
get_report_links<-function(url){
  
  page<-read_html(url)
  
  page %>%
    html_elements("a") %>%
    html_attr("href") %>%
    na.omit() %>%
    url_absolute(url) %>%
    keep(~str_detect(.x,"\\.pdf($|\\?)")) %>%
    tibble(url=.) %>%
    mutate(
      file_name=basename(url)
    ) %>%
    distinct()
}

download_reports<-function(tbl){
  
  tbl %>%
    mutate(dest=file.path("Data/illinois_land_water_reports/pdf", file_name)) %>%
    rowwise() %>%
    mutate(
      downloaded={
        if(!file.exists(dest)){
          
          tryCatch({
            
            resp<-httr2::request(url) %>%
              httr2::req_perform()
            
            writeBin(httr2::resp_body_raw(resp), dest)
            
            TRUE
            
          },error=function(e){
            
            message("Download failed: ", url)
            message("  ", conditionMessage(e))
            FALSE
          })
          
        } else TRUE
      }
    ) %>%
    ungroup()
}


# Extraction functions ---------------------------------------------------------
extract_pdf<-function(pdf_path){

  clean_num<-function(x){
    x|>
      stringr::str_replace_all(",","")|>
      as.numeric()
  }
  
  is_num_token<-function(x){
    stringr::str_detect(x,"^\\d{1,3}(,\\d{3})*$|^\\d+$")
  }
  
  is_total_text<-function(x){
    x<-stringr::str_squish(stringr::str_to_lower(x))
    stringr::str_detect(x,"^grand total")|
      stringr::str_detect(x,"\\btotal\\b")
  }
  
  is_header_text<-function(x){
    x<-stringr::str_squish(stringr::str_to_lower(x))
    
    stringr::str_detect(x,"statement of acreage")|
      stringr::str_detect(x,"department of natural resources")|
      stringr::str_detect(x,"state of illinois")|
      stringr::str_detect(x,"^county\\b")|
      stringr::str_detect(x,"^attendance\\b")|
      stringr::str_detect(x,"^apportionment\\b")|
      stringr::str_detect(x,"^highway\\b")|
      stringr::str_detect(x,"^property\\b")|
      stringr::str_detect(x,"^initial\\b")|
      stringr::str_detect(x,"^federal\\b")|
      stringr::str_detect(x,"^current acreage\\b")
  }
  
  pages<-get_page_words(pdf_path)
  pages<-pages[!sapply(pages,is.null)]
  page_text<-pdftools::pdf_text(pdf_path)|>
    stringr::str_to_lower()
  
  start_page<-5
  
  end_page<-which(stringr::str_detect(page_text,"(?m)^\\s*grand total"))[1]
  if(is.na(end_page)) end_page<-length(pages)
  
  pages<-pages[start_page:end_page]
  
  # ---- build global word table ----
  page_words<-purrr::imap(pages,function(pg,idx){
    
    words<-pg|>
      dplyr::mutate(
        page=as.integer(idx),
        text=stringr::str_trim(text),
        text_lc=stringr::str_to_lower(text)
      )|>
      dplyr::filter(text!="")|>
      dplyr::arrange(y,x)
    
    if(nrow(words)==0) return(NULL)
    
    words$row_id<-cumsum(c(TRUE,abs(diff(words$y))>3))
    
    words
  })
  page_words<-page_words[!sapply(page_words,is.null)]
  
  
  all_words<-dplyr::bind_rows(page_words)
  
  if(nrow(all_words)==0) return(tibble::tibble(site=character(),attendance=numeric()))
  
  # ---- global attendance column position ----
  attendance_x <-
    all_words |>
    dplyr::filter(stringr::str_detect(text_lc, "attend")) |>
    dplyr::pull(x) |>
    stats::median(na.rm = TRUE)
  
  if (is.na(attendance_x)) {
    stop("Could not locate attendance column from header tokens containing 'attend'.")
  }
  
  # ---- global first-column and county-left estimate ----
  all_row_tbl<-
    all_words|>
    dplyr::group_by(page,row_id)|>
    dplyr::summarise(
      min_x=min(x),
      row_text=stringr::str_squish(stringr::str_c(text,collapse=" ")),
      .groups="drop"
    )|>
    dplyr::arrange(page,row_id)
  
  candidate_rows<-
    all_row_tbl|>
    dplyr::filter(
      !is_total_text(row_text),
      !is_header_text(row_text)
    )
  
  if(nrow(candidate_rows)==0) return(tibble::tibble(site=character(),attendance=numeric()))
  
  first_col_left<-
    stats::quantile(candidate_rows$min_x,probs=0.02,na.rm=TRUE,names=FALSE)
  
  global_start_rows<-
    candidate_rows|>
    dplyr::filter(min_x<=first_col_left+8)
  
  county_left_candidates<-
    all_words|>
    dplyr::semi_join(global_start_rows,by=c("page","row_id"))|>
    dplyr::filter(
      x>first_col_left+120,
      x<attendance_x-450
    )|>
    dplyr::pull(x)
  
  if(length(county_left_candidates)==0){
    stop("Could not estimate county column boundary.")
  }
  
  county_left<-
    stats::quantile(county_left_candidates,probs=0.02,na.rm=TRUE,names=FALSE)
  
  first_col_right<-county_left-25
  
  # ---- page-by-page extraction ----
  out<-purrr::map_dfr(seq_along(page_words),function(i){
    
    words<-page_words[[i]]
    if(is.null(words)||nrow(words)==0) return(NULL)
    
    row_tbl<-
      words|>
      dplyr::group_by(row_id)|>
      dplyr::summarise(
        min_x=min(x),
        row_text=stringr::str_squish(stringr::str_c(text,collapse=" ")),
        .groups="drop"
      )|>
      dplyr::arrange(row_id)
    
    candidate_rows<-
      row_tbl|>
      dplyr::filter(
        !is_total_text(row_text),
        !is_header_text(row_text)
      )
    
    if(nrow(candidate_rows)==0) return(NULL)
    
    page_first_col_left<-
      stats::quantile(candidate_rows$min_x,probs=0.02,na.rm=TRUE,names=FALSE)
    
    start_rows<-
      candidate_rows|>
      dplyr::filter(min_x<=page_first_col_left+8)|>
      dplyr::pull(row_id)
    
    if(length(start_rows)==0) return(NULL)
    
    purrr::map_dfr(seq_along(start_rows),function(k){
      
      rec_start<-start_rows[k]
      
      rec_end<-if(k<length(start_rows)){
        start_rows[k+1]-1
      } else {
        max(row_tbl$row_id)
      }
      
      block_rows<-
        row_tbl|>
        dplyr::filter(row_id>=rec_start,row_id<=rec_end)
      
      # Trim block at the first total row inside it so totals do not
      # overwrite or contaminate the previous record.
      first_total_row<-
        block_rows|>
        dplyr::filter(is_total_text(row_text))|>
        dplyr::slice_head(n=1)|>
        dplyr::pull(row_id)
      
      if(length(first_total_row)>0){
        rec_end<-first_total_row-1
      }
      
      if(rec_end<rec_start) return(NULL)
      
      block<-
        words|>
        dplyr::filter(row_id>=rec_start,row_id<=rec_end)
      
      if(nrow(block)==0) return(NULL)
      
      start_row<-
        block|>
        dplyr::filter(row_id==rec_start)|>
        dplyr::arrange(x)
      
      start_text<-stringr::str_squish(stringr::str_c(start_row$text,collapse=" "))
      
      if(is_total_text(start_text)) return(NULL)
      if(is_header_text(start_text)) return(NULL)
      
      site_tokens<-
        start_row|>
        dplyr::filter(
          x<first_col_right,
          !stringr::str_detect(text,"^\\d")
        )
      
      if(nrow(site_tokens)==0) return(NULL)
      
      site<-
        site_tokens$text|>
        stringr::str_c(collapse=" ")|>
        stringr::str_squish()
      
      if(site=="") return(NULL)
      
      # attendance must be in the true far-right attendance column
      att_candidates<-
        block|>
        dplyr::filter(
          is_num_token(text),
          x>=attendance_x+2
        )|>
        dplyr::mutate(attendance=clean_num(text))|>
        dplyr::filter(!is.na(attendance))|>
        dplyr::arrange(y,x)
      
      if(nrow(att_candidates)==0) return(NULL)
      
      attendance<-att_candidates$attendance[nrow(att_candidates)]
      
      tibble::tibble(
        site=site,
        attendance=attendance
      )
    })
  })
  
  out |>
    dplyr::filter(
      !is.na(attendance),
      !is.na(site),
      site!="",
      !is_total_text(site),
      site != "Jubilee College" # There are two records (one a state park and one a state historic site - deleting both to avoid ambiguity) - could fix
    ) |>
    dplyr::distinct()
}

extract_pdf_scanned<-function(pdf_path){
  
  clean_num<-function(x){
    x|>
      stringr::str_replace_all(",","")|>
      stringr::str_replace_all("[^0-9]","")|>
      na_if("")|>
      as.numeric()
  }
  
  is_num_token<-function(x){
    stringr::str_detect(x,"^\\d{1,3}(,\\d{3})*$|^\\d+$")
  }
  
  is_total_text<-function(x){
    x<-stringr::str_squish(stringr::str_to_lower(x))
    stringr::str_detect(x,"^grand total")|
      stringr::str_detect(x,"\\btotal\\b")|
      stringr::str_detect(x,"total state park acreage")|
      stringr::str_detect(x,"total state conservation area acreage")|
      stringr::str_detect(x,"grand totals for all department properties")
  }
  
  is_header_text<-function(x){
    x<-stringr::str_squish(stringr::str_to_lower(x))
    
    stringr::str_detect(x,"statement of acreage")|
      stringr::str_detect(x,"department of natural resources")|
      stringr::str_detect(x,"state of illinois")|
      stringr::str_detect(x,"^county\\b")|
      stringr::str_detect(x,"^attendance\\b")|
      stringr::str_detect(x,"attendance figure")|
      stringr::str_detect(x,"^apportionment\\b")|
      stringr::str_detect(x,"^highway\\b")|
      stringr::str_detect(x,"^property\\b")|
      stringr::str_detect(x,"^initial\\b")|
      stringr::str_detect(x,"^federal\\b")|
      stringr::str_detect(x,"^current acreage\\b")|
      stringr::str_detect(x,"^fy\\b")
  }
  
  bad_site_text<-function(x){
    x2<-stringr::str_squish(x)
    xlc<-stringr::str_to_lower(x2)
    
    x2==""|
      is.na(x2)|
      stringr::str_detect(xlc,"^fy$")|
      stringr::str_detect(xlc,"^py$")|
      stringr::str_detect(xlc,"^a$")|
      stringr::str_detect(x2,"^\\d[\\d,]*$")|
      is_header_text(x2)|
      is_total_text(x2)
  }
  
  pages<-get_page_words(pdf_path)
  page_text<-pdftools::pdf_text(pdf_path)|>
    stringr::str_to_lower()
  
  start_page<-5
  end_page<-which(stringr::str_detect(page_text,"(?m)^\\s*grand total"))[1]
  if(is.na(end_page)) end_page<-length(pages)
  
  pages<-pages[start_page:end_page]
  pages<-pages[!sapply(pages,is.null)]
  
  page_words<-purrr::imap(pages,function(pg,idx){
    
    words<-pg|>
      dplyr::mutate(
        page=as.integer(idx),
        text=stringr::str_trim(text),
        text_lc=stringr::str_to_lower(text)
      )|>
      dplyr::filter(text!="")|>
      dplyr::arrange(y,x)
    
    if(nrow(words)==0) return(NULL)
    
    # OCR rows
    words$row_id<-cumsum(c(TRUE,abs(diff(words$y))>6))
    
    words
  })
  
  page_words<-page_words[!sapply(page_words,is.null)]
  all_words<-dplyr::bind_rows(page_words)
  
  if(nrow(all_words)==0){
    return(tibble::tibble(site=character(),attendance=numeric()))
  }
  
  # global attendance column from header
  attendance_x<-
    all_words|>
    dplyr::filter(stringr::str_detect(text_lc,"attend"))|>
    dplyr::pull(x)|>
    stats::median(na.rm=TRUE)
  
  if(is.na(attendance_x)){
    stop("Could not locate attendance column from header tokens containing 'attend'.")
  }
  
  # global county column from header
  county_x<-
    all_words|>
    dplyr::filter(stringr::str_detect(text_lc,"^county$|\\bcounty\\b"))|>
    dplyr::pull(x)|>
    stats::median(na.rm=TRUE)
  
  if(is.na(county_x)){
    county_x<-attendance_x-500
  }
  
  out<-purrr::map_dfr(seq_along(page_words),function(i){
    
    words<-page_words[[i]]
    if(is.null(words) || nrow(words)==0) return(NULL)
    
    row_tbl<-
      words|>
      dplyr::group_by(row_id)|>
      dplyr::summarise(
        row_y=stats::median(y),
        min_x=min(x),
        row_text=stringr::str_squish(stringr::str_c(text,collapse=" ")),
        .groups="drop"
      )|>
      dplyr::arrange(row_id)
    
    candidate_rows<-
      row_tbl|>
      dplyr::filter(
        !is_header_text(row_text),
        !is_total_text(row_text)
      )
    
    if(nrow(candidate_rows)==0) return(NULL)
    
    page_first_col_left<-
      stats::quantile(candidate_rows$min_x,probs=0.02,na.rm=TRUE,names=FALSE)
    
    # true record starts should be the left-aligned first-column rows
    start_rows<-
      candidate_rows|>
      dplyr::filter(min_x<=page_first_col_left+20)|>
      dplyr::pull(row_id)
    
    # remove obvious split duplicates from OCR jitter
    if(length(start_rows)>1){
      keep<-c(TRUE,diff(start_rows)>1)
      start_rows<-start_rows[keep]
    }
    
    if(length(start_rows)==0) return(NULL)
    
    purrr::map_dfr(seq_along(start_rows),function(k){
      
      rec_start<-start_rows[k]
      rec_end<-if(k<length(start_rows)) start_rows[k+1]-1 else max(row_tbl$row_id)
      
      block_rows<-
        row_tbl|>
        dplyr::filter(row_id>=rec_start,row_id<=rec_end)
      
      first_total_row<-
        block_rows|>
        dplyr::filter(is_total_text(row_text))|>
        dplyr::slice_head(n=1)|>
        dplyr::pull(row_id)
      
      if(length(first_total_row)>0){
        rec_end<-first_total_row-1
      }
      
      if(rec_end<rec_start) return(NULL)
      
      block<-
        words|>
        dplyr::filter(row_id>=rec_start,row_id<=rec_end)
      
      if(nrow(block)==0) return(NULL)
      
      start_row<-
        block|>
        dplyr::filter(row_id==rec_start)|>
        dplyr::arrange(x)
      
      start_text<-stringr::str_squish(stringr::str_c(start_row$text,collapse=" "))
      if(is_header_text(start_text) || is_total_text(start_text)) return(NULL)
      
      # site = all nonnumeric tokens left of County column
      site_tokens<-
        start_row|>
        dplyr::filter(
          x < county_x-10,
          !stringr::str_detect(text,"^\\d")
        )|>
        dplyr::arrange(x)
      
      # fallback if County header was OCR'd badly on this file
      if(nrow(site_tokens)==0){
        site_tokens<-
          start_row|>
          dplyr::filter(
            x < attendance_x-300,
            !stringr::str_detect(text,"^\\d")
          )|>
          dplyr::arrange(x)
      }
      
      if(nrow(site_tokens)==0) return(NULL)
      
      site<-
        site_tokens$text|>
        stringr::str_c(collapse=" ")|>
        stringr::str_squish()
      
      if(bad_site_text(site)) return(NULL)
      
      # attendance = far-right numeric in this record block
      att_candidates<-
        block|>
        dplyr::filter(
          x>=attendance_x-20,
          is_num_token(text)
        )|>
        dplyr::mutate(attendance=clean_num(text))|>
        dplyr::filter(!is.na(attendance))|>
        dplyr::arrange(y,x)
      
      if(nrow(att_candidates)==0) return(NULL)
      
      attendance<-att_candidates$attendance[nrow(att_candidates)]
      
      if(is.na(attendance)) return(NULL)
      
      tibble::tibble(
        site=site,
        attendance=attendance
      )
    })
  })
  
  out|>
    dplyr::filter(
      !is.na(site),
      !is.na(attendance),
      !bad_site_text(site)
    )|>
    dplyr::distinct()
}


# Write CSV ---------------------------------------------------------------
write_csv_same_name<-function(df,pdf){
  
  name<-tools::file_path_sans_ext(basename(pdf))
  
  out<-file.path("Out/IL", paste0(name,".csv"))
  
  write_csv(df,out)
  
  out
}


# Run ---------------------------------------------------------------------
links<-get_report_links(source_page)

links<-links %>% 
  filter(!file_name %in% c("boatdigest.pdf","affirmative_action_plan.pdf"))

downloads<-download_reports(links)

pdf_files<-
  downloads %>%
  filter(downloaded) %>%
  pull(dest)

pdf_files<-pdf_files[-2] # Removing because this file's structure is significantly different from the rest and very hard to parse
pdf_files<-pdf_files[-c(6,18,19)] # Removing because these are scanned and can't find a way to extract correctly with that

results<-map_dfr(pdf_files,function(pdf){ # 17 needs a very little manual editing to delete a few added junk rows
    
    message("Processing ",basename(pdf))
    
  df<-if(is_scanned_pdf(pdf)){
    extract_pdf_scanned(pdf)
  } else {
    extract_pdf(pdf)
  }
    
    out<-write_csv_same_name(df,pdf)
    
    tibble(
      pdf=basename(pdf),
      csv=basename(out),
      rows=nrow(df)
    )
  })

#print(results)

# Combine CSVs ------------------------------------------------------------

csv_files <- fs::dir_ls("Out/IL", regexp="\\.csv$")

attendance_long <-
  purrr::map_dfr(csv_files, function(f){
    
    readr::read_csv(f, show_col_types=FALSE) |>
      dplyr::mutate(
        source_file = tools::file_path_sans_ext(basename(f))
      )
    
  })

year_lookup<-tibble::tribble(
  ~source_file, ~year,
  "00000941", 2023,
  "00000925", 2021,
  "00000921", 2020,
  "00000912", 2019,
  "00000908", 2018,
  "00000902", 2016,
  "00000901", 2015,
  "00000837", 2014,
  "00000828", 2013,
  "00000827", 2012,
  "00000822", 2011,
  "00000821", 2010,
  "00000787", 2009,
  "00000761", 2008,
  "00000640", 2007,
  "00000639", 2006
)

attendance_long <- attendance_long %>%
  dplyr::left_join(year_lookup, by="source_file")

attendance_long$source_file<-NULL
attendance_long$b<-paste0("1/1/",attendance_long$year)
attendance_long$e<-paste0("12/31/",attendance_long$year)

attendance_long$state<-"IL"

attendance_long<-attendance_long|>
  dplyr::select(site,attendance,state,b,e)

# write combined dataset
readr::write_csv(attendance_long, "Out/IL/illinois_attendance_long.csv")
