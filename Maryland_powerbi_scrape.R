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
pkgs<-c("tidyverse","sf","httr","jsonlite")
missing<-pkgs[!vapply(pkgs, requireNamespace, logical(1), quietly=TRUE)]
if (length(missing)>0) {
  stop("Missing packages: ", paste(missing, collapse=", "),
       "\nRun renv::restore()")
}
invisible(lapply(pkgs, library, character.only=TRUE))
rm(pkgs, missing)

# ----------------------------
# 0) CONFIG
# ----------------------------
curl_file<-"curl_assateague.txt"
base_park<-"Assateague"
log_file<-"maryland_parks_visitation_panel_log.txt"

parks<-c(
  "Assateague","Big Run","Bill Burton, Dorchester","Bill Burton, Talbot",
  "Bohemia River","Bridgetown Ponds","Calvert Cliffs","Casselman",
  "Cedarville","Chapel Point","Chapman","Cunningham Falls","Cypress Branch",
  "Dans Mountain","Deep Creek Lake NRMA","Deep Creek Lake SP","Elk Neck",
  "Fair Hill","Franklin Point","Ft. Frederick","Gambrill","Gathland",
  "Greenbrier","Greenwell","Gunpowder Falls","Hallowing Point",
  "Harriet Tubman","Hart Miller","Herrington Manor","Janes Island",
  "Jennings Randolph","Martinak","Merkle","Monocacy","Morgan Run",
  "New Germany","Newtowne","North Point","Palmer","Patapsco Valley",
  "Patuxent NRMA","Patuxent River SP","Pocomoke","Point Lookout","Rocks",
  "Rocky Gap","Rosaryville","Sandy Point","Sang Run","Sassafras",
  "Seneca Creek","Severn Run","Sideling Hill Creek","Smallwood",
  "Soldiers Delight","South Mountain","St. Clement's","St. Mary's",
  "Susquehanna","Swallow Falls","Tawes Garden","TCB Rail Trail","Tuckahoe",
  "Washington Monument","Western MD Rail Trail","Wolf Den Run","Woodmont",
  "Wye Island","Wye Oak","Youghiogheny","Zekiah Swamp"
)

# ----------------------------
# 1) READ + PARSE CURL
# ----------------------------
curl_text<-paste(readLines(curl_file, warn=FALSE), collapse=" ")

url<-str_match(curl_text, "curl '([^']+)'")[,2]
if(is.na(url)) stop("Could not parse URL from curl.txt")

h<-str_match_all(curl_text, "-H '([^']+)'")[[1]]
headers_all<-list()
if(nrow(h)>0){
  for(i in seq_len(nrow(h))){
    kv<-str_split_fixed(h[i,2], ":\\s*", 2)
    if(ncol(kv)==2) headers_all[[kv[1,1]]]<-kv[1,2]
  }
}

# Extract --data-raw payload. Prefer $'...' form; fallback to '...'
body_raw<-str_match(curl_text, "--data-raw \\$'((?:[^'\\\\]|\\\\.)*)'")[,2]
if(is.na(body_raw)){
  body_raw<-str_match(curl_text, "--data-raw '([^']*)'")[,2]
}
if(is.na(body_raw)) stop("Could not parse --data-raw payload from curl.txt")

# Unescape bash ANSI-C style sequences used by $'...'
body_raw<-body_raw |>
  str_replace_all("\\\\n", "\n") |>
  str_replace_all("\\\\r", "\r") |>
  str_replace_all("\\\\t", "\t") |>
  str_replace_all("\\\\\"", "\"") |>
  str_replace_all("\\\\\\\\", "\\\\") |>
  str_replace_all("\\\\'", "'")

# Parse JSON payload
body_json<-fromJSON(body_raw, simplifyVector=FALSE)

# ----------------------------
# 2) MINIMAL, STABLE HEADERS
#    (Drop volatile IDs that often break replay)
# ----------------------------
keep<-c("Accept","Content-Type","Origin","Referer","X-PowerBI-ResourceKey","Accept-Language")
headers<-headers_all[names(headers_all) %in% keep]
headers<-unlist(headers)

# Ensure required headers exist
if(is.null(headers[["Content-Type"]])) headers[["Content-Type"]]<-"application/json;charset=UTF-8"
if(is.null(headers[["Accept"]])) headers[["Accept"]]<-"application/json, text/plain, */*"
if(is.null(headers[["Origin"]])) headers[["Origin"]]<-"https://app.powerbi.com"
if(is.null(headers[["Referer"]])) headers[["Referer"]]<-"https://app.powerbi.com/"

# ----------------------------
# 3) LOCATE PARK FILTER IN JSON (robustly)
# ----------------------------
where_path<-body_json$queries[[1]]$Query$Commands[[1]]$SemanticQueryDataShapeCommand$Query$Where
if(is.null(where_path)) stop("Could not find Query.Where in payload JSON")

park_where_idx<-NA_integer_
for(i in seq_along(where_path)){
  cand<-where_path[[i]]$Condition$In$Expressions[[1]]$Column$Property
  if(!is.null(cand) && identical(cand, "Park Name")){
    park_where_idx<-i
    break
  }
}
if(is.na(park_where_idx)) stop("Could not locate Park Name filter inside Query.Where")

# Helper to set park value correctly (must be a literal like \"'Assateague'\")
set_park_in_body<-function(body_obj, park){
  body2<-body_obj
  body2$queries[[1]]$Query$Commands[[1]]$SemanticQueryDataShapeCommand$Query$Where[[park_where_idx]]$Condition$In$Values<-list(
    list(list(Literal=list(Value=paste0("'", park, "'"))))
  )
  body2
}

# ----------------------------
# 4) RUN QUERY + DECODE MATRIX
# ----------------------------
post_body<-function(body_obj){
  payload<-toJSON(body_obj, auto_unbox=TRUE)
  res<-POST(
    url,
    httr::add_headers(.headers=headers),
    body=payload,
    encode="raw"
  )
  list(res=res, text=content(res, "text", encoding="UTF-8"))
}

decode_month_year_matrix<-function(j){
  # Years
  years_ms<-map_dbl(j$results[[1]]$result$data$dsr$DS[[1]]$SH[[1]]$DM1, "G1")
  years<-as.integer(format(as.POSIXct(years_ms/1000, origin="1970-01-01", tz="UTC"), "%Y"))
  
  # Month labels (some responses store ValueDicts under PH[[1]]; yours is PH[[1]]$ValueDicts)
  months<-j$results[[1]]$result$data$dsr$DS[[1]]$PH[[1]]$ValueDicts$D0
  if(is.null(months)){
    # fallback shape (rare)
    months<-j$results[[1]]$result$data$dsr$DS[[1]]$ValueDicts$D0
  }
  if(is.null(months)) stop("Could not find month dictionary D0 in response JSON")
  
  rows<-j$results[[1]]$result$data$dsr$DS[[1]]$PH[[1]]$DM0
  if(is.null(rows)) stop("Could not find DM0 rows in response JSON")
  
  out<-vector("list", length(rows))
  for(i in seq_along(rows)){
    month<-months[[rows[[i]]$G0 + 1]]
    xs<-rows[[i]]$X
    
    vals<-rep(NA_real_, length(years))
    idx<-0L
    for(k in seq_along(xs)){
      if(!is.null(xs[[k]]$I)) idx<-as.integer(xs[[k]]$I)
      vals[idx + 1L]<-as.numeric(xs[[k]]$M0)
      idx<-idx + 1L
    }
    
    out[[i]]<-data.frame(year=years, month=month, visitors=vals)
  }
  
  bind_rows(out)
}

# ----------------------------
# 5) SANITY CHECK: run base park once and show failure loudly
# ----------------------------
cat("", file=log_file)  # reset log

base_body<-set_park_in_body(body_json, base_park)
p<-post_body(base_body)

if(http_error(p$res)){
  msg<-paste0("BASE REQUEST FAILED: HTTP ", status_code(p$res), "\n", substr(p$text, 1, 300), "\n")
  write(msg, file=log_file, append=TRUE)
  stop(msg)
}

# Parse JSON; if this fails, you'll see it (not silently swallowed)
j_base<-fromJSON(p$text, simplifyVector=FALSE)

df_base<-decode_month_year_matrix(j_base)
df_base$park<-base_park

if(nrow(df_base)==0) stop("Base park decoded to 0 rows. Response shape unexpected.")
write(paste0("Base park success: ", base_park, " rows=", nrow(df_base)), file=log_file, append=TRUE)

# ----------------------------
# 6) LOOP PARKS WITH LOGGING (no silent failure)
# ----------------------------
one_park<-function(park){
  body_p<-set_park_in_body(body_json, park)
  pr<-post_body(body_p)
  
  if(http_error(pr$res)){
    write(paste0("FAIL ", park, " HTTP ", status_code(pr$res), " :: ", substr(pr$text,1,200)),
          file=log_file, append=TRUE)
    return(NULL)
  }
  
  j<-tryCatch(fromJSON(pr$text, simplifyVector=FALSE),
              error=function(e){
                write(paste0("FAIL ", park, " JSON parse :: ", conditionMessage(e)),
                      file=log_file, append=TRUE)
                NULL
              })
  if(is.null(j)) return(NULL)
  
  df<-tryCatch(decode_month_year_matrix(j),
               error=function(e){
                 write(paste0("FAIL ", park, " decode :: ", conditionMessage(e)),
                       file=log_file, append=TRUE)
                 NULL
               })
  if(is.null(df)) return(NULL)
  
  df$park<-park
  write(paste0("OK ", park, " rows=", nrow(df)), file=log_file, append=TRUE)
  df
}

panel_list<-vector("list", length(parks))
for(i in seq_along(parks)){
  cat("Park ", i, "/", length(parks), ": ", parks[[i]], "\n", sep="")
  panel_list[[i]]<-one_park(parks[[i]])
}

panel<-bind_rows(panel_list)

panel_wide<-panel %>% 
  pivot_wider(id_cols = c(year,park),names_from = month,values_from = visitors)

panel_ann_sum<-panel %>% 
  group_by(park,year) %>% 
  summarise(totvisits = sum(visitors)) %>% 
  drop_na()

if (dir.exists("Out")) unlink("Out", recursive = TRUE, force = TRUE) # Deletes output folder
dir.create("Out")
write.csv(panel_wide, "Out/MD_wide.csv", row.names=FALSE)
write.csv(panel_ann_sum, "Out/MD_ann_sum.csv", row.names=FALSE)

print(nrow(panel))
print(head(panel))
cat("Wrote: ", out_csv, "\nLog: ", log_file, "\n", sep="")