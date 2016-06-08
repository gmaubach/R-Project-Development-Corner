
# Example dataset
gene <- c("ENSG00000208234","ENSG00000199674","ENSG00000221622","ENSG00000207604", 
  "ENSG00000207431","ENSG00000221312","ENSG00134940305","ENSG00394039490",
  "ENSG09943004048")
hsap <- c(0,0,0, 0, 0, 0, 1,1, 1)
mmul <- c(NA,2 ,3, NA, 2, 1 , NA,2, NA)
mmus <- c(NA,2 ,NA, NA, NA, 2 , NA,3, 1)
rnor <- c(NA,2 ,NA, 1 , NA, 3 , NA,NA, 2)
cfam <- c(NA,2,NA, 2, 1, 2, 2,NA, NA)

ds_example <- data.frame(gene, hsap, mmul, mmus, rnor, cfam)
ds_example$gene <- as.character(ds_example$gene)

t_count_na <- function(dataset,
                       variables = "ALL"
                       ) { #1 open
  na_count <- NULL
  if (variables == "ALL") { #2 open
    na_count <- apply(dataset, 2, is.na)
  } #2 close
  else { #3 open
    # ds_na <- NA
    for (var %in% variables) { #4 open
      new_name <- paste0("na_", var)
      ds_na[[ new_name ]] <- apply(dataset, 2, is.na)
    } #4 close
    na_count <- apply(ds_na, 2, rowSums)
  } # 3 close
} #1 close

t_count_na <- function(dataset,
                       variables = "all_n_sum") {
  if (variables != "all_n_sum" | variables != "sum") {
    print("Param Error: param 'variables' must be either 'all_n_sum' oder 'sum'!")
  }
  
  if (variables == "sum") {
    ds_na <- apply(dataset, 2, is.na)
  }
  else {
    for (var in variables) {
      new_name <- paste0("na_", var)
      ds_na[[ new_name ]] <- apply(dataset, 2, is.na)
    }
    ds_na[[ "na_count" ]] <- apply(ds_na, 2, rowSums)
  }
  
  return(ds_na)
}

t_count_na <- function(dataset,
                       variables = "all")
  # credit: http://stackoverflow.com/questions/4862178/remove-rows-with-nas-in-data-frame
  {
  ds_na <- data.frame()
  # if variables = "all" create character vector of variable names
  if (variables == "all") {
    variable_list <- dimnames(dataset)[[ 2 ]] 
  }
  
  for (var in variable_list) {
    new_name <- paste0("na_", var)
    ds_na[[ new_name ]] <- as.data.frame(is.na(dataset[[ var ]]))
  }
  # ds_na[[ "na_count" ]] <- rowSums(ds_na)
  
  return(ds_na)
}



for (year in 2011:2015) {
  nmK <- paste0("Kunde_real_", year)
  nmU <- paste0("Umsatz_", year)
  # cat('Creating',nmK,'from',nmU,'\n')
  Kunden[[ nmK ]] <- ifelse( Kunden[[ nmU ]] <= 0, 1, 2)
  Kunden[[ nmK ]] <- factor( Kunden[[ nmK ]],
                             levels=c(1,2),
                             labels= c("NICHT kaufend", "kaufend")
                             
                             

z.na.keep = function(df, col=NULL, n=0){
  if (!is.null(col)) {
    df.temp = df[,col]
  } else {
    df.temp = df
  }
  
  if (length(n)==1){
    if (n==0) {
      # simply call complete.cases which might be faster
      result = df[complete.cases(df.temp),]
    } else {
      # credit: http://stackoverflow.com/a/30461945/2292993
      log <- apply(df.temp, 2, is.na)
      logindex <- apply(log, 1, function(x) sum(x) == n)
      result = df[logindex, ]
    }
  }
  
  if (length(n)==2){
    min = n[1]; max = n[2]
    log <- apply(df.temp, 2, is.na)
    logindex <- apply(log, 1, function(x) {sum(x) >= min && sum(x) <= max})
    result = df[logindex, ]
  }
  
  return(result)
}
#-------------------------------------------------------------------------------

# Example dataset
gene <- c("ENSG00000208234","ENSG00000199674","ENSG00000221622","ENSG00000207604", 
          "ENSG00000207431","ENSG00000221312","ENSG00134940305","ENSG00394039490",
          "ENSG09943004048")
hsap <- c(0,0,0, 0, 0, 0, 1,1, 1)
mmul <- c(NA,2 ,3, NA, 2, 1 , NA,2, NA)
mmus <- c(NA,2 ,NA, NA, NA, 2 , NA,3, 1)
rnor <- c(NA,2 ,NA, 1 , NA, 3 , NA,NA, 2)
cfam <- c(NA,2,NA, 2, 1, 2, 2,NA, NA)

ds_example <- data.frame(gene, hsap, mmul, mmus, rnor, cfam)
ds_example$gene <- as.character(ds_example$gene)


t_count_na <- function(dataset,
                       variables = "all") {
  if (identical(variables, "all")) {
    variable_list <- names(dataset)
  }  else {
    variable_list <- variables
  }  
  ds_na_count <- apply(dataset[,variable_list], 1, function(x) sum(is.na(x)))
}

test <- t_count_na(dataset = ds_example, variables = c("mmul", "mmus"))

table(ds_example)
class(test)
str(test)
table(test, useNA = "always")




