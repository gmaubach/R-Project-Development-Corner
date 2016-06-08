
# [] Operator
foo <- data.frame(a=1:4)
foo

foo['b'] <- foo['a']*3
foo


# [[]] Operator
foo <- data.frame(a=1:4)
foo

foo[['b']] <- foo[['a']]*3
foo

# Wo ist der Unterschied zwischen [] und [[]]?

for (year in 2011:2015) {
     nmK <- paste0("Kunde_real_", year)
     nmU <- paste0("Umsatz_", year)
     # cat('Creating',nmK,'from',nmU,'\n')
     Kunden[[ nmK ]] <- ifelse( Kunden[[ nmU ]] <= 0, 1, 2)
     Kunden[[ nmK ]] <- factor( Kunden[[ nmK ]],
                                levels=c(1,2),
                                labels= c("NICHT kaufend", "kaufend")
    )
}



do_test <- FALSE


t_create_variable <- function(dataset,
                              variables,
                              prefix = "",
                              suffix = "",
                              function_definition = c(1)) {
  
  for (variable in variables) {
    new_var <- paste0(prefix, variable, suffix)
    # cat("n", "\n", "Creating ", new_var, " from ", variable, "\n")
    dataset [[ new_var ]] <- function_definition
  }
  
  return(dataset)
}

#-------------------------------------------------------------------------------

test <- function(do_test = FALSE) {
  
  cat("\n", "\n", "Test function t_count_na()", "\n", "\n")
  
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
  
  cat("\n", "\n", "Example dataset before function call", "\n", "\n")
  print(ds_example)
  
  cat("\n", "\n", "Function call", "\n", "\n")
  ds_example <- t_create_variable(dataset = ds_example,
                                  variables = c("mmul", "mmus"),
                                  prefix = "t_",
                                  suffix = "_new",
                                  function_definition = "1")
  
  cat("\n", "\n", "Example dataset after function call", "\n", "\n")
  print(ds_example)
}

do_test = TRUE

test(do_test = do_test)

# EOF .
