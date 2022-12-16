xport.label8 <- function(
                          # required arguments
                          
                          varName,
                          varLabel,
                          varNum,
                          labLen,
                          namLen,
                          totLen
                          )
{
  


  ## force variable name into format permitted by SAS.  Starts with
  ## alpha, alpha, numbers, and underscore permitted.  R's
  ## make.names() function almost does what we want, but allows
  ## periods instead of underscores, so just use make.names() and
  ## replace periods with underscores, :-)
  varName <- gsub("\\.","_", make.names(varName))

  ## Note that the variable name field in the xport file only permits
  ## 8 characters, so names will be truncated.
  
  
  name_label <- paste0(toupper(as.character(varName)), as.character(varLabel))
  .C("fill_label8",
     varName = as.character(name_label),
     varNum = as.integer(varNum),              # Bool: Is this a character varible
     labLen   = as.integer(labLen),           # LENGTH OF VARIABLE IN OBSERVATION
     namLen  = as.integer(namLen),              # VARNUM
     totLen  = as.integer(totLen),  # NAME OF VARIABLE
     
     PACKAGE="SASxport"
     )

    .Call("getRawBuffer", PACKAGE="SASxport")
    # print('something')
}

