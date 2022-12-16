#' @importFrom Hmisc label
#' @export
write.xport <- function(...,
                        list=base::list(),
                        file = stop("'file' must be specified"),
                        verbose=FALSE,
                        sasVer="7.00",
                        osType,
                        cDate=Sys.time(),
                        formats=NULL,
                        autogen.formats=TRUE
                        )
  {
  library(stringr)

    ## Handle verbose option ##
  
    oldDebug <- getOption("DEBUG")
    if(verbose)
      {
        options(DEBUG=TRUE)
      }
    else
      {
        options(DEBUG=FALSE)
      }
    on.exit( options(DEBUG=oldDebug) )

    ## Handle osType default value ##
    if(missing(osType))
      osType <- paste("R ", R.version$major, ".", R.version$minor, sep="")

    ## Handle '...' ##
    dotList <- base::list(...)
    
    if (typeof(dotList[[1]])=='list')
      dotList <- unlist(dotList, recursive = F)
    
    dotNames <- names(dotList)
    if(is.null(dotNames)) dotNames <- rep("", length(dotList))

    if(length(dotList)>0)
      {
        ## Get data frame names from ... in function call, but don't
        ## clobber any explicitly provided names
        mc <- match.call()
        mc$file <- NULL
        mc$verbose <- NULL
        mc$sasVer <- NULL
        mc$osType <- NULL
        mc$cDate <- NULL
        mc$list <- NULL
        mc$autogen.formats <- NULL
        mc[[1]] <- NULL
        # note we *do not* mask off format argument so it will get
        # magically included if present.

        mc <- as.character(mc)

        badNames <- which(is.na(dotNames) | dotNames<="")
        dotNames[badNames] <- mc[badNames]
      }

    ## Join explicit 'list' argument to '...' arguments ##
    listNames <- names(list)
    if(is.null(listNames))
      listNames <- rep("", length(list))
    dfList  <- c(dotList, list)
    dfNames <- c(dotNames, listNames)

    ## check for and handle <NA> or empty names ##
    badNames <- which(is.na(dfNames) | dfNames<="")
    if(length(badNames)>0)
      {
        warning("Replacing missing or invalid dataset names")
        dfNames[badNames] = paste("DATA",badNames,sep="")
      }

    ## put revised names back ##
    names(dfList) <- dfNames

    #######
    ##
    scat("Ensure all objects to be stored are data.frames...\n")
    not.df <- which(!sapply(dfList,is.data.frame))
    
    
    if (length(not.df)){
          if(length(not.df)==1)
        stop(paste("'", dfNames[not.df], "'"),
             " is not a data.frame object.")
      else
        stop(paste("'", dfNames[not.df], "'", sep="", collapse=", "),
             " are not data.frame objects.")
    }
    ##
    #######


    #######
    ##
    scat("Ensure object names are valid and unique...\n")
    names(dfList) <- dfNames <- makeSASNames(dfNames)
    ##
    #######

    #######
    ## Generate formats for factor variables
    if(autogen.formats)
      {
        dfList <- make.formats(dfList, formats=formats)
        dfNames <- names(dfList)
        formats <- dfList$FORMATS
      }

    if(is.null(formats) || length(formats)<1 || nrow(formats)<1)
      {
        formats <- NULL
        dfList$FORMATS <- NULL
        dfNames <- names(dfList)
      }

    #######
    scat("opening file ...")
    if (is.character(file))
      if (file == "")
        file <- stdout()
      else {
        file <- file(description=file, open="wb")
        on.exit(close(file))
      }
    scat("Done")

    if(file==stdout())
      out <- function(...)
        {
          cat("ASCII: ", rawToDisplay(...), "\n")
          cat("HEX:   ", ..., "\n")
        }
    else
      out <- function(...) writeBin( ..., raw(), con=file)

    scat("Write file header ...")
    out( xport.file.header( cDate=cDate, sasVer=sasVer, osType=osType ) )
    scat("Done.")
    
    
    
    for(i in dfNames)
      {

        df <- dfList[[i]]

        if(is.null(colnames(df)))
           colnames(df) <- rep("", length=ncol(df))

        emptyFlag <- ( colnames(df)=="" | is.na(colnames(df)) )
        if(any(emptyFlag))
          {
            warning("Unnamed variables detected, using default names")
            colnames(df)[emptyFlag] = paste("VAR",1:length(emptyFlag),sep="")
            dfList[[i]] <- df
          }


        colnames(dfList[[i]]) <- colnames(df) <- varNames <- makeSASNames(colnames(df))

        offsetTable <- data.frame("name"=varNames,
                                  "len"=rep(NA, length(varNames)),
                                  "offset"=rep(NA, length(varNames)) )
        rownames(offsetTable) <- offsetTable[,"name"]

        dfLabel <- label(df, default="", self=TRUE )
        dfType <- SAStype(df, default="")

        scat("Write data frame header ...")
        out( xport.member.header(dfName=i, cDate=cDate, sasVer=sasVer, osType=osType,
                                 dfLabel=dfLabel, dfType=dfType) )
        scat("Done.")
############################ Namestr Variable Information. ##########################
        scat("Write variable information block header ...")
        out( xport.namestr.header( nvar=ncol(df) ) )
        scat("Done.")

        scat("Write entries for variable information block ...")
        lenIndex <- 0
        varIndex <- 1
        spaceUsed <- 0
        list_long_labels <- list()
        nof_long_labels <- 0
        for(i in colnames(dfList[[i]]) )
          {
            scat("", i , "...")
            var <- df[[i]]

            # get attribute information before any transformations!"
            varLabel <- attr(var, "label")
            varFormat <- attr(var, "SASformat")
            varIFormat <- attr(var, "SASiformat")
            varLen <- attr(var,'varLength')
            
            #print('1st')
            #print(attr(var,'varLength'))
            # Convert R object to SAS object
            df[[i]] <- var <- toSAS(var, format.info=formats)
            
            if(is.character(var) & is.null(varLen)){    # compute variable length
            print('found no varlength')
              varLen <- 20
            #   varLen <- attr(var, 'varLength')
            
              
            if (varLen > 200){
              print('found')
            }
            #   sub('.*-([0-9]+).*','\\1',varFormat)
            #   
            #   m <- gregexpr('[0-9]+',varFormat)
            #   n <- as.numeric(regmatches(varFormat,m))
            #   print(n)
            #   if (varLen > n){
            #     var <- sapply(var, function(x) strtrim(x,varLen-2))
            #     b
            #     print(varLen)
            #     print(n)
            #     }
            # } else if(is.numeric(var)){
            #   print(varFormat)
              #varLen <- as.numeric(varFormat)
            }
            else if (is.numeric(var))
              varLen <- 8

            # fill in variable offset and length information
            offsetTable[i, "len"]    <- varLen
            offsetTable[i, "offset"] <- lenIndex

            # parse format and iformat
            formatInfo  <- parseFormat(varFormat)
            iFormatInfo <- parseFormat(varIFormat)
            
            lablen <- nchar(varLabel, 'bytes', keepNA = F)
            if (!is_empty(lablen)){
            if (lablen > 40){
              nof_long_labels <- nof_long_labels +1
              name_label <- paste0(toupper(as.character(i)), as.character(varLabel))
              varName = toupper(as.character(i))
              labName = as.character(varLabel)
              nameLen <- nchar(i, "bytes", keepNA=FALSE)
              charLen = nameLen + lablen
              
              llabel <- structure(list(name=name_label, index = varIndex, labLen = lablen, 
                                       namLen = nameLen, bytes = (6 + charLen),charLen = charLen,
                                       varName = varName, varLabel=varLabel))
              list_long_labels[[nof_long_labels]] <- llabel
              
            }
            }
            
            # write the entry
            out(
                xport.namestr(
                              var=var,
                              varName   = i,
                              varNum    = varIndex,
                              varPos    = lenIndex,
                              varLength = varLen,
                              varLabel  = varLabel,
                              fName   = formatInfo$name,
                              fLength = formatInfo$len,
                              fDigits = formatInfo$digits,
                              iName   = iFormatInfo$name,
                              iLength = iFormatInfo$len,
                              iDigits = iFormatInfo$digits,
                              lVarName = i,
                              lablen = lablen
                              )
                )

            # increment our counters
            lenIndex <- lenIndex + varLen
            varIndex <- varIndex + 1
            spaceUsed <- spaceUsed + 140
          }
        scat("Done.")
        
       
        
        
        ################################################

        # Space-fill to 80 character record end
        fillSize <- 80 - (spaceUsed %% 80)
        if(fillSize==80) fillSize <- 0
        out( xport.fill( TRUE, fillSize ) )

        ##print(nof_long_labels)
        ########## Write Long Label Names Header##############
        if (nof_long_labels >0){
          spaceUsed <- 0
          scat("Write header for label block ...")
          out( xport.label8.header(nvar=nof_long_labels) )
          scat("Done")
          ########## Write Long Label Names Header##############        
          scat('Write long Label Data')
          
          for (llabel in list_long_labels){
            spaceUsed = spaceUsed + llabel$bytes
            out(xport.label8(
              varName   = llabel$varName,
              varLabel = llabel$varLabel,
              varNum    = llabel$index,
              labLen    = llabel$labLen,
              namLen = llabel$namLen,
              totLen = llabel$bytes
            ))
            
            # out( xport.numeric( llabel$index ) )
            # out( xport.numeric( llabel$namLen ) )
            # out( xport.numeric( llabel$labLen ) )
            # out(xport.character(llabel$varName, width=llabel$namLen ) )
            # out(xport.character(llabel$varLabel, width=llabel$labLen ) )
            
            
          }
          
          
          fillSize <- 80 - (spaceUsed %% 80)
          if(fillSize==80) fillSize <- 0
          out( xport.fill(TRUE, fillSize ) )
          
          scat("Done.")
          
          
        }
        
        
        scat("Write header for data block ...")
        out( xport.obs.header(nvar=nrow(df)) )
        scat("Done")

        scat("Write data ... ");
        spaceUsed <- 0
        if(nrow(df)>0)
          {
          for(i in 1:nrow(df) )
            for(j in 1:ncol(df) )
              {
                val <- df[i,j]
                valLen <- offsetTable[j,"len"]

                scat("i=", i, " j=", j, " value=", val, " len=", valLen, "");
                if(is.character( val ))
                  {
                    out(xport.character(val, width=valLen ) )
                  }
                else
                  out( xport.numeric( val ) )

                spaceUsed <- spaceUsed + valLen
              }
        }

        fillSize <- 80 - (spaceUsed %% 80)
        if(fillSize==80) fillSize <- 0
        out( xport.fill(TRUE, fillSize ) )

        scat("Done.")
        
        

      }

    scat("Closing file ...")
    if (is.character(file))
      if (file != "")
        {
          close(file)
          on.exit()
        }
    scat("Done")

  }
