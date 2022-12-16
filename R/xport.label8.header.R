`xport.label8.header` <-  function( nvar )
  {
    .C("fill_label8_header",
       nvar = as.character(nvar),  # Number of variables *AS A STRING*
       PACKAGE="SASxport"
       )
    
    .Call("getRawBuffer", PACKAGE="SASxport")
  }

