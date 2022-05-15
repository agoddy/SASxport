xport.obs.header <- function(nvar)
{
  .C("fill_obs_header",as.character(nvar),  PACKAGE="SASxport")
  .Call("getRawBuffer", PACKAGE="SASxport")
}

