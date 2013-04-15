loadMessage <- function()
{
  cat("\n")
  ver <-packageDescription("SASxport", fields="Version")
  date <- packageDescription("SASxport", fields="Date")
  cat("Loaded SASxport version ", ver,  " (", date ,").\n", sep="")
  cat("\n")
  cat("  Updates and technical support available from Random Technologies, LLC\n")
  cat("  <http://random-technologies-llc.com/products/SASxport>\n")
  cat("\n")
  cat("  Type `?SASxport' for usage information.\n")
  cat("\n")
}

.onLoad <- function(lib, pkg) {
  loadMessage()
}
