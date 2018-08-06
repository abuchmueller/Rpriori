## duplicatd ##

function (x, incomparables = FALSE, ...) 
{
  .local <- function (x, incomparables = FALSE) 
  {
    i <- .Call(R_pnindex, x@data, NULL, FALSE)
    duplicated(i)
  }
  .local(x, incomparables, ...)
}
<bytecode: 0x00000000112f6748>
  <environment: namespace:arules>
  
  Signatures:
  x           
target  "itemMatrix"
defined "itemMatrix"


### Unique
setMethod("unique", signature(x = "itemMatrix"),
          function(x,  incomparables = FALSE)
            x[!duplicated(x, incomparables = incomparables)])