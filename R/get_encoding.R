#' Get the encoding of a file
#' This function will check the encoding of the file and is vectorized.
#'
#' This is especially important for the CEM and CKU files, since they are often stored in either Shift_JIIS or UTF-8.
#' This will depend on the computer used to offload the data from the instruments.
#'
#' @param con the name of the file to process, which can be a vector.
#'
#' @return Return the encoding of the file.
#'
#' @export
#'
#' @importFrom stringi stri_enc_detect
#' @importFrom stringi stri_enc_isutf8
#'
get_encoding = function(con) {
  encoding = function(con) {
    x = readBin(con, "raw", 500)
    if(stri_enc_isutf8(x)) {
      enc = "UTF-8"
    } else {
      z = stri_enc_detect(x)[[1]]
      enc = z[z$Language == "ja", "Encoding"]
    }
    enc
  }
  unname(sapply(con, encoding))
}
