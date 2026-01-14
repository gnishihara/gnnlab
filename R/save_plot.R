#' Save ggplot to pdf and png file.
#'
#' Saving a ggplot into a pdf then converting the pdf into a
#' png file. The filenames will automatically be saved with
#' a pdf and png extension.
#'
#' @param pdfname the pdf filename
#' @param plot the ggplot object or the most recent figure in the plot panel.
#' @param width the width of the saved plot in mm. The default is 100.
#' @param height the height of the saved plot in mm. The default is 100.
#' @param units the units of the plot dimensions. The default is "mm".
#' @param density the density of the png file in pixels. The default is 300.
#'
#' @examples
#' \dontrun{
#' save_plot("filename.pdf", width = 100, height = 100, units = "mm")
#' }
#'
#' @export
#'
#' @import tidyverse
#' @importFrom tools file_ext
#' @importFrom magick image_read_pdf
#' @importFrom magick image_write
#' @importFrom ggplot2 last_plot
#' @importFrom stringr str_replace


save_plot = function(pdfname, plot = last_plot(),
                     width = 100, height = 100, units = "mm",
                     density = 300)  {

  if(file_ext(pdfname) == "pdf") {
    pngname = str_replace(pdfname, "pdf", "png")
    ggsave(pdfname, width = width, height = height, units = units, plot = plot)
    image_read_pdf(pdfname, density = density) |> image_write(pngname)
  } else {
    stop(paste(pdfname, "must have pdf as a file extension."))
  }
}
