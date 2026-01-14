#' Adjust dissolved oxygen concentrations
#' Use temperature and salinity to adjust the oxygen concentration.
#'
#' @param mgl is the dissolved oxygen concentration in mg / L
#' @param temperature is the water temperature in celcius
#' @param salinity = 32 is the salinity in PSU
#' @export

fix_mgl = function(mgl, temperature, salinity = 32) {
  o2sat_0 = marelac::gas_O2sat(0, temperature, method = "Weiss")
  o2sat_salinity = marelac::gas_O2sat(salinity, temperature, method = "Weiss")
  mgl * (o2sat_salinity / o2sat_0)
}

#' Adjust dissolved oxygen concentrations
#' Use barometric pressure, temperature, and salinity to adjust the oxygen concentration.
#'
#' @param mgl is the dissolved oxygen concentration in mg / L
#' @param pressure is the barometric pressure in HPa
#' @param temperature is the water temperature in celcius
#' @param salinity = 32 is the salinity in PSU
#' @export


fix_mgl2 = function(mgl, temperature, bar, salinity = 32) {
  bar = bar / 1000 # Convert HPa to bar
  o2sat_0 = marelac::gas_satconc(0, temperature, bar, species = "O2")
  o2sat_salinity = marelac::gas_satconc(salinity, temperature, bar, species = "O2")
  mgl * (o2sat_salinity / o2sat_0)
}
