#' Calculate the mass transfer rate of oxygen at the air-sea interface.
#' This version ignores barometric pressure.
#'
#' See Crusius and Wanninkhof 2003 Limnology and Oceanography 48 for details
#'
#' @param windspeed is the windspeed in m / s
#' @param temperature is the water surface temperature in Celsius
#' @param salinity is the salinity in PSU
#' @param oxygen is the dissolved oxygen concentration at the surface in mg / L
#' @param height = 1 is the height of the wind sensor in m
#'
#' @return a vector of fluxes with units g / m2 / hr. Positive values indicate flux out of the water surface and negative values indicate flux into the water surface.
#' @export


masstransfer = function(windspeed,
                        temperature,
                        salinity,
                        oxygen,
                        height = 1) {
  calc_k600 = function(windspeed, height) {
    cf = 1 + sqrt(1.3e-3) / 0.4 * (log(10 / height))
    U10 = cf * windspeed # m/s
    0.228 * U10 ^ 2.2 + 0.168 # cm/h
  }

  k600 = calc_k600(windspeed, height)
  SCoxygen = marelac::gas_schmidt(temperature, species = "O2")
  a = ifelse(windspeed < 3, 2 / 3, 1 / 2)
  kx = k600 * (600 / SCoxygen) ^ a # cm / h
  o2sat = marelac::gas_O2sat(salinity, temperature, method = "Weiss")
  kx / 100 * (o2sat - oxygen) # g / m2 / h
}

#' Calculate the mass transfer rate of oxygen at the air-sea interface.
#' This version includes barometric pressure.
#'
#' See Crusius and Wanninkhof 2003 Limnology and Oceanography 48 for details
#'
#' @param windspeed is the windspeed in m / s
#' @param temperature is the water surface temperature in Celsius
#' @param salinity is the salinity in PSU
#' @param oxygen is the dissolved oxygen concentration at the surface in mg / L
#' @param pressure is the barometric pressure in HPa
#' @param height = 1 is the height of the wind sensor in m
#'
#' @return a vector of fluxes with units g / m2 / hr. Positive values indicate flux out of the water surface and negative values indicate flux into the water surface.

masstransfer2 = function(windspeed,
                         temperature,
                         salinity,
                         oxygen,
                         pressure,
                         height = 1) {
  calc_k600 = function(windspeed, height) {
    cf = 1 + sqrt(1.3e-3) / 0.4 * (log(10 / height))
    U10 = cf * windspeed # m/s
    0.228 * U10 ^ 2.2 + 0.168 # cm/h
  }

  k600 = calc_k600(windspeed, height)
  SCoxygen = marelac::gas_schmidt(temperature, species = "O2")
  a = ifelse(windspeed < 3, 2 / 3, 1 / 2)
  kx = k600 * (600 / SCoxygen) ^ a # cm / h
  pressure = pressure / 1000 # Convert HPa to bar

  o2sat = marelac::gas_satconc(salinity, temperature, pressure, species = "O2") # mmol / m3

  o2sat = o2sat * 32 / 1000
  kx / 100 * (o2sat - oxygen) # g / m2 / h
}