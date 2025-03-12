#' Calculate the time rate-of-change in dissolved oxygen concentrations.
#' This function will fit a GAM assuming a scaled t distribution.
#'
#' @param data is a tibble containing one day of oxygen data.
#' @param response is the name of the variable containing the oxygen data.
#' @param k = 24 is the number of basis functions used to fit the GAM.
#' @param bs = "cr" the type of basis function to use in the GAM.
#' @return A tibble of the data passed to the function is returned, including the fitted values and the derivatives.
#'
#' @export

calculate_rate = function(data,
                          response,
                          k = 24,
                          bs = "cr") {
  gcontrol = mgcv::gam.control(maxit = 500,
                               newton = list(maxNstep = 10, maxHalf = 50))

  if (!rlang::has_name(data, "H")) {
    stop("Error: The variable 'H' is missing.", call. = FALSE)
  }

  if (!is.numeric(data$H) ||
      any(data$H < 0 | data$H > 24, na.rm = TRUE)) {
    stop("Error: The variable 'H' must be numeric and contain values between 0 and 24.",
         call. = FALSE)
  }
  # Allow response to be passed as unquoted text or string
  response <- tryCatch(
    { rlang::ensym(response) },  # If unquoted, capture symbol
    error = function(e) { rlang::sym(response) }  # If quoted (character), convert to symbol
  )

  # Ensure response variable exists in data
  if (!rlang::has_name(data, as.character(response))) {
    stop(
      paste0(
        "Error: The variable '",
        as.character(response),
        "' is missing from the provided data."
      ),
      call. = FALSE
    )
  }

  formula <- rlang::new_formula(lhs = rlang::enquo(response), rhs = rlang::expr(s(H, k = !!k, bs = !!bs)))
  safe_gam = purrr::possibly(mgcv::gam, otherwise = NULL)

  out = safe_gam(
    formula,
    data = data,
    family = mgcv::scat(link = "identity"),
    control = gcontrol
  )

  if (is.null(out)) {
    return(NULL)
  } else {
    fit = gratia::fitted_values(out)$.fitted
    rate = gratia::derivatives(out, n = 144)$.derivative
    data = data |> dplyr::mutate(fit, rate)
    return(data)
  }
}
