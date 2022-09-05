#' @name vegtable_stat
#' @rdname vegtable_stat
#'
#' @title General statistics from vegtable objects
#'
#' @description
#' This function calculates general statistics of local **Turboveg**
#' databases as required by GIVD (Global Index of Vegetation-Plot Databases,
#' \url{https://www.givd.info}).
#'
#' This function is based on a script delivered by GIVD for summarising
#' statistics required in the descriptions of databases (see meta data in the
#' page of the Global Index for Vegetation-Plot Databases).
#'
#' @param vegtable An object of class [vegtable-class].
#' @param ... Further arguments passed among methods.
#'
#' @author GIVD. Adapted by Miguel Alvarez \email{kamapu78@@gmail.com}
#'
#' @examples
#' ## Statistics for GIVD
#' vegtable_stat(Kenya_veg)
#'
#' @export
vegtable_stat <- function(vegtable, ...) {
  UseMethod("vegtable_stat", vegtable)
}

#' @rdname vegtable_stat
#' @aliases vegtable_stat,vegtable-method
#' @method vegtable_stat vegtable
#' @export
vegtable_stat.vegtable <- function(vegtable, ...) {
  summary(vegtable)
  vegtable <- vegtable@header
  # Number of references
  if (!is.null(vegtable$REFERENCE)) {
    cat("REFERENCES", "\n")
    cat("Primary references: ",
      length(base::levels(factor(vegtable$REFERENCE))),
      sep = "", "\n"
    )
    cat("\n")
  }
  # Area statistics
  if (!is.null(vegtable$SURF_AREA)) {
    cat("## AREA", "\n")
    area <- vegtable$SURF_AREA
    cat("Area range (m^2): ", min(area, na.rm = TRUE), " - ", max(area,
      na.rm = TRUE
    ), sep = "", "\n")
    cat("<1 m^2: ", round(sum(area < 1, na.rm = TRUE) / nrow(vegtable) * 100),
      "%",
      sep = "", "\n"
    )
    cat("1-<10 m^2: ", round(sum(area >= 1 & area < 10,
      na.rm = TRUE
    ) / nrow(vegtable) * 100), "%",
    sep = "",
    "\n"
    )
    cat("10-<100 m^2: ", round(sum(area >= 10 & area < 100,
      na.rm = TRUE
    ) / nrow(vegtable) * 100), "%",
    sep = "",
    "\n"
    )
    cat("100-<1000 m^2: ", round(sum(area >= 100 & area < 1000,
      na.rm = TRUE
    ) / nrow(vegtable) * 100), "%",
    sep = "",
    "\n"
    )
    cat("1000-<10000 m^2: ", round(sum(area >= 1000 & area < 10000,
      na.rm = TRUE
    ) / nrow(vegtable) * 100), "%",
    sep = "",
    "\n"
    )
    cat(">=10000 m^2: ", round(sum(area >= 10000,
      na.rm = TRUE
    ) / nrow(vegtable) * 100), "%",
    sep = "",
    "\n"
    )
    cat("unknow: ", round(sum(is.na(area)) / nrow(vegtable) * 100), "%",
      sep = "",
      "\n"
    )
    cat("\n")
  }
  # Time statistics
  if (!is.null(vegtable$DATE)) {
    cat("## TIME", "\n")
    years <- as.numeric(format(vegtable$DATE, "%Y"))
    cat("oldest: ", min(years, na.rm = TRUE), " - youngest: ", max(years,
      na.rm = TRUE
    ), sep = "", "\n")
    cat("<=1919: ", round(sum(years <= 1919,
      na.rm = TRUE
    ) / nrow(vegtable) * 100), "%",
    sep = "",
    "\n"
    )
    cat("1920-1929: ", round(sum(years > 1919 & years < 1930,
      na.rm = TRUE
    ) / nrow(vegtable) * 100), "%",
    sep = "",
    "\n"
    )
    cat("1930-1939: ", round(sum(years > 1929 & years < 1940,
      na.rm = TRUE
    ) / nrow(vegtable) * 100), "%",
    sep = "",
    "\n"
    )
    cat("1940-1949: ", round(sum(years > 1939 & years < 1950,
      na.rm = TRUE
    ) / nrow(vegtable) * 100), "%",
    sep = "",
    "\n"
    )
    cat("1950-1959: ", round(sum(years > 1949 & years < 1960,
      na.rm = TRUE
    ) / nrow(vegtable) * 100), "%",
    sep = "",
    "\n"
    )
    cat("1960-1969: ", round(sum(years > 1959 & years < 1970,
      na.rm = TRUE
    ) / nrow(vegtable) * 100), "%",
    sep = "",
    "\n"
    )
    cat("1970-1979: ", round(sum(years > 1969 & years < 1980,
      na.rm = TRUE
    ) / nrow(vegtable) * 100), "%",
    sep = "",
    "\n"
    )
    cat("1980-1989: ", round(sum(years > 1979 & years < 1990,
      na.rm = TRUE
    ) / nrow(vegtable) * 100), "%",
    sep = "",
    "\n"
    )
    cat("1990-1999: ", round(sum(years > 1989 & years < 2000,
      na.rm = TRUE
    ) / nrow(vegtable) * 100), "%",
    sep = "",
    "\n"
    )
    cat("2000-2009: ", round(sum(years > 1999 & years < 2010,
      na.rm = TRUE
    ) / nrow(vegtable) * 100), "%",
    sep = "",
    "\n"
    )
    cat("2010-2019: ", round(sum(years > 2009 & years < 2020,
      na.rm = TRUE
    ) / nrow(vegtable) * 100), "%",
    sep = "",
    "\n"
    )
    cat("unknow: ", round(sum(is.na(years)) / nrow(vegtable) * 100), "%",
      sep = "", "\n"
    )
    cat("\n")
  }
  # Country
  if (!is.null(vegtable$COUNTRY)) {
    cat("## DISTRIBUTION", "\n")
    countries <- summary(factor(vegtable$COUNTRY), maxsum = 1000)
    for (i in names(countries)) {
      cat(i, ": ", round(countries[i] / nrow(vegtable) * 100), "%",
        sep = "",
        "\n"
      )
    }
    cat("\n")
  }
  # Performance
  if (!is.null(vegtable$COVERSCALE)) {
    cat("## PERFORMANCE", "\n")
    covscale <- summary(as.factor(vegtable$COVERSCALE))
    for (i in names(covscale)) {
      cat(i, ": ", round(covscale[i] / nrow(vegtable) * 100), "%",
        sep = "",
        "\n"
      )
    }
    cat("\n")
  }
}
