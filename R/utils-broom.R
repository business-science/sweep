# Utility functions for working with forecast package -----

#' Augments data
#'
#' @param ret An object of class tibble
#' @param data Any time series data that is to be augmented
#' @param rename_index A variable indicating the index name to be used in the
#' tibble returned
sw_augment_columns <- function(ret, data, rename_index) {

    ret_1 <- data
    ret_2 <- ret

    if (is.null(ret_1)) {
        # No data supplied, return ret_2 with index

        if(!validate_index(ret_2, rename_index)) {
            # No index, must add
            ret <- add_index(ret_2, rename_index)
        } else {
            # Has index, return ret_2
            ret <- ret_2
        }
    } else {
        # data supplied, attempt to combine

        # Prep ret_1, coerce to tbl, add index
        ret_1 <- tk_tbl(ret_1, rename_index = rename_index, silent = TRUE)
        if (!validate_index(ret_1, rename_index)) {
            # if no index, add index
            ret_1 <- add_index(ret_1, rename_index)
        }

        # Check structure
        if (nrow(ret_1) != nrow(ret_2)) {
            warning("Incompatible structure. Returning .actual, .fitted and .residuals only.")
            return(ret_2) # Return unaugmented fitted and residuals
        }

        # Prep ret_2, drop index column if exists
        if (validate_index(ret_2, rename_index)) {
            ret_2 <- ret_2 %>%
                dplyr::select(-1)
        }
        if (".actual" %in% colnames(ret_2)) {
            ret_2 <- ret_2 %>%
                dplyr::select(-.actual)
        }

        # Combine data
        ret <- dplyr::bind_cols(ret_1, ret_2)

    }

    return(ret)
}

#' Validates data frame has column named the same name as variable rename_index
#'
#' @param ret An object of class tibble
#' @param rename_index A variable indicating the index name to be used in the
#' tibble returned
validate_index <- function(ret, rename_index) {
    ret_has_index <- rename_index %in% colnames(ret)
    return(ret_has_index)
}

#' Adds a sequential index column to a data frame
#'
#' @param ret An object of class tibble
#' @param rename_index A variable indicating the index name to be used in the
#' tibble returned
add_index <- function(ret, rename_index) {

    # Auto index
    ret_auto_index <- 1:nrow(ret)
    ret <- ret %>%
        tibble::add_column(..index = ret_auto_index)
    colnames(ret)[[ncol(ret)]] <- rename_index

    # Rearrange index
    ret <- ret %>%
        dplyr::select_(rename_index, "dplyr::everything()")

    return(ret)

}
