
cps_basic <- read_dta(here("hwething_gates/data/CPS_families_2007_2023.dta"))

# function to type.convert vector and preserve labels
type_convert_labels <- function(x) {
  # grab labels
  if ( haven::is.labelled(x) ) {
    is_labelled <- TRUE
    labels <- attributes(x)[["labels"]]
  }
  else is_labelled <- FALSE
  
  # compress columns as best as possible
  # this will discard labels
  x <- type.convert(x)
  # labelled class does not work on logical vectors, 
  # converting back to integer for now.
  # I think/hope this won't introduce problems over different years of data?
  if (is.logical(x)) {
    x <- as.integer(x)
  }
  
  # add labels back if needed
  if ( is_labelled ) {
    haven::labelled(x, labels)
  }
  else x
}


# correct column types
dta <- mutate(cps_basic, across(where(is.numeric), type_convert_labels))

# write feather
arrow::write_feather(dta, here("hwething_gates/data/CPS_families_2007_2023.feather"))


cps_feather <- arrow::read_feather(here("hwething_gates/data/CPS_families_2007_2023.feather"))
