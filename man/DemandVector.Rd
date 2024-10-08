% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/DemandVector.R
\name{DemandVector}
\alias{DemandVector}
\title{Generate Demand Change Input Vector}
\usage{
DemandVector(
  year_io = 2019,
  reallocate_prop = 1,
  excluded_products = c("alcohol", "tobacco", "food", "gambling")[1:4],
  conversion_matrix = cdohio.mod::cpa_conversion_matrix,
  food_vec,
  gambling_vec,
  tobacco_vec,
  alcohol_vec,
  consumption_category = NULL
)
}
\arguments{
\item{year_io}{Numeric. Year of input-output tables to use (select one from 2017. 2018, 2019, or 2020) - default is 2019.}

\item{reallocate_prop}{Numeric. Proportion of total change in spending reallocated to other products (0 to 1) - default is 1.}

\item{excluded_products}{Character vector. Products to exclude from reallocation. The products excluded can be any from
c("alcohol","tobacco","food","gambling") - default is to exclude all four categories.}

\item{conversion_matrix}{Data table. A matrix used for mapping between CPA product categories.}

\item{food_vec}{Numeric vector. Vector of changes in demand for food products (7)}

\item{gambling_vec}{Numeric. Change in demand for all gambling (1).}

\item{tobacco_vec}{Numeric vector. Change in demand for tobacco products (3) - licit tobacco in purchaser prices, licit
tobacco in basic prices, and illicit tobacco}

\item{alcohol_vec}{Numeric vector. Change in demand for all alcohol (2) - all alcohol in purchaser prices, and all alcohol
in basic prices}

\item{consumption_category}{Numeric integer. Takes on a value of 1-36 or NULL (default). If NULL, reallocated spending is
distributed on a pro-rata basis across the aggregate distribution of consumption across all 36 consumption categories. If
a value is specified, all consumption is allocated to the respective consumption category (See `cdohio.mod::coicop_categories` for an
index of the 36 consumption category). Note that this overrides any
restrictions imposed by `excluded_products` e.g. if "alcohol" is specified but `consumption_category` is set equal to 3
(alcoholic beverages), all expenditure will be reallocated to alcoholic beverages.}
}
\value{
A vector of length 105
}
\description{
Take inputs created with the `GenExpenditure` function and construct a vector of changes in
final demand to use as the input to the input-output model.
}
\examples{

\dontrun{

}
}
