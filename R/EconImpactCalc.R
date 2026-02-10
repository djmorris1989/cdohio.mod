#' Calculate Economic Impacts
#'
#' Apply the input-output model parameters to a pre-specified vector of changes in final demand for 105 products. Calculate
#' impacts on output, gross value added, tax receipts to government, employment, and earnings. Three types of effect are
#' reported - the direct effects, direct + indirect effects, and direct + indirect + induced effects.
#'
#' @param year Numeric. Year of expenditure, tax, and labour market outcomes data to use - default is 2022.
#' @param year_io Numeric. Year of input-output tables to use (select one from 2017 to 2022) - default is 2022.
#' @param base Numeric. Base year to use for inflation adjustment - default is 2022.
#' @param input_vector Numeric vector. The vector of length 105 of changes in final demand in basic prices.
#' @param alcohol_off_tax Numeric. Change in tax less subsidies on products for off-trade alcohol calculated from `GenExpenditure()` function.
#' @param alcohol_on_tax Numeric. Change in tax less subsidies on products for on-trade alcohol calculated from `GenExpenditure()` function.
#' @param tobacco_tax Numeric. Change in tax less subsidies on products for tobacco calculated from `GenExpenditure()` function.
#'
#' @return A list of outputs
#' @export
#'
#' @examples
#'
#' \dontrun{
#'
#' }
EconImpactCalc <- function(year = 2022,
                           year_io = 2022,
                           base = 2022,
                           input_vector,
                           alcohol_off_tax = 0,
                           alcohol_on_tax = 0,
                           tobacco_tax = 0){

  ###################################################################
  ## Add changes in tax on alcohol and tobacco which was calculated
  ## manually

  alc_tob_duties <- alcohol_off_tax + alcohol_on_tax + tobacco_tax

  ###############################################
  ### create inflation adjustment factor ########

  yr <- copy(year)

  infl_adjust <- as.numeric(cdohio.mod::rpi[year == base,"rpi_index"]) / as.numeric(cdohio.mod::rpi[year == yr,"rpi_index"])

  ###############################################
  ### extract the selected input-output table ###
  ###############################################

  if (year_io == 2017){
    inputoutput <- cdohio.mod::inputoutput_2017
  }
  if (year_io == 2018){
    inputoutput <- cdohio.mod::inputoutput_2018
  }
  if (year_io == 2019){
    inputoutput <- cdohio.mod::inputoutput_2019
  }
  if (year_io == 2020){
    inputoutput <- cdohio.mod::inputoutput_2020
  }
  if (year_io == 2021){
    inputoutput <- cdohio.mod::inputoutput_2021
  }
  if (year_io == 2022){
    inputoutput <- cdohio.mod::inputoutput_2022
  }

  L0 <- diag(nrow(inputoutput$L))

  products <- inputoutput$product_categories_io

  output_to_supply <- as.matrix(inputoutput$supply[,"output_to_supply"])

  tax_pct <- as.matrix(inputoutput$supply[,"tax_pct"])

  ###############################################################################
  ### extract wage data for the chosen year and calculate income tax and NICs ###
  ###############################################################################

  inctax <- TaxCalc(earn_data = cdohio.mod::lfs_wage_data,
                    tax_data = cdohio.mod::income_tax_params,
                    year = year)

  gross_earnings <- inctax$earn
  net_earnings <- inctax$net_earn
  inc_tax_nics <- inctax$total_employee_tax

  ################################
  #### DIRECT IMPACTS ############

  ### calculate change in output
  output_vec <- L0 %*% input_vector

  ### calculate change in gva
  gva_vec <- output_vec * inputoutput$gva_coefficients

  ### calculate change in compensation of employees
  coe_vec <- output_vec * inputoutput$coe_coefficients

  ### calculate change in tax
  tax_production <- output_vec * inputoutput$gva_coefficients
  tax_products <- output_vec * output_to_supply * tax_pct

  tax_products_tobalc_adjust <- tax_products[16]
  tax_products[16] <- tax_products[16] - tax_products_tobalc_adjust + alc_tob_duties

  tax_vec <- tax_products + tax_production

  ### calculate change in employment
  ### (note fte coefficients are employment per £ million of output,
  ### divide to get it per £ output)
  fte_vec <- output_vec * (cdohio.mod::fte_coefficients / 1000000)

  ## calculate change in wages and taxes
  earn_vec         <- (gross_earnings * fte_vec)
  net_earn_vec     <- (net_earnings * fte_vec)
  inc_tax_nics_vec <- (inc_tax_nics * fte_vec)

  ## adjust for inflation
  output_vec <- output_vec * infl_adjust
  gva_vec <- gva_vec * infl_adjust
  tax_vec <- tax_vec * infl_adjust
  earn_vec <- earn_vec * infl_adjust
  net_earn_vec <- net_earn_vec * infl_adjust
  inc_tax_nics_vec <- inc_tax_nics_vec * infl_adjust

  type0_effects <- cbind(products, output_vec, gva_vec, tax_vec, fte_vec, earn_vec, net_earn_vec, inc_tax_nics_vec)
  setDT(type0_effects)
  setnames(type0_effects, "output_to_supply", "tax_vec")

  rm(output_vec, gva_vec, tax_vec, fte_vec, earn_vec, net_earn_vec, inc_tax_nics_vec)

  ###########################################
  #### DIRECT + INDIRECT IMPACTS ############

  ### calculate change in output
  output_vec <- inputoutput$L %*% input_vector

  ### calculate change in gva
  gva_vec <- output_vec * inputoutput$gva_coefficients

  ### calculate change in compensation of employees
  coe_vec <- output_vec * inputoutput$coe_coefficients

  ### calculate change in tax (replace direct effect of tax on products for alcohol
  ### and tobacco with manually calculated amounts)
  tax_production <- output_vec * inputoutput$gva_coefficients
  tax_products <- output_vec * output_to_supply * tax_pct


  tax_products[16] <- tax_products[16] - tax_products_tobalc_adjust + alc_tob_duties

  tax_vec <- tax_products + tax_production

  ### calculate change in employment
  fte_vec <- output_vec * (cdohio.mod::fte_coefficients / 1000000)

  ## calculate change in wages
  earn_vec         <- (gross_earnings * fte_vec)
  net_earn_vec     <- (net_earnings * fte_vec)
  inc_tax_nics_vec <- (inc_tax_nics * fte_vec)

  ## adjust for inflation
  output_vec <- output_vec * infl_adjust
  gva_vec <- gva_vec * infl_adjust
  tax_vec <- tax_vec * infl_adjust
  earn_vec <- earn_vec * infl_adjust
  net_earn_vec <- net_earn_vec * infl_adjust
  inc_tax_nics_vec <- inc_tax_nics_vec * infl_adjust

  type1_effects <- cbind(products, output_vec, gva_vec, tax_vec, fte_vec, earn_vec, net_earn_vec, inc_tax_nics_vec)
  setDT(type1_effects)
  #setnames(type1_effects, "output_to_supply", "tax_vec")

  rm(output_vec, gva_vec, tax_vec, fte_vec, earn_vec, net_earn_vec, inc_tax_nics_vec)

  #####################################################
  #### DIRECT + INDIRECT + INDUCED IMPACTS ############

  ### calculate change in output
  output_vec <- inputoutput$L2 %*% c(input_vector,0)
  output_vec <- output_vec[1:105,1]

  ### calculate change in gva
  gva_vec <- output_vec * inputoutput$gva_coefficients

  ### calculate change in compensation of employees
  coe_vec <- output_vec * inputoutput$coe_coefficients

  ### calculate change in tax
  tax_production <- output_vec * inputoutput$gva_coefficients
  tax_products <- output_vec * output_to_supply * tax_pct


  tax_products[16] <- tax_products[16] - tax_products_tobalc_adjust + alc_tob_duties

  tax_vec <- tax_products + tax_production

  ### calculate change in employment
  fte_vec <- output_vec * (cdohio.mod::fte_coefficients / 1000000)

  ## calculate change in wages
  earn_vec         <- (gross_earnings * fte_vec)
  net_earn_vec     <- (net_earnings * fte_vec)
  inc_tax_nics_vec <- (inc_tax_nics * fte_vec)

  ## adjust for inflation
  output_vec <- output_vec * infl_adjust
  gva_vec <- gva_vec * infl_adjust
  tax_vec <- tax_vec * infl_adjust
  earn_vec <- earn_vec * infl_adjust
  net_earn_vec <- net_earn_vec * infl_adjust
  inc_tax_nics_vec <- inc_tax_nics_vec * infl_adjust

  type2_effects <- cbind(products, output_vec, gva_vec, tax_vec, fte_vec, earn_vec, net_earn_vec, inc_tax_nics_vec)
  setDT(type2_effects)
  setnames(type2_effects, "output_to_supply", "tax_vec")

  rm(output_vec, gva_vec, tax_vec, fte_vec, earn_vec, net_earn_vec, inc_tax_nics_vec)

  ##########################
  ##### Gather outputs #####

  type0_effects_aggregate <- type0_effects
  type1_effects_aggregate <- type1_effects
  type2_effects_aggregate <- type2_effects

  setDT(type0_effects_aggregate)
  setDT(type1_effects_aggregate)
  setDT(type2_effects_aggregate)

  type0_effects_aggregate[, type := "Direct"]
  type1_effects_aggregate[, type := "Direct + Indirect"]
  type2_effects_aggregate[, type := "Direct + Indirect + Induced"]

  effects <- rbindlist(list(type0_effects_aggregate,
                            type1_effects_aggregate,
                            type2_effects_aggregate))

  #############################
  #### aggregate effects

  effects <- effects[, .(output_vec = sum(output_vec),
                         gva_vec = sum(gva_vec),
                         tax_vec = sum(tax_vec),
                         fte_vec = sum(fte_vec),
                         earn_vec = sum(earn_vec),
                         net_earn_vec = sum(net_earn_vec),
                         inc_tax_nics_vec = sum(inc_tax_nics_vec)), by = "type"]

  #################################
  #### aggregate effects (%)

  ## to estimate % of totals, adjust the figures in the effects table for inflation to the year
  ## of the input-output table used. For employment, use the employment data for the input-output table
  ## year also

  infl_adjust2 <- as.numeric(cdohio.mod::rpi[year == year_io,"rpi_index"]) / as.numeric(cdohio.mod::rpi[year == base,"rpi_index"])

  effects_pct <- copy(effects)

  effects_pct[, output_vec := output_vec * infl_adjust2]
  effects_pct[, gva_vec := gva_vec * infl_adjust2]
  effects_pct[, tax_vec := tax_vec * infl_adjust2]
  effects_pct[, earn_vec := earn_vec * infl_adjust2]
  effects_pct[, net_earn_vec := net_earn_vec * infl_adjust2]
  effects_pct[, inc_tax_nics_vec := inc_tax_nics_vec * infl_adjust2]

  #####################################################################
  ## get annual totals to calculate relative effects

  inctax_year_ioat <- TaxCalc(earn_data = cdohio.mod::lfs_wage_data,
                              tax_data = cdohio.mod::income_tax_params,
                              year = year_io)


  if (year_io == 2017){
    total_fte     <- sum(cdohio.mod::lfs_empl_data[, "fte_2017"])
    fte           <- cdohio.mod::lfs_empl_data$fte_2017
  } else if (year_io == 2018){
    total_fte     <- sum(cdohio.mod::lfs_empl_data[, "fte_2018"])
    fte           <- cdohio.mod::lfs_empl_data$fte_2018
  } else if (year_io == 2019){
    total_fte     <- sum(cdohio.mod::lfs_empl_data[, "fte_2019"])
    fte           <- cdohio.mod::lfs_empl_data$fte_2019
  } else if (year_io == 2020){
    total_fte     <- sum(cdohio.mod::lfs_empl_data[, "fte_2020"])
    fte           <- cdohio.mod::lfs_empl_data$fte_2020
  } else if (year_io == 2021){
    total_fte     <- sum(cdohio.mod::lfs_empl_data[, "fte_2021"])
    fte           <- cdohio.mod::lfs_empl_data$fte_2021
  } else if (year_io == 2022){
    total_fte     <- sum(cdohio.mod::lfs_empl_data[, "fte_2022"])
    fte           <- cdohio.mod::lfs_empl_data$fte_2022
  }

  ## get baseline totals

  total_gross_earnings <- sum(inctax_year_ioat$earn * fte) / 1000000
  total_net_earnings   <- sum(inctax_year_ioat$net_earn * fte) / 1000000
  total_inc_tax_nics   <- sum(inctax_year_ioat$total_employee_tax * fte) / 1000000
  total_tax            <- sum(inputoutput$tax_on_products) + sum(inputoutput$tax_on_production)
  total_output         <- sum(inputoutput$output)
  total_gva            <- sum(inputoutput$gva)


  ## calculate the estimated relative effects

  effects_pct[, output_vec := 100*(output_vec / total_output)]
  effects_pct[, gva_vec := 100*(gva_vec / total_gva)]
  effects_pct[, tax_vec := 100*(tax_vec / total_tax)]
  effects_pct[, fte_vec := 100*(fte_vec / total_fte)]
  effects_pct[, earn_vec := 100*(earn_vec / total_gross_earnings)]
  effects_pct[, net_earn_vec := 100*(net_earn_vec / total_net_earnings)]
  effects_pct[, inc_tax_nics_vec := 100*(inc_tax_nics_vec / total_inc_tax_nics)]

  ## add the baseline total numbers to a matrix

  baseline <- matrix(c(total_output,
                       total_gva,
                       total_tax,
                       total_fte,
                       total_net_earnings,
                       total_inc_tax_nics),
                     nrow = 1,
                     dimnames = list(NULL,
                                     c("output","gva","tax_employers","fte","net_earn","inc_tax_nics"))
                     )

  return(list(effects = effects,
              effects_pct = effects_pct,
              baseline = baseline,
              type0_effects_by_product = type0_effects,
              type1_effects_by_product = type1_effects,
              type2_effects_by_product = type2_effects))

}
