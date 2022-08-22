


#' Title: Fuel vs Electric cost comparison
#' In this Markdown document we will create a model on rental cars prices so you can select the best option regarding price and carbon offset
#'
#' @param tot_days (Total Days of your trip)
#' @param electric_rent_cost
#' @param dist_plan
#' @param full_battery_cost
#' @param range_electric
#' @param fuel_rent_cost
#' @param full_tank_cost
#' @param range_fuel
#'
#' @return
#' @export
#'
#' @examples
price_delta <- function(tot_days, electric_rent_cost, dist_plan, full_battery_cost, range_electric, fuel_rent_cost, full_tank_cost, range_fuel) {

  electric_cost <- ((tot_days * electric_rent_cost) + (dist_plan * full_battery_cost)/range_electric)

  fuel_cost <- ((tot_days * fuel_rent_cost) + ((dist_plan * full_tank_cost)/range_fuel))

  if (electric_cost > fuel_cost){
  print(paste0("The electric option is $", electric_cost - fuel_cost, " more expensive."))
  } else {
  print(paste0("The fuel option is $", fuel_cost - electric_cost, " more expensive."))
  }

slope_electric <- (((tot_days * electric_rent_cost) + (10000 * full_battery_cost)/range_electric) - ((tot_days * electric_rent_cost) + (0 * full_battery_cost)/range_electric)) / 10000

slope_fuel <- (((tot_days * fuel_rent_cost) + ((10000 * full_tank_cost)/range_fuel)) - ((tot_days * fuel_rent_cost) + (0 * full_tank_cost)/range_fuel)) / 10000

A <- rbind(c(-slope_electric, 1), c(-slope_fuel, 1))
B <- c(electric_rent_cost, fuel_rent_cost)

intersection <- solve(A,B)

print(paste0("The point of intersection for both cars is at $", round(intersection[1], 2), " miles, where the price is $", round(intersection[2],2), "  for both options"))


ggplot(data.frame(x = c(0, 2000)), aes(x = x)) +
  stat_function(fun = price_delta)

}


