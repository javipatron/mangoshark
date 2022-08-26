
#' Title: Fuel vs Electric cost comparison
#' In this Markdown document we will create a model on rental cars prices so you can select the best option regarding price and carbon offset
#'
#' @param tot_days (Total Days of your trip)
#' @param electric_rent_cost (Electric rental car rate, per day)
#' @param dist_plan (Estimated distance covered in your trip)
#' @param full_battery_cost (Cost to fully charge your electric rental car)
#' @param range_electric (Electric car range when its fully charged)
#' @param fuel_rent_cost (Fuel rental car rate, per day)
#' @param full_tank_cost (Cost to fill up the tank)
#' @param range_fuel (Fuel vehicle range distance (miles))
#'
#' @return
#' @export
#'
#' @examples
#'
#' Decision making at a Hertz Rental Car agency at the LA Airport.
#' The trip is just for the weekend = 3 days
#' There will be some addional trips around, but the estimated total distance is
#' LA to SB = 105.5 Miles
#' Weekend trips = + 100 miles
#' Total planned distance = 105.5 + 100 + 105.5 = 311 miles
#'
#'
#'Add some default configurations.
#'Add example
#'
#'Pass the results with return
#'
#'
#' price_delta(3,100,500,18.29,405,40,51.50,446)
#'
#'
price_delta <- function(tot_days, electric_rent_cost = 100, dist_plan, full_battery_cost, range_electric, fuel_rent_cost, full_tank_cost, range_fuel) {

  electric_cost <- ((tot_days * electric_rent_cost) + (dist_plan * full_battery_cost)/range_electric)

  fuel_cost <- ((tot_days * fuel_rent_cost) + ((dist_plan * full_tank_cost)/range_fuel))

  if (electric_cost > fuel_cost){
  print(paste0("The electric option is $", electric_cost - fuel_cost, " more expensive."))
  } else {
  print(paste0("The fuel option is $", round(fuel_cost - electric_cost,2), " more expensive."))
  }

slope_electric <- (((tot_days * electric_rent_cost) + (10000 * full_battery_cost)/range_electric) - ((tot_days * electric_rent_cost) + (0 * full_battery_cost)/range_electric)) / 10000

slope_fuel <- (((tot_days * fuel_rent_cost) + ((10000 * full_tank_cost)/range_fuel)) - ((tot_days * fuel_rent_cost) + (0 * full_tank_cost)/range_fuel)) / 10000

A <- rbind(c(-slope_electric, 1), c(-slope_fuel, 1))
B <- c(electric_rent_cost, fuel_rent_cost)

intersection <- solve(A,B)


print(paste0("The point of intersection for both cars is at ", round(intersection[1], 2), " miles, where the price is $", round(intersection[2],2), "  for both options"))


ggplot(data=data.frame( x=c(0,2000),y=c(0,500) ), aes(x=x,y=y)) +
  geom_point(shape = 2) +
  geom_abline(intercept = electric_rent_cost , slope = slope_electric, col = "red") +
  geom_abline(intercept = fuel_rent_cost , slope = slope_fuel, col = "blue") +
  labs(title = "Fuel vs Electric Car Price",
       x = "Distance (Miles",
       y = "Total Price (Dls)")+
  geom_hline(yintercept = 0) +
  geom_vline(xintercept = 0) +
  geom_hline(yintercept = intersection[2], col = "cyan4", lty = "dashed", alpha = 0.5) +
  geom_vline(xintercept = intersection[1], col = "cyan4", lty = "dashed", alpha = 0.5)

}


