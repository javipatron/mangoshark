
#' Title: Fuel vs Electric cost comparison
#' In this Markdown document we will create a model on rental cars prices so you can select the best option regarding price and carbon offset
#'
#' @param tot_days (Total Days of your trip)
#' @param electric_rent_cost (Electric car rental price per day)
#' @param dist_plan (Estimated distance to be covered in your trip)
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
#' There will be some additional trips around, so the estimated total distance is
#' LA to SB = 105.5 Miles
#' Weekend trips = + 100 miles
#' Total planned distance = 105.5 + 100 + 105.5 = 311 miles
#' The rental prices at Hertz are; Tessla $76 per day, Honda Civic $48 per day
#' The cost to fully charge the Tessla is $20 usd
#' The cost to fill up the tank of a Honda Civic $55dls
#' The range of the Tessla with a fully battery charged is 405 miles
#' The range of the Honda with a full tank is 450 miles
#'
#'
#'Add some default configurations.
#'
#'Add example
#'
#'
#'
#' Default configuration = price_delta(3,76,311,20,400,48,48,450)
#'
#'
price_delta <- function(tot_days = 3, electric_rent_cost = 76, dist_plan = 311, full_battery_cost = 20, range_electric = 400, fuel_rent_cost = 48, full_tank_cost = 48 , range_fuel = 450) {

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


colors <- c("electric_car" = "red", "fuel_car" = "blue")
plot <- ggplot(data=data.frame(x = c(0,2000),y = c(0,500) ), aes(x = x ,y = y)) +
  geom_point(shape = 2) +
  geom_abline(intercept = electric_rent_cost ,
              slope = slope_electric,
              color = "red",
              size = 1) +
  geom_abline(intercept = fuel_rent_cost ,
              slope = slope_fuel,
              color = "blue",
              size = 1) +
  labs(title = "Rental Price Comparison",
       subtitle = "Fuel vs Electric Car Price",
       x = "Distance (Miles)",
       y = "Total Price (Dls)",
       caption = "price vizualization",
       color = "Legend") +
  geom_hline(yintercept = 0) +
  geom_vline(xintercept = 0) +
  geom_hline(yintercept = intersection[2], col = "cyan4", lty = "dashed", alpha = 0.5) +
  geom_vline(xintercept = intersection[1], col = "cyan4", lty = "dashed", alpha = 0.5)


print(plot)
return(list(price_intersection = round(intersection[2],2), distance_intersection = round(intersection[1], 2)))

}


