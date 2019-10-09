rope_features <- function(d, r, density = 1.024, d_lt, wa_f = 9, dens_f = 0.94, wa_s = 12.8, dens_s = 1.14) {
  
  total_rope_length <- d * r
  
  loop_length <- total_rope_length - d
  
  loop_arm_length <- loop_length / 2
  
  weight_in_water_f <- (wa_f * (1 - (density / dens_f))) / 100
  
  weight_in_water_s <- (wa_s * (1 - (density / dens_s))) / 100
  
  length_arm_f <- loop_arm_length * (weight_in_water_s / (weight_in_water_s - weight_in_water_f))
  
  length_arm_s <- loop_arm_length * (-weight_in_water_f / (weight_in_water_s - weight_in_water_f))
  
  length_f <- d - d_lt + length_arm_f
  
  length_s <- d_lt + loop_arm_length + length_arm_s
  
  return(c(length_f, length_s))
  
}


rope_cost <- function(rope_lengths, rope_costs = c(0.3, 0.3)) {
  
  sum(rope_lengths * rope_costs)
  
}

get_cost <- function(depth) {
  
  r <- case_when(depth < 1000 ~ 1.2,
                 between(depth, 1000, 2000) ~ 1.5,
                 T ~ 2)
  
  depth <- abs(depth)
  
  rope_cost <- rope_features(d = depth, r = r, d_lt = 300) %>% 
    rope_cost()
  
  rope_cost + 1100
  
}

#- r-factor as a function current speed
#





#900 - 1100 C ost of buoys

