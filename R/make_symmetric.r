#Function to force a matrix into being symmetric

make_symmetric <- function(uneven){
  up <- upper.tri(uneven) * uneven
  down <- t(upper.tri(uneven)) * uneven
  
  up[upper.tri(up)]
  down[lower.tri(down)]
  
  uneven[upper.tri(uneven)] <- up[upper.tri(up)]
  uneven[lower.tri(uneven)] <- down[lower.tri(down)]
  
  return(uneven)
}