#Function to force a matrix into being symmetric

make_symmetric <- function(uneven) pmax(uneven, t(uneven))
