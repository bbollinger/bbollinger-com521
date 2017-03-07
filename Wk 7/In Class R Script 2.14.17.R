levels(warbreaks$tenstion) #lets you look at the levels in the factor
wb <- warpbreaks
relevel(wb$tension, "H", "M", "L")#change the order of the levels in order to change the order of a variable in a table
factor(wb$tension, levels-c("H", "M", "L"))#change the order of the levels in order to change the order of a variable in a table
