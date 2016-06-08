# Truth table for logicals and NA

var2 <- c(TRUE, FALSE)
var3 <- c(NA, NA)
var1 <- c(1, 1)
ds <- data.frame(var1, var2, var3)
ds

ds$value_and_logical <- ifelse(ds$var1 | ds$var2, TRUE, FALSE)
ds$logical_and_na <- ifelse(ds$var2 | ds$var3, TRUE, FALSE)
ds$value_and_na <- ifelse(ds$var1 | ds$var3, TRUE, FALSE)

print(ds)

ds$var1 <- factor(ds$var1, levels = c(0, 1), labels = c("NOT ok", "OK"))
ds$var2 <- factor(ds$var2, levels = c(0, 1), labels = c("NOT ok", "OK"))
ds$var3 <- factor(ds$var3, levels = c(0, 1), labels = c("NOT ok", "OK"))

ds$value_and_logical <- ifelse(ds$var1 | ds$var2, TRUE, FALSE)
ds$logical_and_na <- ifelse(ds$var2 | ds$var3, TRUE, FALSE)
ds$value_and_na <- ifelse(ds$var1 | ds$var3, TRUE, FALSE)

print(ds)

