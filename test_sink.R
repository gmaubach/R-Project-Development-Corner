sink("C:/Temp/sink-examp.txt")
i <- 1:10
outer(i, i, "*")
sink()


unlink("C:/Temp/sink-examp.txt")

## capture all the output to a file.
zz <- file("C:/Temp/all.Rout", open = "wt")
sink(zz)
sink(zz, type = "message")
try(log("a"))
sink.number("message")  # does not report any diversions
sink()
## back to the console
sink(type = "message")
sink.number("message")  # reports 2 diversions although they should be closed
                        # at this point
close(zz)
