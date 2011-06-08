setwd("~/Documents/Rpackages/vegbase")

objs <- system("ls", intern = TRUE)
objs <- objs[objs != "everythingelse"]
objs <- objs[objs != "vegbase"]

package.skeleton("vegbase", code_files = objs,
	path = "~/Documents/Rpackages/vegbase",
	namespace = TRUE, force = TRUE)