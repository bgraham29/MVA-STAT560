uni <- read.table("university.DAT")

library(RCurl)
temp <- getURL("https://ramct.colostate.edu/webapps/portal/frameset.jsp?tab_tab_group_id=_2_1&url=%2Fwebapps%2Fblackboard%2Fexecute%2Flauncher%3Ftype%3DCourse%26id%3D_28698_1%26url%3D/university.DAT")
# fails / SSL certificate problem

