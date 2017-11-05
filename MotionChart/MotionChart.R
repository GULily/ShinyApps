library(googleVis)
demo(googleVis)
pause()

data = read.csv("MotionChart.csv")
data = na.omit(data)

M = gvisMotionChart(data, idvar = "Country",
                timevar = "Year", 
                xvar = "Dim1",
                yvar = "Dim2",
                colorvar = "Region",
                options = list(width = 1000, height = 700))

plot(M)



