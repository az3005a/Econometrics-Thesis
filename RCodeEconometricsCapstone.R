# Packages used 

library(readxl)
library(dplyr)
library(stringr)
library(corrplot)
library(arsenal)
library(ggplot2)
library(shiny)

################################################################################

makeFootnote = function(footnoteText =
                          format(Sys.time(), "%d %b %Y"),
                        size = .7, color = grey(.5))
{
  require(grid)
  pushViewport(viewport())
  grid.text(label = footnoteText ,
            x = unit(1,"npc") - unit(2, "mm"),
            y = unit(2, "mm"),
            just = c("right", "bottom"),
            gp = gpar(cex = size, col = color))
  popViewport()
}

data = read_xlsx("Data.xlsx")
data = data%>%
  select(i, country, year, density, capital, debt, gdp, crisis,
         fsize, bsize)%>%
  rename(id = i, 
         ctry = country,
         yr = year,
         den = density,
         cap = capital,
         lgdp = gdp)
data = data%>%
  filter(yr == "2000" | yr == "2001" | yr == "2002" | yr == "2003" |
           yr == "2004" | yr == "2005" | yr == "2006" | yr == "2007"| 
           yr == "2008" | yr == "2009" | yr == "2010" | yr == "2011" |
           yr == "2012")
data$ctry = str_replace_all(data$ctry, "Bélgica", "Belgium")
data$ctry = str_replace_all(data$ctry, "Canadá", "Canada")
data$ctry = str_replace_all(data$ctry, "Chequia", "Czech Republic")
data$ctry = str_replace_all(data$ctry, "Dinamarca", "Denmark")
data$ctry = str_replace_all(data$ctry, "Finlandia", "Finland")
data$ctry = str_replace_all(data$ctry, "Francia", "France")
data$ctry = str_replace_all(data$ctry, "Alemania", "Germany")
data$ctry = str_replace_all(data$ctry, "Grecia", "Greece")
data$ctry = str_replace_all(data$ctry, "Hungría", "Hungrary")
data$ctry = str_replace_all(data$ctry, "Irlanda", "Ireland")
data$ctry = str_replace_all(data$ctry, "Islandia", "Iceland")
data$ctry = str_replace_all(data$ctry, "Italia", "Italy")
data$ctry = str_replace_all(data$ctry, "Japón", "Japan")
data$ctry = str_replace_all(data$ctry, "Corea", "Korea")
data$ctry = str_replace_all(data$ctry, "Luxemburgo", "Luxembourg")
data$ctry = str_replace_all(data$ctry, "Letonia", "Latvia")
data$ctry = str_replace_all(data$ctry, "México", "Mexico")
data$ctry = str_replace_all(data$ctry, "Lituania", "Lithuania")
data$ctry = str_replace_all(data$ctry, "Países Bajos", "Netherlands")
data$ctry = str_replace_all(data$ctry, "Nueva Zelanda", "New Zealand")
data$ctry = str_replace_all(data$ctry, "Noruega", "Norway")
data$ctry = str_replace_all(data$ctry, "Polonia", "Poland")
data$ctry = str_replace_all(data$ctry, "Eslovaquia", "Slovakia")
data$ctry = str_replace_all(data$ctry, "Eslovenia", "Slovenia")
data$ctry = str_replace_all(data$ctry, "España", "Spain")
data$ctry = str_replace_all(data$ctry, "Suecia", "Sweden")
data$ctry = str_replace_all(data$ctry, "Turquía", "Turkey")
data$ctry = str_replace_all(data$ctry, "Reino Unido", "United Kingdom")
data$ctry = str_replace_all(data$ctry, "Estados Unidos", "United States")

# Compute the average of all of the countries for each of the column variables

data1 = data%>%
  group_by(ctry)%>%
  summarise(avden = mean(den, na.rm = TRUE),
            avcap = mean(cap, na.rm = TRUE), 
            avdebt = mean(debt, na.rm = TRUE), 
            avlgdp = mean(lgdp, na.rm = TRUE), 
            avfsize = mean(fsize, na.rm = TRUE),
            avbsize = mean(bsize, na.rm = TRUE))
data = data%>%
  full_join(data1, by ="ctry")%>%
  mutate(impden = coalesce(den, avden),
         impcap = coalesce(cap, avcap),
         impdebt = coalesce(debt, avdebt),
         implgdp = coalesce(lgdp, avlgdp),
         impfsize = coalesce(fsize, avfsize),
         impbsize = coalesce(bsize, avbsize))%>%
  select(id, ctry, yr, impden, impcap, impdebt, implgdp, 
         crisis, impfsize, impbsize)

# Exploratory data analysis

cpd = data[, c(4:10)]
cp = cor(cpd)
colnames(cp) = c("Density", "Capital", "Debt", "LGDP", 
                "Crisis","FS Size", "BS Size")
corrplot.mixed(cp, lower.col = "black", number.cex = 0.7, tl.cex = 0.7, 
               mar = c(0,0,1,0))
makeFootnote("FS = Financial Sector
              BS = Banking Sector")

cor(data4$impfsize, data4$impbsize)

euc = ifelse(data$ctry %in% c("Austria", "Belgium", "Bulgaria", 
                              "Czech Republic", "Denmark", "Estonia", 
                              "Finland", "France", "Germany", "Greece", 
                              "Hungrary", "Ireland", "Italy", "Luxembourg", 
                              "Latvia","Lithuania", "Netherlands", "Poland", 
                              "Portugal", "Slovakia", "Slovenia", "Spain",
                              "Sweden"), 1, 0)
data3 = cbind(data, euc)
data2 = data%>%
  select(implgdp, impfsize, impden, impcap, impdebt)

# Data Visualizations

ui = fluidPage(
  selectInput("vars", "Variables", 
              choices = names(data2)),
  plotOutput("plot"),
  
)

server = function(input, output) {
  output$plot = renderPlot({
    ggplot(data2, aes(x = .data[[input$vars]])) +
      geom_histogram(fill = "red", col = "white") +
      ggtitle("Histogram") +
      theme(plot.title = element_text(hjust = 0.5)) 
    
  })
}
shinyApp(ui = ui, server = server)

data3%>%
filter(abs(impfsize) > 0.5)

data4 = data3%>%
  filter(!abs(impfsize) > 0.5)

data9 = data3%>%
  filter(!abs(impbsize) > 0.5)

plot(data3$implgdp, data3$impfsize)

ggplot(data = data, mapping = aes(x = impfsize, y = implgdp)) +
  geom_point(color = "black") +
  labs(x = "Log of Financial Sector Size", y = "GDP Growth Rate") +
  geom_smooth(method = lm, se = FALSE, col = "blue") +
  geom_smooth(data = data4, method = lm, se = FALSE, col = "green") +
  theme(plot.margin = unit(c(.75,.75,.75,.75),"cm"))
makeFootnote("Green Line = Outliers Excluded; Blue Line = Outliers Included")

ggplot(data = data3, mapping = aes(x = impbsize, y = implgdp)) +
  geom_point(color = "black") +
  labs(x = "Log of Banking Sector Size", y = "GDP Growth Rate") +
  geom_smooth(method = lm, se = FALSE, col = "red") +
  geom_smooth(data = data9, method = lm, se = FALSE, col = "purple")+
  theme(plot.margin = unit(c(.75,.75,.75,.75),"cm"))
makeFootnote("Purple Line = Outliers Excluded; Red Line = Outliers Included")

data6 = data%>%
  filter(ctry == "Iceland")

write.csv(data3,"~/Desktop/Capstone/data3.csv", row.names = TRUE)

summary(data3)
summary(data7)
summary(data8)

data7 = data3%>%
  filter(euc == 1)
summary(data7)

data8 = data3%>%
  filter(euc == 0)
summary(data8)

data10 = data3%>%
  filter(!abs(impbsize) > 0.5)
data11 = data3%>%
  filter(abs(impfsize) > 0.5)

data3$bsize = exp(data3$impbsize)
data9$bsize = exp(data9$impbsize)

ggplot(data = data3, mapping = aes(x = bsize, y = implgdp)) +
  geom_point(color = "black") +
  labs(x = "Quadratic of Banking Sector Size", y = "GDP Growth Rate") +
  stat_smooth(method = "lm", formula = y~x+I(x^2), col = "red", se = FALSE) + 
  stat_smooth(data = data9, method = "lm", formula = y~x+I(x^2), col = "blue", se = FALSE) +
  theme(plot.margin = unit(c(.75,.75,.75,.75),"cm"))
makeFootnote("Blue Line = Outliers Excluded; Red Line = Outliers Included")