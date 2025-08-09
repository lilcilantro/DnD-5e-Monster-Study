#welcome to the code fo rmy final project
#aka the time i lost my mind because i realized I hate coding
#no seriously its the worst and this is a math class

#get the data cause... duh
sampledata = read.csv("dnd_monsters.csv", header = TRUE)

#focus on cr and ac
#plot the points against one another
plot(sampledata$ac,sampledata$hp, xlab="Armor Class Rating", ylab="Hit Points")
#check the reverse
#plot(sampledata$hp,sampledata$ac, ylab="Armor Class Rating", xlab="Hit Points", col="firebrick3")
#i will focus on the first graph

#now looking back on lab 5 to find the corelation

# Try a linear fit function with Y=a+b*X 
model1=lm(hp~ac, data=sampledata)
# Plot the linear function obtained by regression 
xVals=seq(min(sampledata$ac),max(sampledata$ac), by=1)
lines(xVals,xVals*model1$coefficients[2]+model1$coefficients[1],col="blue")
# Find the correlation coefficient between X and Y - its     #
# value is close to 0 indicating a weak or absent linear     #
#relationship between X and Y                                #
#Notice the two lines of code do the same thing since the    #
#dataframe has only numeric variables.               
cor(sampledata$ac,sampledata$hp,method="pearson")
#cor(sampledata,method="pearson")
#this bottom line doesn't work because there are values in this data
#set that are characters instead of numbers
# Read features of the model including the coeffients.       #
# Notice the very low "Multiple R-Squared" value, indicating #
# a poor fit
summary(model1)

#use this summary in the report
#this summary can be used when focused on residuals and r squared
#ok continue with lab 5

# Since the points look parabolic, consider incorporating   #
# the X's squares - you need to add these to the data frame
sampledata$acSquared=sampledata$ac^2
# Make a new model incorporating the original values and squares #
# Y = a + b*X+c*XSquared
model2=lm(hp~ac+acSquared, data=sampledata)
# Plot the points again, add the new model
plot(sampledata$ac,sampledata$hp)
lines(xVals,model2$coefficients[1]+xVals*model2$coefficients[2]+xVals^2*model2$coefficients[3],col="red")
# Read features of the degree two polynomial linear regression #
# Notice that the value of the R-Squared value is much closer  #
# to 1, indicating a near-perfect match between model and data
summary(model2)

#highlight this summary on the report

#count ac
freq_table00 <- table(sampledata$ac)
pie(freq_table00, main = "Pie Chart of ac")

#count hp
freq_table0 <- table(sampledata$hp)
pie(freq_table0, main = "Pie Chart of hp")

#yap about that in the report






#ok now onto the characteristics 


#some characteristics include but are not limited to:
#Type, Size, Movement type, Alignment, Legendary Status, and more 
#while some monsters have a specific answer for these because of their nature
#(ex. a dragon is Huge) there are some that are per character
#(ex. a spy can be any alignment)





#count speed 
freq_table1 <- table(sampledata$speed)
#Labled ccw and had to be done so that walking would not be n/a
labels <- c("Walking", "Fly", "Fly and Swim", "Swim")
# Simple pie chart with labels
pie(freq_table1, labels = labels, main = "Pie Chart for Movement Type")

#count size
freq_table2 <- table(sampledata$size)
pie(freq_table2, main = "Pie Chart from Size Listing")

#count alignment
freq_table3 <- table(sampledata$align)
pie(freq_table3, main = "Pie Chart of Alignment")


#now is when i apply what my TA commented on my report
#wanted me to make the pie charts into histograms
#I hope this goes well


#found better way of coding graphs so I can see the lines better


#test for size
par(mar = c(5, 4, 4, 2)) 
bp = barplot(freq_table2,
             main = "The Variety of Monster Sizes",
             ylab = "# of Monsters",
             col ="mediumpurple1",
             ylim = c(0, max(freq_table2) + 50),
             axes = TRUE)
text(bp, freq_table2, labels = freq_table2, pos = 3)

#try for alignment
par(mar = c(10.5, 4, 2, 0.5)) 
bp2 = barplot(freq_table3,
        main = "The Variety of Monster ALignment",
        ylab = "# of Monsters",
        col = "sienna3",
        las = 2,
        ylim = c(0, max(freq_table3) + 25),)
text(bp2, freq_table3, labels = freq_table3, pos = 3)

#now for speed
par(mar = c(5, 4, 4, 2)) 
bp3= barplot(freq_table1,
             main = "The Variety of Monster Sizes",
             ylab = "# of Monsters",
             col ="skyblue2",
             ylim = c(0, max(freq_table1) + 40),
             axes = TRUE,
             names.arg = c("Walking", "Fly", "Fly & Swim", "Swim"))
text(bp3,freq_table1, labels = freq_table1, pos = 3)

#Now we've got the desired graphs given the feedback
#well almost because I'm actually illiterate and misunderstood
#also im being casual heer because I dont think youll actually read this
#if you are ... sorry
#but coder's notes are meant to shed light in thought process
#this is said thought process
#... anyways as I was saying
#TA actually requested histograms of the % from the original pie charts
#not just bar chart
#but uhhhh yeah bon apetite




#graphs of % of monsters


#size
freq_percent1=(freq_table2 / sum(freq_table2)) * 100
# Create bar plot with percentages
bp4=barplot(freq_percent1,
              main = "The Variety of Monster Sizes (%)",
              ylab = "Percentage of Monsters",
              col = "mediumpurple1",
              ylim = c(0, max(freq_percent1)+ 10),
              axes = TRUE)
# Add percentage labels above bars (rounded to 1 decimal place)
text(bp4,freq_percent1,labels = paste0(round(freq_percent1,1), "%"), pos = 3)

#alignment
freq_percent2=(freq_table3 / sum(freq_table3)) * 100
par(mar = c(10.5, 4, 2, 0.5)) 
# Create bar plot with percentages
bp5=barplot(freq_percent2,
            main = "The Variety of Monster Alignment (%)",
            ylab = "Percentage of Monsters",
            col = "sienna3",
            las = 2,
            ylim = c(0, max(freq_percent2)+ 2),
            axes = TRUE)
# Add percentage labels above bars (rounded to 1 decimal place)
text(bp5,freq_percent2,labels = paste0(round(freq_percent2,1), "%"), pos = 3)


#movement
freq_percent3=(freq_table1 / sum(freq_table1)) * 100
par(mar = c(5, 4, 4, 2))
# Create bar plot with percentages
bp6=barplot(freq_percent3,
            main = "The Variety of Monster Movement Type (%)",
            ylab = "Percentage of Monsters",
            col = "skyblue2",
            ylim = c(0, max(freq_percent3)+ 10),
            axes = TRUE,
            names.arg = c("Walking", "Fly", "Fly & Swim", "Swim"))
# Add percentage labels above bars (rounded to 1 decimal place)
text(bp6,freq_percent3,labels = paste0(round(freq_percent3,1), "%"), pos = 3)


#and thats the end of that
#SHout out google for all of the help 
#thank you for the r studio color cheat sheet
#and the how to for histograms, pie charts, ferquency tables, and more
#much appreciated




