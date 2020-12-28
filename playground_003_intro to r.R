# Intro to R - Advanced Graphics -----------------------------------------
ajax_3p <- filter(Eredivisie, Team == "Ajax" & Year >= 1996)
ggplot(ajax_3p, aes(x = Year, y = Points)) + geom_line()


# Advanced Graphics Exercises ---------------------------------------------
load(
  "/Users/mr.hsp/Documents/Erasmus University - RSM/Courses/Introduction to R/2 - Graphics/patents.RData"
)

ggplot(patents, aes(x = logtotal, color = densitycat)) +
  geom_density(fill = "black",
               alpha = 0.1,
               show.legend = FALSE)

# Intro to R - Data Wrangling ---------------------------------------------
data("Matches")
ggplot(Matches, aes(x = Result, fill = Place)) +
  geom_bar(position = "dodge")

HomeRaw <- read_csv("/Users/mr.hsp/Documents/Erasmus University - RSM/Courses/Introduction to R/5 - Data Wrangling/FeyenoordMatches-Home.csv")

