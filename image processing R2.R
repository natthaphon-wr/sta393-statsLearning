# Load the package
library(jpeg)
url <- "https://en.wikipedia.org/wiki/Bangkok_Metropolitan_Region#/media/File:Bangkok%E2%80%99s_green_lung_(40468870113).jpg"

# Download the file and save it as "Image.jpg" in the directory
#dFile <- download.file(url, "Image.jpg")
#img <- readJPEG("Image.jpg") # Read the image

img <- readJPEG("bangkok3.jpg")
img <- readJPEG("rainier.jpg")


# Obtain the dimension
imgDm <- dim(img)

# Assign RGB channels to data frame
imgRGB <- data.frame(
  x = rep(1:imgDm[2], each = imgDm[1]),
  y = rep(imgDm[1]:1, imgDm[2]),
  R = as.vector(img[,,1]),
  G = as.vector(img[,,2]),
  B = as.vector(img[,,3])
)

#plot(y ~ x, data=imgRGB, main="Road",
#     col = rgb(imgRGB[c("R", "G", "B")]),
#     asp = 1, pch = ".")
library(ggplot2)

# ggplot theme to be used
plotTheme <- function() {
  theme(
    panel.background = element_rect(
      linewidth = 3,
      colour = "black",
      fill = "white"),
    axis.ticks = element_line(
      linewidth = 2),
    panel.grid.major = element_line(
      colour = "gray80",
      linetype = "dotted"),
    panel.grid.minor = element_line(
      colour = "gray90",
      linetype = "dashed"),
    axis.title.x = element_text(
      #linewidth = rel(1.2),
      face = "bold"),
    axis.title.y = element_text(
      #linewidth = rel(1.2),
      face = "bold"),
    plot.title = element_text(
      #linewidth = 20,
      face = "bold",
      vjust = 1.5)
  )
}

# Plot the image
ggplot(data = imgRGB, aes(x = x, y = y)) + 
  geom_point(colour = rgb(imgRGB[c("R", "G", "B")])) +
  labs(title = "Original Image: Mount Ranier") +
  xlab("x") +
  ylab("y") +
  plotTheme()



kClusters <- 10
kMeans <- kmeans(imgRGB[, c("R", "G", "B")], centers = kClusters)
kColours <- rgb(kMeans$centers[kMeans$cluster,])

ggplot(data = imgRGB, aes(x = x, y = y)) + 
  geom_point(colour = kColours) +
  labs(title = paste("k-Means Clustering of", kClusters, "Colours")) +
  xlab("x") +
  ylab("y") + 
  plotTheme()

