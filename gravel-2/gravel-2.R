# Computer generated art by Georg Nees
# "Schotter" = Gravel
# https://collections.vam.ac.uk/item/O221321/schotter-print-nees-georg/

library(tidyverse) # For wrangling
library(gglcd) # For geom_lc()

# Define number of boxes in x and y
nx <- 21
ny <- 12

# Compute unit size of a box
u <- 1/nx

# Compute x and y coordinates of all boxes in a uniform grid
# All image dimensions from 0 to 1
d <-
  crossing(y = seq(0, l = ny, by=u),
           x = seq(0, 1, by = u)) %>% 
  mutate(rn = row_number())

# Define a function for how values change in y 
t <- seq(0, 1, l=100)
fun <- function(k, l){
  y <- exp(k*(t))
  approxfun(t, ((y-min(y))/(max(y) - min(y))) * l)
}

# Help to visualise the function with different values of k and l
plot(t, fun(k=3, l=1)(t))
lines(t, fun(k=10, l=1)(t), col=2)


# Define a function that will map of over all xy coordinate pairs in turn
# Give it x and y and it will return the new values of x and y and angle
f <- function(x, y){
  
  xn <- x + runif(1, min = -fun(k=1, l=0.4*u)(x), max = fun(k=1, l=0.4*u)(x))
  yn <- y + fun(k=5, l=10*u)(x) + runif(1, min=0, max=fun(k=5, l=1*u)(x))
    
  # yn <- y + fun(k=2, l=1*u)(y) + runif(1, min = -fun(4, 0.8*u)(y), max = fun(4, 0.8*u)(y))
  angle <- 0 + runif(1, min = -fun(k=1, l=50)(x), max = fun(k=1, l=50)(x))
  
  tibble(xn=xn, yn=yn, angle=angle)
}


# Generate image ----------------------------------------------------------

# Map the function across all x-y pairs and plot the image using ggplot and
# my geom_lc() function

set.seed(1)
d <- d %>% mutate(map2_df(x, y, f))

d %>% 
  ggplot()+
  geom_lc(aes(x=xn, y=-yn, length=u, width=u, angle=angle), col="grey10", fill=NA, size=0.5)+
  coord_equal()+
  theme_minimal()+
  theme(legend.position = "",
        panel.grid = element_blank(),
        axis.text = element_blank(),
        axis.title = element_blank(),
        axis.ticks = element_blank(),
        panel.background = element_rect(fill="white", colour=NA))+
  annotate(geom="text", x=min(d$xn), y=min(-d$yn), hjust=0, vjust=0.5, 
           label="Homage to Georg Nees [Schotter]\ngithub.com/cj-holmes/computergraphik", 
           col="grey70", size=2.5)

ggsave("gravel-2/gravel-2.pdf", device="pdf", width=21.8, height=28, units="cm")
