# Computer generated art by Georg Nees
# "Schotter" = Gravel
# https://collections.vam.ac.uk/item/O221321/schotter-print-nees-georg/

library(tidyverse) # For wrangling
library(gglcd) # For geom_lc()

# Define number of boxes in x and y
nx <- 12
ny <- 21

# Compute unit size of a box
u <- 1/ny

# Compute x and y coordinates of all boxes in a uniform grid
# All image dimensions from 0 to 1
d <-
  crossing(y = seq(0, 1, by = u), 
           x = seq(0, l = nx, by=u)) %>% 
  mutate(rn = row_number())

# Define a function for how values change in y 
t <- seq(0, 1, l=100)
fun <- function(k, l){
  y <- exp(k*(t))
  approxfun(t, ((y-min(y))/(max(y) - min(y))) * l)
}

# Help to visualise the function with different values of k and l
# plot(t, fun(k=1, l=1)(t))
# lines(t, fun(k=2, l=1)(t), col=2)


# Define a function that will map of over all xy coordinate pairs in turn
# Give it x and y and it will return the new values of x and y and angle
f <- function(x, y){
  
  xn <- x + runif(1, min = -fun(k=1, l=0.5*u)(y), max = fun(k=1, l=0.5*u)(y))
  yn <- y + runif(1, min = -fun(k=1, l=0.5*u)(y), max = fun(k=1, l=0.5*u)(y))
  # yn <- y + fun(k=2, l=1*u)(y) + runif(1, min = -fun(4, 0.8*u)(y), max = fun(4, 0.8*u)(y))
  angle <- 0 + runif(1, min = -fun(k=1, l=50)(y), max = fun(k=1, l=50)(y))
  
  tibble(xn=xn, yn=yn, angle=angle)
}

# Set seed for reproducability
set.seed(1)

# Map the function across all x-y pairs and plot the image using ggplot and
# my geom_lc() function
d %>% 
  mutate(map2_df(x, y, f)) %>%
  ggplot()+
  geom_lc(aes(x=xn, y=-yn, length=u, width=u, angle=angle), fill=NA, size=0.5)+
  # geom_lc(aes(x=xn, y=-yn, length=u, width=u, angle=angle), fill=NA, lc_shape = "ellipse")+
  # geom_text(aes(xn, -yn, label=rn, angle=angle), col="grey")+
  coord_equal()+
  theme_minimal()+
  theme(legend.position = "",
        panel.grid = element_blank(),
        axis.text = element_blank(),
        axis.title = element_blank())+
  annotate(geom="text", x=(nx*u)-(0.5*u), y=-(ny+1.5)*u, hjust=1, vjust=1, 
           label="Homage to Georg Nees [Schotter]\ngithub.com/cj-holmes/computergraphik", 
           col="grey70", size=2.5)
  # scale_fill_gradient(low = "grey50", high="white")+

# Save 
ggsave("gravel/gravel.pdf", device="pdf", width=21.8, height=28, units="cm")
ggsave("gravel/gravel.png", device="png", width=21.8, height=28, units="cm")


