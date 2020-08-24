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
  yn <- y + fun(k=7, l=10*u)(x) + runif(1, min=0, max=fun(k=7, l=1*u)(x))
    
  # yn <- y + fun(k=2, l=1*u)(y) + runif(1, min = -fun(4, 0.8*u)(y), max = fun(4, 0.8*u)(y))
  angle <- 0 + runif(1, min = fun(k=2, l=-30)(x), max = fun(k=2, l=30)(x))
  
  tibble(xn=xn, yn=yn, angle=angle)
}


# Generate image ----------------------------------------------------------

# Map the function across all x-y pairs and plot the image using ggplot and
# my geom_lc() function

colours <- viridis::cividis(2, alpha=1/2)
set.seed(1)
d <- d %>% mutate(map2_df(x, y, f))

d %>% 
  sample_n(nrow(d)) %>% 
  ggplot()+
  geom_lc(aes(x=xn, y=-yn, length=u, width=u, angle=angle),
          fill=NA, 
          col="grey10", size=0.4)+
  coord_equal()+
  theme_minimal()+
  theme(legend.position = "",
        panel.grid = element_blank(),
        axis.text = element_blank(),
        axis.title = element_blank(),
        axis.ticks = element_blank(),
        panel.background = element_rect(fill="white", colour=NA),
        plot.background = element_rect(fill="white", colour = NA))+
  annotate(geom="text", x=min(d$xn), y=min(-d$yn), hjust=0, vjust=0.5, 
           label="Homage to Georg Nees [Schotter]\ngithub.com/cj-holmes/computergraphik", 
           col="grey70", size=2.5)
  # scale_fill_gradient2(low=colours[1], high=colours[2], mid = "white", midpoint = 0)

ggsave("gravel/variations/out/gravel-a.pdf", device="pdf", width=21.8, height=28, units="cm", bg="white")
ggsave("gravel/variations/out/gravel-a.png", device="png", width=21.8, height=28, units="cm", bg="white")







# Square ------------------------------------------------------------------

# Define number of boxes in x and y
nx <- 21
ny <- 21

# Compute unit size of a box
u <- 1/nx

# Compute x and y coordinates of all boxes in a uniform grid
# All image dimensions from 0 to 1
d <-
  crossing(y = seq(0, 1, by=u),
           x = seq(0, 1, by = u)) %>% 
  mutate(rn = row_number())


# Grid origin
ox <- (max(d$x)/2)+u/2
oy <- (max(d$y)/2)+u/2

f <- function(x, y){
  
  distance <- ((x - ox)^2 + (y - oy)^2)^0.5
  
  xn <- x + runif(1, 
                  min = -fun(k=3, l=2*u)(distance), 
                  max = fun(k=3, l=2*u)(distance))
  
  yn <- y + runif(1, 
                  min = -fun(k=3, l=2*u)(distance), 
                  max = fun(k=3, l=2*u)(distance))
  
  angle <- 0 + runif(1, 
                     min = fun(k=3, l=-45)(distance), 
                     max = fun(k=3, l=45)(distance))
  
  tibble(xn=xn, yn=yn, angle=angle, dist=distance)
}

set.seed(1)
d <- d %>% mutate(map2_df(x, y, f))
  
d %>% 
  ggplot()+
  geom_lc(aes(x=xn, y=yn, length=u, width=u, angle=angle), fill=NA, col="grey10", size=0.3)+
  expand_limits(y=-0.25)+
  coord_equal()+
  theme_minimal()+
  theme(legend.position = "",
        panel.grid = element_blank(),
        axis.text = element_blank(),
        axis.title = element_blank(),
        axis.ticks = element_blank(),
        panel.background = element_rect(fill="white", colour=NA),
        plot.background = element_rect(fill="white", colour = NA))+
  annotate(geom="text", x=1, y=-0.25, hjust=1, vjust=0.5, 
           label="Homage to Georg Nees [Schotter]\ngithub.com/cj-holmes/computergraphik", 
           col="grey70", size=2.5)

ggsave("gravel/variations/out/gravel-b.pdf", device="pdf", width=21.8, height=28, units="cm", bg="white")
ggsave("gravel/variations/out/gravel-b.png", device="png", width=21.8, height=28, units="cm", bg="white")















# spiral ------------------------------------------------------------------



library(tidyverse) # For wrangling
library(gglcd) # For geom_lc()

spiral <- function(x, y, n, res, a0, a1){
  
  b <- (a1 - a0)/(2*pi*n)
  l <- seq(0, 2*n*pi, l=res)
  
  tibble::tibble(x = (a0 + (b*l))*cos(l) + x,
                 y = (a0 + (b*l))*sin(l) + y)
}

# Define number of boxes in x and y
nx <- 21
ny <- 21

# Compute unit size of a box
u <- 1/nx

# Compute x and y coordinates of all boxes in a uniform grid
# All image dimensions from 0 to 1
d <-
  crossing(y = seq(0, 1, by=u),
           x = seq(0, 1, by = u)) %>% 
  mutate(rn = row_number())


# Generate spiral coordinates
ox <- (max(d$x)/2)+u/2
oy <- (max(d$y)/2)+u/2

spiral_data <-
  spiral(ox, oy, 
         n=5, 
         res=1000, 
         a0=0, 
         a1=0.7) %>% 
  mutate(spiral_id = row_number()) %>% 
  rename(spiral_x = x, spiral_y=y)

#
a <- 
  d %>%
  rename(box_x = x, box_y=y) %>% 
  left_join(spiral_data, by = character(), keep=FALSE) %>% 
  filter(abs(box_x - spiral_x) <= u/2,
         abs(box_y - spiral_y) <= u/2) %>% 
  distinct(box_x, box_y, rn) %>% 
  mutate(distance = ((box_x - ox)^2 + (box_y - oy)^2)^0.5)


d %>% 
  ggplot()+
  geom_lc(aes(x=x, y=y, length=u, width=u, angle=0), col="grey10", fill=NA, size=0.5)+
  geom_segment(data = spiral_data, aes(spiral_x, spiral_y, 
                                       xend=lead(spiral_x), yend=lead(spiral_y)),
               col=2)+
  geom_point(data = a, aes(x=box_x, y=box_y, col=distance), size=3)+
  coord_equal()+
  theme_minimal()+
  theme(legend.position = "",
        panel.grid = element_blank(),
        axis.text = element_blank(),
        axis.title = element_blank(),
        axis.ticks = element_blank(),
        panel.background = element_rect(fill="white", colour=NA),
        plot.background = element_rect(fill="white", colour = NA))
