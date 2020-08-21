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


# Generate images ---------------------------------------------------------

# Set seed for reproducability
set.seed(1)

# Map the function across all x-y pairs 
d <- d %>% mutate(map2_df(x, y, f))

# Plot the image using ggplot2 and my geom_lc() function
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
        panel.background = element_rect(fill="white", colour = NA),
        plot.background = element_rect(fill="white", colour = NA))+
  annotate(geom="text", x=(nx*u)-(0.5*u), y=-(ny+1.5)*u, hjust=1, vjust=1, 
           label="Homage to Georg Nees [Schotter]\ngithub.com/cj-holmes/computergraphik", 
           col="grey70", size=2.5)
  
# Save 
ggsave("gravel/out/gravel.pdf", device="pdf", width=21.8, height=28, units="cm", bg="white")
ggsave("gravel/out/gravel.png", device="png", width=21.8, height=28, units="cm", bg="white")



# Show deviation of each square from starting point -----------------------

d %>% 
  ggplot()+
  # geom_point(aes(x, -y), pch=3, col="red")+
  geom_lc(aes(x=xn, y=-yn, length=u, width=u, angle=angle), fill=NA, size=0.5, col="grey98")+
  geom_segment(aes(x=x, y=-y, xend=xn, yend=-yn), col="grey10", size=0.7, lineend = "round")+
  coord_equal()+
  theme_minimal()+
  theme(legend.position = "",
        panel.grid = element_blank(),
        axis.text = element_blank(),
        axis.title = element_blank(),
        axis.ticks = element_blank(),
        panel.background = element_rect(fill="white", colour = NA),
        plot.background = element_rect(fill="white", colour = NA))+
  annotate(geom="text", x=(nx*u)-(0.5*u), y=-(ny+1.5)*u, hjust=1, vjust=1, 
           label="Homage to Georg Nees [Schotter]\ngithub.com/cj-holmes/computergraphik", 
           col="grey70", size=2.5)

ggsave("gravel/out/gravel-deviations.pdf", device="pdf", width=21.8, height=28, units="cm", bg="white")


# Variation Cividis (black baground and cividis colour) -------------------------

d %>% 
  ggplot()+
  geom_lc(aes(x=xn, y=-yn, length=u, width=u, angle=angle, col=yn), fill=NA, size=0.5)+
  coord_equal()+
  theme_minimal()+
  theme(legend.position = "",
        panel.grid = element_blank(),
        axis.text = element_blank(),
        axis.title = element_blank(),
        axis.ticks = element_blank(),
        panel.background = element_rect(fill="grey10", colour = NA),
        plot.background = element_rect(fill="grey10", colour = NA))+
  annotate(geom="text", x=(nx*u)-(0.5*u), y=-(ny+1.5)*u, hjust=1, vjust=1, 
           label="Homage to Georg Nees [Schotter]\ngithub.com/cj-holmes/computergraphik", 
           col="grey70", size=2.5)+
  scale_colour_viridis_c(option="cividis", begin=0.3)

# Save
ggsave("gravel/out/gravel-cividis.pdf", device="pdf", width=21.8, height=28, units="cm", bg="grey10")
ggsave("gravel/out/gravel-cividis.png", device="png", width=21.8, height=28, units="cm", bg="grey10")
