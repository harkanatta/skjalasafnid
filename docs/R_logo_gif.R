
library(magick)
heaven <- image_read('https://cdn.shopify.com/s/files/1/0078/8575/0369/products/Heaven_s_Gate_Diamond_Painting.jpg?v=1571713820')
logo <- image_read("https://jeroen.github.io/images/Rlogo.png")

translogo <- image_scale(image_colorize(logo, "white", opacity = 100), "x200")
rass <- image_resize(c(translogo, logo), '200x150!') %>%
  image_transparent("white") %>%
  image_morph(frames = 80)

Rheaven <- image_animate(image_composite(image_scale(heaven, "x600"), rass, offset = "+120+140"),optimize = T, loop = 1)
image_write(Rheaven, "Rheaven.gif")