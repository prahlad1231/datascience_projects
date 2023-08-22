# install devtools, not needed if already installed
install.packages("devtools")
# install aframer and arframer
devtools::install_github("JohnCoene/aframer") # select option 3 (none)
devtools::install_github("JohnCoene/arframer") # select option 3 (none)
# load libraries
library(aframer)
library(arframer)

# it creates the scenes in html code
embed_aframe(
  a_scene(
    a_dependency(),
    a_box(position = "-1 0.5 -3", rotation = "0 45 0", color = "#4CC3D9"),
    a_sphere(position = "0 1.25 -5", radius = "1.25", color = "#EF2D5E"),
    a_cylinder(position = "1 0.75 -3", radius = "0.5", height = "1.5", color = "#FFC65D"),
    a_plane(position = "0 0 -4", rotation = "-90 0 0", width = "4", height = "4", color = "#7BC8A4"),
    a_sky(color="#ECECEC")
  )
)

# it runs the scene in web browser
browse_aframe(
  a_scene(
    a_dependency(),
    a_box(position = "-1 0.5 -3", rotation = "0 45 0", color = "#4CC3D9"),
    a_sphere(position = "0 1.25 -5", radius = "1.25", color = "#EF2D5E"),
    a_cylinder(position = "1 0.75 -3", radius = "0.5", height = "1.5", color = "#FFC65D"),
    a_plane(position = "0 0 -4", rotation = "-90 0 0", width = "4", height = "4", color = "#7BC8A4"),
    a_sky(color="#ECECEC")
  )
)
