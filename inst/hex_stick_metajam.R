# devtools::install_github("GuangchuangYu/hexSticker")

library(hexSticker)

sticker("inst/images/metajam_jar.png", 
        package="metajam",
        p_size=7, p_family = "wqy-microhei", p_y = 0.94,
        s_x=1, s_y=1.04, s_width=.7,
        h_fill="#953735", h_color="#F79646",
        filename="inst/images/metajam_hex.png")


