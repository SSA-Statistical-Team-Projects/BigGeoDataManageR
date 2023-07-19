
pacman::p_load(hexSticker)

imgurl <- "inst/sticker/geosticker.png"

sticker_obj <-
sticker(imgurl,
        package = "BigGeoDataManageR",
        p_color = "ivory",
        u_color = "white",
        p_size = 35,
        p_y = 1.475,
        s_x = 1,
        s_y = 1,
        s_width = 3,
        s_height = 3,
        white_around_sticker = TRUE,
        h_color = "#30287F",
        dpi = 1000,
        spotlight = TRUE,
        filename = "inst/sticker/sticker_hex.png")
