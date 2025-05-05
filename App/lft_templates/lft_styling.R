# done using openxlsx

# general table styling
lft_header_style <- createStyle(fgFill = "#0082C4", halign = "CENTER",
                                textDecoration = "Bold",
                                border = c("top", "bottom", "left", "right"),
                                fontColour = "white",
                                wrapText = TRUE)

# mann kendall
increasing_trend <- createStyle(bgFill = "#FFC7CE", halign = "CENTER", textDecoration = "Bold",
                                border = c("top", "bottom", "left", "right"), fontColour = "#9C0006") # red

decreasing_trend <- createStyle(bgFill = "#90EE90", halign = "CENTER", textDecoration = "Bold",
                                border = c("top", "bottom", "left", "right"), fontColour = "#006400") # green


# exceedance criteria
crit_mcl <- createStyle(bgFill = "#f8a102",
                        halign = "CENTER",
                        border = c("top", "bottom", "left", "right"),
                        fontColour = "#000000",
                        textDecoration = "bold")

light_yellow <- createStyle(bgFill = "#ffff66",
                        halign = "CENTER",
                        border = c("top", "bottom", "left", "right"),
                        fontColour = "#000000",
                        textDecoration = "bold")

bold_style <- createStyle(textDecoration = "bold")

italic_style <- createStyle(textDecoration = "italic")

underline_style <- createStyle(textDecoration = "underline")

no_style <- createStyle()

# formatting --------------------------------------------
