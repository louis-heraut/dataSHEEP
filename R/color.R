# Copyright 2022 Louis Héraut (louis.heraut@inrae.fr)*1,
#                Éric Sauquet (eric.sauquet@inrae.fr)*1
#
# *1   INRAE, France
#
# This file is part of dataSheep R package.
#
# dataSheep R package is free software: you can redistribute it and/or
# modify it under the terms of the GNU General Public License as
# published by the Free Software Foundation, either version 3 of the
# License, or (at your option) any later version.
#
# dataSheep R package is distributed in the hope that it will be useful, but
# WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
# General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with dataSheep R package.
# If not, see <https://www.gnu.org/licenses/>.

# "EXPLORE2", "IPCC"
 
#' @title assign_colors
#' @export
assign_colors = function (refCOL="INRAE") {

     # lighter lighter plot background
    assign("IPCCgrey99", "#f8f9f9", .GlobalEnv)
    # lighter hot plot background
    assign("IPCCgrey97b", "#f9f8f7", .GlobalEnv)
    # lighter blue plot background
    assign("IPCCgrey97", "#f6f7f7", .GlobalEnv)
    # plot background
    assign("IPCCgrey95", "#f4f2f1", .GlobalEnv)
    assign("IPCCgrey92", "#e9eceb", .GlobalEnv)
    # minor tick
    assign("IPCCgrey90", "#e3e2e0", .GlobalEnv)
    # grid on grey background low important axis
    assign("IPCCgrey85", "#dcdad9", .GlobalEnv)
    # grid on white background
    assign("IPCCgrey80", "#cfd1d0", .GlobalEnv)
    # major tick
    assign("IPCCgrey75", "#bebdbb", .GlobalEnv)
    # minor line
    assign("IPCCgrey67", "#adabaa", .GlobalEnv)
    # important axis
    assign("IPCCgrey60", "#9c9c9b", .GlobalEnv)
    # low important annotation
    assign("IPCCgrey50", "#81848b", .GlobalEnv)
    # major line
    assign("IPCCgrey48", "#847b73", .GlobalEnv)
    # low important label
    assign("IPCCgrey40", "#656769", .GlobalEnv)
    assign("IPCCgrey35", "#565859", .GlobalEnv) 
    assign("IPCCgrey25", "#454547", .GlobalEnv)
    assign("IPCCgrey23", "#3b3b3c", .GlobalEnv)
    # important title, label or annotation
    assign("IPCCgrey20", "#060403", .GlobalEnv)
    # low important title
    assign("IPCCgrey18", "#2f2f32", .GlobalEnv)
    # font
    assign("IPCCgrey13", "#231f20", .GlobalEnv)
    # realy important title
    assign("IPCCgrey05", "#100f0d", .GlobalEnv)
    assign("IPCCcyan", "#449c93", .GlobalEnv)
    assign("IPCCligthcyan", "#90d6c6", .GlobalEnv)
    assign("IPCCwhitecyan", "#a8ded3", .GlobalEnv)
    assign("IPCCbrique", "#794822", .GlobalEnv)
    assign("IPCCgold", "#e6d495", .GlobalEnv)
    assign("IPCCblue", "#1e2f59", .GlobalEnv)
    assign("IPCCfreshblue", "#BBD6ED", .GlobalEnv)

    assign("INRAEcyan", "#00a3a6", .GlobalEnv)
    assign("INRAElightercyan", "#b2e0df", .GlobalEnv)
    assign("INRAElightcyan", "#66c1bf", .GlobalEnv)
    assign("INRAEmediumcyan", "#008c8e", .GlobalEnv)
    assign("INRAEdarkcyan", "#275662", .GlobalEnv)
    assign("INRAElightblue", "#9ed6e3", .GlobalEnv)
    assign("INRAEgreen", "#9dc544", .GlobalEnv)
    assign("INRAEroyalblue", "#423089", .GlobalEnv)
    assign("INRAEred", "#ed6e6c", .GlobalEnv)
    assign("INRAEgrey", "#797870", .GlobalEnv)
    assign("INRAElightgrey", "#c4c0b3", .GlobalEnv)

    assign("EXPLORE2blue", "#007A92", .GlobalEnv)
    assign("EXPLORE2orange", "#EE7402", .GlobalEnv)

    assign("EXPLORE2pmc", '#075e9b', .GlobalEnv)
    assign("EXPLORE2pc", '#43a2ca', .GlobalEnv)
    assign("EXPLORE2pn", '#7bccc4', .GlobalEnv)
    assign("EXPLORE2np", '#bae4bc', .GlobalEnv)
    assign("EXPLORE2nng", '#f0f9e8', .GlobalEnv)

    assign("TRACCblue", '#2646A2', .GlobalEnv)
    assign("TRACClightblue", '#E4E3F4', .GlobalEnv)
    assign("TRACCorange", '#F47216', .GlobalEnv)
    assign("TRACCred", '#AE1C27', .GlobalEnv)
    
    assign("COLORmediumgreen", '#57bb8a', .GlobalEnv)
    

    if (refCOL == "INRAE") {
        assign("refCOL", INRAEcyan, .GlobalEnv)
    }
    if (refCOL == "EXPLORE2") {
        assign("refCOL", EXPLORE2blue, .GlobalEnv)
    }
    if (refCOL == "TRACC") {
        assign("refCOL", TRACCblue, .GlobalEnv)
    }
    if (refCOL == "IPCC") {
        assign("refCOL", IPCCblue, .GlobalEnv)
    }

    # theme_set(theme(text=element_text(family="Lato")))
    
# extrafont::font_import(paths = c("path/to/font1/directory", "path/to/font2/directory", ...))

# # Select your fonts
# # Replace "Custom Font 1", "Custom Font 2", etc. with the names of your fonts
# extrafont::loadfonts(device = "win", quiet = TRUE)
# font_names <- c("Custom Font 1", "Custom Font 2", ...) 
    
}


#' @title get_IPCC_Palette
#' @export
get_IPCC_Palette = function (palette_name, colorStep=NA, reverse=FALSE) {
    if (palette_name == "SSP") {
        Palette =
            c("#971A1F",
              "#E11D25",
              "#F68815",
              "#203563",
              "#45AED4")
    }

    if (palette_name == "RCP") {
        Palette =
            c("#003466",
              "#70A0CD",
              "#C47900",
              "#990002")
    }

        if (palette_name == "Mini_Metro") {
        Palette =
            c("#A22525",
              "#E59F00",
              "#DCD000",
              "#1A9D69",
              "#B1A8B7",
              "#59B3E8",
              "#3C5797")
    }

    if (palette_name == "ground_8") {
        Palette =
            c("#7F4A23",
              "#B3762A",
              "#D4B86A",
              "#EFE0B0",
              "#BCE6DB",
              "#7ACEB9",
              "#449C93",
              "#2A6863")
    }

    if (palette_name == "ground_10") {
        Palette =
            c("#452C1A",
              "#7F4A23",
              "#B3762A",
              "#D4B86A",
              "#EFE0B0",
              "#BCE6DB",
              "#7ACEB9",
              "#449C93",
              "#2A6863",
              "#193830")
    }

    if (palette_name == "ground_14") {
        Palette =
            c("#452C1A",
              "#704220",
              "#9A6327",
              "#BC8A3C",
              "#D5BA6D",
              "#EADBA4",
              "#F0ECD6",
              "#DCF1E9",
              "#B1E2D5",
              "#7CCFBB",
              "#51AFA0",       
              "#36847F",
              "#255A54",
              "#193830")
    }

    if (palette_name == "OrangePurple") {
        Palette =
            c("#CD5629",
              "#E8B962",
              "#F8E6DF",
              "#EDE1ED",
              "#E58DBE",
              "#893687")
    }

    if (palette_name == "rainbow_6") {
        Palette =  
            c("#D73027",
              "#FC8D59",
              "#FEE090",
              "#E0F3F8",
              "#91BFDB",
              "#4575B4")
    }
        
    if (palette_name == "rainbow_8") {
        Palette =  
            c("#96221b",
              "#D73027",
              "#FC8D59",
              "#FEE090",
              "#E0F3F8",
              "#91BFDB",
              "#4575B4",
              "#30527e")
    }

    if (palette_name == "red_ramp") {
        Palette =  
            c("#59141C",
              "#F9DDD5")
    }
    if (palette_name == "green_ramp") {
        Palette =  
            c("#276245",
              "#D5EEE2")
    }
    if (palette_name == "greyblue_ramp") {
        Palette =  
            c("#294756",
              "#d6e4eb")
    }

    if (palette_name == "darkergreyblue_ramp") {
        Palette =  
            c("#213945",
              "#d6e4eb")
    }

    if (palette_name == "YlGnBu_prettier") {
        Palette =  
            c("#2b3f5c",
              "#41b6c4",
              "#ffffcc")
    }

    if (palette_name == "YlGnBu") {
        Palette =
            c("#ffffd9",
              "#edf8b1",
              "#c7e9b4",
              "#7fcdbb",
              "#41b6c4",
              "#1d91c0",
              "#225ea8",
              "#253494",
              "#081d58")
    }

    if (palette_name == "hydro_10") {
        Palette =
            c("#452C1A", "#7F4A23", "#B3762A", "#D4B86A", "#EFE0B0", "#BCE6DB", "#7ACEB9", "#449C93", "#2A6863", "#193830")
    }

    if (palette_name == "temperature_10") {
        Palette =
            c("#053061", "#2166AC", "#4393C3", "#92C5DE", "#D1E5F0", "#FDDBC7", "#F4A582", "#D6604D", "#B2182B", "#67001F")
    }

    if (palette_name == "date_10") {
    Palette =
        c("#60265e", "#893687", "#c05fbe", "#dba3da", "#edd1ec", "#f6ddd3", "#edbaa7", "#e08765", "#CD5629", "#8f3c1d")
        # https://mdigi.tools/darken-color/#893687
        # -30 0 +30 +60 +80
    }

    if (palette_name == "risk_10") {
        Palette = c("#193830", "#2A6863", "#449C93", "#7ACEB9", "#BCE6DB", "#FDDBC7", "#F4A582", "#D6604D", "#B2182B", "#67001F")
    }
    
    if (!is.na(colorStep)) {
        Palette = colorRampPalette(Palette)(colorStep)
    }
    
    if (reverse) {
        Palette = rev(Palette)
    }
    
    return (Palette)
}


#' @title test_palette
#' @export
test_palette = function (Palette=NULL, palette_name=NULL, colorStep=NA, reverse=FALSE) {
    if (!is.null(palette_name)) {
        Palette = get_IPCC_Palette(palette_name,
                                   colorStep=colorStep,
                                   reverse=reverse)
    }
    if (is.null(Palette)) {
        stop("Give either `Palette` or `palette_name` argument")
    }
    plot = ggplot2::ggplot() + ggplot2::theme_void() +
        ggplot2::annotate("tile", x=1:colorStep, y=0,
                          color=NA, fill=Palette)
    outdir = "../palette"
    if (!dir.exists(outdir)) {
        dir.create(outdir)
    }
    ggplot2::ggsave(plot=plot,
                    path=outdir,
                    filename=paste0(palette_name, "_",
                                    paste0(Palette, collapse="-"),
                                    ".pdf"),
                    width=10,
                    height=5, units='cm',
                    dpi=300,
                    device=cairo_pdf)
}



## 1. PERSONALISATION ________________________________________________
### 1.1. Personal theme ______________________________________________
#' @title Ggplot2 theme ash
#' @export
theme_IPCC = function (is_panel.background=FALSE,
                       is_plot.background=FALSE,

                       is_plot.title=TRUE,
                       plot.title_size=10,
                       
                       is_panel.grid.major.x=FALSE,
                       is_panel.grid.major.y=TRUE, 

                       is_axis.line.x=TRUE,
                       axis.line.x_size=0.45,
                       is_axis.ticks.x=TRUE,
                       is_axis.text.x=TRUE,
                       axis.text.x_size=9,
                       axis.text.x_angle=NULL,
                       axis.text.x_hjust=NULL,
                       axis.text.x_vjust=NULL,
                       axis.ticks.length.x=1.5,

                       is_axis.line.y=FALSE,
                       is_axis.ticks.y=TRUE,
                       is_axis.text.y=TRUE,
                       axis.text.y_margin=NULL,
                       axis.text.y_size=8,
                       axis.text.y_vjust=0.59,
                       axis.ticks.length.y=axis.ticks.length.x,

                       isLabelX=FALSE, isLabelY=FALSE, 
                       is_border=FALSE,

                       family="Lato") {

    if (is_plot.background) {
        plot.background=element_rect(fill=IPCCgrey97, color=NA)
    } else {
        plot.background=element_blank()
    }
    
    if (is_panel.background) {
        panel.background=element_rect(fill=IPCCgrey97, color=NA)
    } else {
        panel.background=element_blank()
    }

    if (is_panel.grid.major.x) {
        panel.grid.major.x = element_line(color=IPCCgrey85,
                                          size=0.25)
    } else {
        panel.grid.major.x = element_blank()
    }

    if (is_axis.ticks.y) {
        axis.ticks.y = element_line(color=IPCCgrey75, size=0.4)
    } else {
        axis.ticks.y = element_blank()
        
    }
    if (is_axis.text.y) {
        axis.text.y = ggtext::element_markdown(color=IPCCgrey40,
                                               size=axis.text.y_size,
                                               margin=axis.text.y_margin,
                                               vjust=axis.text.y_vjust)
    } else {
        axis.text.y = element_blank()
    }


    if (is_axis.ticks.x) {
        axis.ticks.x = element_line(color=IPCCgrey75, size=0.4,
                                    lineend="square")
    } else {
        axis.ticks.x = element_blank()
    }
    if (is_axis.text.x) {
        axis.text.x =
            ggtext::element_markdown(color=IPCCgrey40,
                                     size=axis.text.x_size,
                                     angle=axis.text.x_angle,
                                     hjust=axis.text.x_hjust,
                                     vjust=axis.text.x_vjust)
    } else {
        axis.text.x = element_blank()
    }

    if (is_panel.grid.major.y) {
        panel.grid.major.y=element_line(color=IPCCgrey85,
                                        size=0.25)
    } else {
        panel.grid.major.y=element_blank()
    }
    
    if (is_plot.title) {
        plot.title=element_text(size=plot.title_size,
                                vjust=0, hjust=0,
                                color=IPCCgrey23,
                                face="bold")
    } else {
        plot.title=element_blank()
    }

    if (isLabelX) {
        axis.title.x = element_text(size=7.5,
                                    vjust=1, hjust=0.5,
                                    color=IPCCgrey40)
    } else {
        axis.title.x = element_blank()
    }
    
    if (isLabelY) {
        axis.title.y=element_text(size=8,
                                  vjust=1.2, hjust=0.5,
                                  color=IPCCgrey25)
    } else {
        axis.title.y=element_blank()
    }

    if (is_border) {
        panel.border = element_rect(color=IPCCgrey85,
                                    fill=NA,
                                    size=0.7)
    } else {
        panel.border = element_blank()
    }

    if (is_axis.line.x) {
        axis.line.x = element_line(color=IPCCgrey60, size=axis.line.x_size,
                                   lineend="square")
    } else {
        axis.line.x = element_blank()
    }

    if (is_axis.line.y) {
        axis.line.y = element_line(color=IPCCgrey60, size=0.45,
                                   lineend="square")
    } else {
        axis.line.y = element_blank()
    }

    library(ggh4x)
    theme_ =
        theme(
            # White background
            plot.background=plot.background,
            panel.background=panel.background,
            # Font
            text=element_text(family=family),
            # Border of plot
            panel.border=panel.border,
            # Grid
            panel.grid.major.x=panel.grid.major.x,
            panel.grid.major.y=panel.grid.major.y,
            panel.grid.minor.x=element_blank(),
            panel.grid.minor.y=element_blank(),
            # Ticks marker
            axis.ticks.x=axis.ticks.x,
            axis.ticks.y=axis.ticks.y,
            # Ticks label
            axis.text.x=axis.text.x,
            axis.text.y=axis.text.y,
            # Ticks length
            axis.ticks.length.x=unit(axis.ticks.length.x, 'mm'),
            axis.ticks.length.y=unit(axis.ticks.length.y, 'mm'),
            # Ticks minor
            ggh4x.axis.ticks.length.minor=rel(0.6),
            # Title
            plot.title=plot.title,
            # Axis title
            axis.title.x=axis.title.x,
            axis.title.y=axis.title.y,
            # Axis line
            axis.line.x=axis.line.x,
            axis.line.y=axis.line.y,

            line=element_line(lineend="round")
        )
    
    return (theme_)
}

theme_void_Lato = function () {
    theme_void() + theme(text=element_text(family="Lato"))
}


theme_void_back = function () {
    theme_void() + theme(panel.background=element_rect(fill=IPCCgrey97))
}




theme_WIP = function () {
    theme(panel.background=element_rect(fill=IPCCgrey97),
          text=element_text(family="Lato"),
          axis.ticks.x=element_line(color=IPCCgrey75, size=0.3),
          axis.ticks.y=element_line(color=IPCCgrey75, size=0.3),
          # Ticks label
          axis.text.x=element_text(color=IPCCgrey75),
          axis.text.y=element_text(color=IPCCgrey75),
          # Ticks length
          axis.ticks.length=unit(1, 'mm'),
          # Ticks minor
          ggh4x.axis.ticks.length.minor=rel(0.5),
          # Title
          plot.title=element_blank(),
          # Axis title
          axis.title.x=element_blank(),
          axis.title.y=element_blank(),
          # Axis line
          axis.line.x=element_line(color=IPCCgrey75, size=0.3),
          axis.line.y=element_line(color=IPCCgrey75, size=0.3))
}


## 2. COLOR MANAGEMENT _______________________________________________
### 2.1. Compute colors ______________________________________________
#' @title Number formatting
#' @export
get_power = function (value) {
    if (is.na(value) | !is.finite(value)) {
        return (0)
    }
    if (length(value) > 1) {
        power = unlist(as.list(sapply(value, get_power),
                               recursive=TRUE,
                               use.names=FALSE))
    } else {
        if (!is.na(value)) {
            value = abs(value)
            
            if (value >= 1) {
                power = nchar(as.character(as.integer(value))) - 1
            } else if (value == 0) {
                power = 0
            } else {
                dec = gsub('0.', '', as.character(value), fixed=TRUE)
                ndec = nchar(dec)
                nnum = nchar(as.character(
                    as.numeric(dec)))
                power = -(ndec - nnum + 1)
            }
        } else {
            power = NA
        }
    }
    return (power)
}

get_nearest = function (x, X) {
    X[which.min(abs(x - X))]
}


round_pimp = function (bin, center=NULL) {
    step = round(min(diff(bin)), -get_power(min(diff(bin))))
    Step = step*0:round(10^(max(sapply(bin, get_power))+1))
    if (is.null(center)) {
        center = 0
    }
    Step = c(center-rev(Step[-1]), center + Step)
    Step = sapply(bin, get_nearest, Step)
    dStep = round(diff(Step), 10)

    i = 1
    while (any(duplicated(Step)) |
           any(dStep != dStep[1])) {
        step = round(min(diff(bin)),
                     -get_power(min(diff(bin)))+i)
        Step = step*0:round(10^(max(sapply(bin,
                                           get_power))+(i+1)))
        if (is.null(center)) {
            center = 0
        }
        Step = c(center-rev(Step[-1]), center + Step)
        Step = sapply(bin, get_nearest, Step)
        dStep = round(diff(Step), 10)
        i = i+1

        if (i == 4) {
            break
        }
    }
    return (Step)
}


#' @title Compute color bin
#' @export
compute_colorBin = function (min, max, colorStep, center=NULL,
                             include=FALSE, round=TRUE) {

    if (!is.null(center)) {
        maxAbs = max(abs(max-center), abs(min-center))
        minValue = -maxAbs + center
        maxValue = maxAbs + center
    } else {
        minValue = min
        maxValue = max
    }

    if (all(include)) {
        nBin = colorStep + 1
    } else if (all(!include)) {
        nBin = colorStep - 1
    } else {
        nBin = colorStep
    }
    
    bin = seq(minValue, maxValue, length.out=nBin)
    
    if (round) {
        bin = round_pimp(bin, center=center)
    }
    
    if (length(include) == 1) {
        if (!include) {
            upBin = c(bin, Inf)
            lowBin = c(-Inf, bin)
            bin = c(-Inf, bin, Inf)
        } else {
            upBin = bin[2:length(bin)]
            lowBin = bin[1:(length(bin)-1)]
        }

    } else if (length(include) == 2) {
        if (!include[1] & !include[2]) {
            upBin = c(bin, Inf)
            lowBin = c(-Inf, bin)
            bin = c(-Inf, bin, Inf)

        } else if (include[1] & !include[2]) {
            upBin = c(bin[2:length(bin)], Inf)
            lowBin = bin
            bin = c(bin, Inf)

        } else if (!include[1] & include[2]) {
            upBin = bin
            lowBin = c(-Inf, bin[1:(length(bin)-1)])
            bin = c(-Inf, bin)
            
        } else if (include[1] & include[2]) {
            upBin = bin[2:length(bin)]
            lowBin = bin[1:(length(bin)-1)]
        }
    }

    res = list(bin=bin, upBin=upBin, lowBin=lowBin)
    return (res)
}



#' @title get_color
#' @export
get_color = function (value, upBin, lowBin, Palette,
                      Palette_level=NULL,
                      include_min=FALSE,
                      include_max=TRUE,
                      return_id=FALSE) {


    if (length(include_min) != 1 | length(include_max) != 1) {
        if (length(include_min) != length(Palette)) {
            include_min = rep(include_min, length.out=length(Palette))
        }
        if (length(include_max) != length(Palette)) {
            include_max = rep(include_max, length.out=length(Palette))
        }

        id = mapply(get_color, include_min, include_max,
                    MoreArgs=list(value=value,
                                  upBin=upBin, lowBin=lowBin,
                                  Palette=Palette,
                                  return_id=TRUE))
        id = id[!is.na(id)]
        id = id[1]
        
    } else {
        if (!include_min & include_max) {
            id = which(lowBin < value & value <= upBin)
        } else if (include_min & !include_max) {
            id = which(lowBin <= value & value < upBin)
        } else if (!include_min & !include_max) {
            id = which(lowBin < value & value < upBin)
        } else if (include_min & include_max) {
            id = which(lowBin < value & value <= upBin)
        }
    }
    if (return_id) {
        if (length(id) == 0) {
            id = NA
        }
        return (id)
        
    } else {
        if (length(id) == 0) {
            color = NA
        } else {
            color = Palette[id]
        }
        return (color)
    }
}

#' @title get_colors
#' @export
get_colors = function (Value, upBin, lowBin, Palette,
                       Palette_layers=NULL,
                       include_min=FALSE,
                       include_max=TRUE) {
    
    colors = unlist(sapply(Value, get_color,
                           upBin=upBin,
                           lowBin=lowBin,
                           Palette=Palette,
                           include_min=include_min,
                           include_max=include_max))

    if (!is.null(Palette_layers)) {
        palette_match = match(colors, Palette)
        palette_matchNoNA = palette_match[!is.na(palette_match)]
        layers = Palette_layers[palette_match]
        res = list(colors=colors, layers=layers)
    } else {
        res = colors
    }
    return (res)
} 


#RdBu

#' @title Get palette
#' @export
get_palette = function (palette_name="BrBG", colorStep=10,
                        reverse=FALSE) {

    if (length(palette_name) > 1) {
        Palette = palette_name
    } else {
        Palette = RColorBrewer::brewer.pal(n=colorStep,
                                           name=palette_name)
    }
    Palette = colorRampPalette(Palette)(colorStep)
    if (reverse) {
        Palette = rev(Palette)
    }
    return (Palette)
}


#' @title Color event
#' @export
get_colorEvent = function () {
    colorEvent = c("#423089", "#9ed6e3", "#9dc544", "#ed6e6c")
    names(colorEvent) = c("Crue", "Crue Nivale", "Moyennes Eaux", "Étiage")
    return(colorEvent)
}

#' @title Text color event
#' @export
get_colorTextEvent = function () {
    colorTextEvent = c("#9687d5", "#d8eff4", "#cee2a2", "#f6b6b5")
    names(colorTextEvent) = c("Crue", "Crue Nivale", "Moyennes Eaux", "Étiage")
    return(colorTextEvent)
}

#' @title Switch color label
#' @export
switch_color = function (color, color_to_switch) {
    #switch 12% https://mdigi.tools/darken-color/#f6e8c3
    color = toupper(color)
    color_to_switch = toupper(color_to_switch)
    if (color %in% color_to_switch) {
        color = names(color_to_switch)[color_to_switch == color]
    }
    return (color)
}

#' @title Get reverse
#' @export
get_reverse = function (var) {
    reverse = FALSE
    if (grepl('(^fin)|([{]fin)', var) |
        grepl('(^dt)|([{]dt)', var) |
        grepl('(^t)|([{]t)', var) |
        grepl('(^v)|([{]v)', var)) {
        reverse = TRUE
    }
    return (reverse)
}
