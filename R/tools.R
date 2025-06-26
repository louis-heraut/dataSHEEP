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


## 1. PERSONAL PLOT __________________________________________________
### 1.1. Void plot ___________________________________________________
# A plot completly blank
#' @title Void plot
#' @export
void = function (panel.background_fill=NA,
                 plot.margin=margin(t=0, r=0, b=0, l=0, "mm")) {

    plot = ggplot() + theme_void() +
        
        theme(plot.margin=plot.margin,
              panel.background=element_rect(fill=panel.background_fill,
                                            color=NA))
    
    return (plot)
}

### 1.2. Contour void plot ___________________________________________
# A plot completly blank with a contour
#' @title Contour plot
#' @export
contour = function (panel.background_fill=NA) {
    
    plot = ggplot() + theme_void() +
        
        theme(plot.margin=margin(t=0, r=0, b=0, l=0, unit="mm"),
              panel.background=element_rect(fill=panel.background_fill,
                                            color="#EC4899"))
    
    return (plot)
}

# add_contour = function () {
#     return (theme(plot.background=element_rect(fill=NA, color="#EC4899")))
# }



### 1.3. Circle ______________________________________________________
# Allow to draw circle in ggplot2 with a radius and a center position
#' @title Circle
#' @export
gg_circle = function(r, xc, yc, color="black", fill=NA, ...) {
    x = xc + r*cos(seq(0, pi, length.out=100))
    ymax = yc + r*sin(seq(0, pi, length.out=100))
    ymin = yc + r*sin(seq(0, -pi, length.out=100))
    annotate("ribbon", x=x, ymin=ymin, ymax=ymax, color=color,
             fill=fill, ...)
}

#' @title nbsp
#' @export
nbsp = function (n, size=NA) {
    if (is.na(size)) {
        paste0(rep("<span> </span>", times=n), collapse="")
    } else {
        paste0(rep(paste0("<span style='font-size:", size,
                          "pt'> </span>"), times=n),
               collapse="")
    }
}


## 3. NUMBER MANAGEMENT ______________________________________________
### 3.1. Number formatting ___________________________________________
# Returns the power of ten of the scientific expression of a value


### 3.2. Pourcentage of variable _____________________________________
# Returns the value corresponding of a certain percentage of a
# data serie
#' @title Pourcentage of variable
#' @export
gpct = function (pct, L, min_lim=NULL, shift=FALSE) {

    # If no reference for the serie is given
    if (is.null(min_lim)) {
        # The minimum of the serie is computed
        minL = min(L, na.rm=TRUE)
        # If a reference is specified
    } else {
        # The reference is the minimum
        minL = min_lim
    }

    # Gets the max
    maxL = max(L, na.rm=TRUE)
    # And the span
    spanL = maxL - minL
    # Computes the value corresponding to the percentage
    xL = pct/100 * as.numeric(spanL)

    # If the value needs to be shift by its reference
    if (shift) {
        xL = xL + minL
    }
    return (xL)
}


#' @title round_label
#' @export
round_label = function (labelRaw, direction="V", ncharLim=4) {
    labelRaw = round(labelRaw, 10)
    if (direction == "V") {
        label2 = signif(labelRaw, 2)
        label2[label2 >= 0] = paste0(" ", label2[label2 >= 0])
        label1 = signif(labelRaw, 1)
        label1[label1 >= 0] = paste0(" ", label1[label1 >= 0])
        label = label2
        label[nchar(label2) > ncharLim] =
            label1[nchar(label2) > ncharLim]
    } else if (direction == "H") {
        label2 = signif(labelRaw, 2)
        label1 = signif(labelRaw, 1)
        nCharLabel2 = nchar(label2)
        nCharLabel2[nCharLabel2 >= 0] =
            nCharLabel2[nCharLabel2 >= 0] + 1
        label = label2
        label[nCharLabel2 > ncharLim] = label1[nCharLabel2 > ncharLim]
    }
    return (label)
}

#' @title is.wholenumber
#' @export
is.wholenumber = function (X, tol=.Machine$double.eps^0.5) {
    res = abs(X - round(X)) < tol
    return (res)
}

#' @title chr2op
#' @export
chr2op = function (x) {
    res = eval(parse(text=x))
    return (res)
}

#' @title float2frac
#' @export
float2frac = function (X, den) {
    Frac = paste0(round(X * den), "/", den)
    evalFrac = sapply(X, chr2op)
    OK = is.wholenumber(evalFrac)
    Frac[OK] = evalFrac[OK]
    return (Frac)
}

## 4. LOADING ________________________________________________________
### 4.1. Shapefile loading ___________________________________________
#' @title Shapefiles loading
#' @description  Generates a list of shapefiles to draw a hydrological
#' map of the France
#' @param resources_path Path to the resources directory.
#' @param france_dir Directory you want to use in ash\\resources_path\\
#' to get the France shapefile.
#' @param france_file Name of the France shapefile.
#' @param bassinHydro_dir Directory you want to use in ash\\resources_path\\
#' to get the hydrological basin shapefile.
#' @param bassinHydro_file Name of the hydrological basin shapefile.
#' @param regionHydro_dir Directory you want to use in
#' ash\\resources_path\\ to get the hydrological sub-basin shapefile.
#' @param regionHydro_file Name of the hydrological sub-basin shapefile.
#' @param river_dir Directory you want to use in ash\\resources_path\\
#' to get the hydrological network shapefile.
#' @param river_file  Name of the hydrological network shapefile.
#' @param show_river Boolean to indicate if the shapefile of the
#' hydrological network will be charge because it is a heavy one and
#' that it slows down the entire process (default : TRUE)
#' @return A list of shapefiles converted as tibbles that can be plot
#' with 'geom_polygon' or 'geom_path'.
#' @export
load_shapefile = function (computer_shp_path, Code=NULL,
                           europe_shp_path=NULL,
                           france_shp_path=NULL,
                           bassinHydro_shp_path=NULL,
                           regionHydro_shp_path=NULL,
                           secteurHydro_shp_path=NULL,
                           entiteHydro_shp_path=NULL,
                           entitePiezo_shp_path=NULL,
                           MESO_shp_path=NULL,
                           river_shp_path=NULL,
                           river_class=NULL,
                           river_length=NULL,
                           river_selection=NULL,
                           toleranceRel=10000) {

    # Europe
    if (!is.null(europe_shp_path)) {
        europe_path = file.path(computer_shp_path,
                                europe_shp_path)
        europe = sf::st_read(europe_path)
        europe = sf::st_transform(europe, 2154)
        europe = sf::st_simplify(europe,
                             preserveTopology=TRUE,
                             dTolerance=toleranceRel)
    } else {
        europe = NULL
    }

    
    # France
    if (!is.null(france_shp_path)) {
        france_path = file.path(computer_shp_path,
                                france_shp_path)
        france = sf::st_read(france_path)
        france = sf::st_union(france)
        france = sf::st_transform(france, 2154)
        france = sf::st_simplify(france,
                             preserveTopology=TRUE,
                             dTolerance=toleranceRel)
    } else {
        france = NULL
    }

    # Hydrological basin
    if (!is.null(bassinHydro_shp_path)) {
        bassinHydro_path = file.path(computer_shp_path,
                                    bassinHydro_shp_path)
        bassinHydro = sf::st_read(bassinHydro_path)
        bassinHydro = sf::st_transform(bassinHydro, 2154)
        bassinHydro = sf::st_simplify(bassinHydro,
                                 preserveTopology=TRUE,
                                 dTolerance=toleranceRel*0.6)
    } else {
        bassinHydro = NULL
    }

    # Hydrological sub-basin
    if (!is.null(regionHydro_shp_path)) {
        regionHydro_path = file.path(computer_shp_path,
                                     regionHydro_shp_path)
        regionHydro = sf::st_read(regionHydro_path)
        regionHydro = sf::st_make_valid(regionHydro)
        regionHydro = sf::st_transform(regionHydro, 2154)
        regionHydro = sf::st_simplify(regionHydro,
                                  preserveTopology=TRUE,
                                  dTolerance=toleranceRel*0.6)
    } else {
        regionHydro = NULL
    }

    # Hydrological sector
    if (!is.null(secteurHydro_shp_path)) {
        secteurHydro_path = file.path(computer_shp_path,
                                      secteurHydro_shp_path)
        secteurHydro = sf::st_read(secteurHydro_path)
        secteurHydro = sf::st_make_valid(secteurHydro)
        secteurHydro = sf::st_transform(secteurHydro, 2154)
        secteurHydro = sf::st_simplify(secteurHydro,
                                   preserveTopology=TRUE,
                                   dTolerance=toleranceRel*0.6)
    } else {
        secteurHydro = NULL
    }
    
    # Hydrological code bassin
    if (!is.null(entiteHydro_shp_path)) {
        entiteHydro_path = file.path(computer_shp_path,
                                     entiteHydro_shp_path)
        entiteHydro_list = lapply(entiteHydro_path, read_sf)
        entiteHydro_list = lapply(entiteHydro_list, sf::st_transform, 2154)
        entiteHydro = do.call(rbind, entiteHydro_list)
        entiteHydro = dplyr::rename(entiteHydro, code=Code)
        entiteHydro = entiteHydro[entiteHydro$code %in% Code,]
        entiteHydro = sf::st_simplify(entiteHydro,
                                  preserveTopology=TRUE,
                                  dTolerance=toleranceRel*0.4)
    } else {
        entiteHydro = NULL
    }
    
    # Piezo entity
    if (!is.null(entitePiezo_shp_path)) {
        entitePiezo_path = file.path(computer_shp_path,
                                     entitePiezo_shp_path)
        entitePiezo = sf::st_read(entitePiezo_path)
        entitePiezo = sf::st_transform(entitePiezo, 2154)
        entitePiezo = sf::st_simplify(entitePiezo,
                                  preserveTopology=TRUE,
                                  dTolerance=toleranceRel*0.6)
    } else {
        entitePiezo = NULL
    }    


    # MESO
    if (!is.null(MESO_shp_path)) {
        MESO_path = file.path(computer_shp_path,
                              MESO_shp_path)
        MESO = sf::st_read(MESO_path)
        MESO = sf::st_make_valid(MESO)
        MESO = sf::st_transform(MESO, 2154)
        MESO = sf::st_simplify(MESO,
                               preserveTopology=TRUE,
                               dTolerance=toleranceRel*0.25)
    } else {
        MESO = NULL
    }
    
    # If the river shapefile needs to be load
    if (!is.null(river_shp_path)) {
        river_path = file.path(computer_shp_path,
                               river_shp_path)
        # Hydrographic network
        river = sf::st_read(river_path)
        river = sf::st_transform(river, 2154)
        
        if (!is.null(river_class)) {
            river = river[river$Classe %in% river_class,]

        }
        if (!is.null(river_length)) {
            river$length = as.numeric(sf::st_length(river$geometry))
            river = river[river$length >= river_length,]
        }
        river = river[!grepl(paste(c("canal", "Canal"), collapse='|'),
                             river$TopoOH),]
        if (!is.null(river_selection)) {
            river = river[grepl(paste(river_selection, collapse='|'),
                                river$TopoOH),]
        }
        
        river = sf::st_simplify(river,
                            preserveTopology=TRUE,
                            dTolerance=toleranceRel*0.4) 
    } else {
        river = NULL
    }

    return (list(europe=europe,
                 france=france,
                 bassinHydro=bassinHydro,
                 regionHydro=regionHydro,
                 secteurHydro=secteurHydro,
                 entiteHydro=entiteHydro,
                 entitePiezo=entitePiezo,
                 MESO=MESO,
                 river=river))
}

# ### 4.2. Logo loading ________________________________________________
# #' @title Logo loading
# #' @export
# load_logo = function (resources_path, logo_dir, logo_to_show) {
#     logo_path = c()
#     nLogo = length(logo_to_show)
#     for (i in 1:nLogo) { 
#         logo_path = c(logo_path, file.path(resources_path,
#                                            logo_dir,
#                                            logo_to_show[i]))
#         names(logo_path)[length(logo_path)] = names(logo_to_show)[i]
#     }
#     return (logo_path)
# }

### 4.3. Font loading ________________________________________________
#' @title load_font
#' @export
load_font = function (path=NULL, force_import=FALSE) {

    extrafont::font_import(paths=path)
    
    # if (is.null(extrafont::fonts()) | force_import) {
    # remotes::install_version("Rttf2pt1", version = "1.3.8")
    # extrafont::font_import(paths=path)
    # }
    # extrafont::loadfonts(device="all", quiet=TRUE)
    # theme = theme(text=element_text(family="frutiger-57-condensed"))
}


## 5. OTHER __________________________________________________________
#' @title Split filename
#' @export
splitext = function(file) { # tools::file_ext
    ex = strsplit(basename(file), split="\\.")[[1]]
    res = list(name=ex[1], extension=ex[2])
    return (res)
}

#' @title Split path
#' @export
split_path = function (path) {
    if (dirname(path) %in% c(".", path)) return(basename(path))
    return(c(basename(path), split_path(dirname(path))))
}


# X2px(unlist(strsplit(text, "")), PX)

# PX = get_alphabet_in_px(save=TRUE)

# Span = lapply(strsplit(Model, "*"), X2px, PX=PX)
# Span = lapply(Span, sum)
# Span = unlist(Span)

#' @title plotly_save
#' @export
plotly_save = function (fig, path) {
    htmlwidgets::saveWidget(fig,
                            file=path,
                            selfcontained=TRUE)
    libdir = paste0(tools::file_path_sans_ext(basename(path)), "_files")
    unlink(file.path(dirname(path), libdir), recursive=TRUE)
}

#' @title strsplit_unlist
#' @export
strsplit_unlist = function (...) {unlist(strsplit(...))}

#' @title get_alphabet_in_px
#' @export
get_alphabet_in_px = function (alphabet=c(letters, LETTERS,
                                          c("é", "è", "à", "ç",
                                            "É", "È", "À", "Ç"),
                                          c("1", "2", "3", "4",
                                            "5", "6", "7", "8",
                                            "9", "0"),
                                          c("-", "_", ".", ",",
                                            "*", "'", "%", "(",
                                            ")", "[", "]", "{",
                                            "}", "!", "?", "+",
                                            "=", "@", "|", "#",
                                            "&")),
                               size=50, font="sans",
                               style="normal",
                               isNorm=TRUE,
                               out_dir="letter",
                               save=FALSE) {
        
    library(magick)
    if (save &!dir.exists(out_dir)) {
        dir.create(out_dir)
    }
    find_id = function (X, a, where="") {
        if (any(a %in% X)) {
            id = which(X == a)
            if (where == "first") {
                id = id[1]
            } else if (where == "last") {
                id = id[length(id)]
            }
            return (id)
        } else {
            return (NA)
        }
    }

    if (style == "bold") {
        weight = 700
    } else {
        weight = 400
    }
    
    PX = c()
    for (letter in alphabet) {
        img = image_blank(width=size, height=size, color="white")
        img = image_annotate(img, letter, size=size, style="normal",
                             weight=weight, font=font, color="#000000")
        pixels = as.character(c(image_data(img, channel="gray")))
        pixels[pixels != "ff"] = "1"
        pixels[pixels == "ff"] = "0"
        pixels = as.numeric(pixels)
        pixels = matrix(pixels, ncol=size, byrow=TRUE)
        if (save) {
            write.table(pixels,
                        file=file.path(out_dir,
                                       paste0(letter, ".txt")),
                        row.names=FALSE, col.names=FALSE)
        }
        # firsf::
            st_one = apply(pixels, 1, find_id, a=1, where="first")
        # lasf::
            st_one = apply(pixels, 1, find_id, a=1, where="last")
        px = max(
            # lasf::
                 st_one, na.rm=TRUE) -
            min(
                # firsf::
                st_one, na.rm=TRUE) + 1
        PX = c(PX, px)
        names(PX)[length(PX)] = letter
    }
    PX = c(PX, PX["_"])
    names(PX)[length(PX)] = ' '
    if (isNorm) {
        PX = PX/max(PX)
    }
    return (PX)
}

#' @title text2px
#' @export
text2px = function (text, PX) {
    text = unlist(strsplit(text, ""))
    px = PX[text]
    px[is.na(px)] = mean(px, na.rm=TRUE)
    px = sum(px, na.rm=TRUE)
    return (px)
}

#' @title char2px
#' @export
char2px = function (char, PX) {
    px = PX[char]
    px[is.na(px)] = mean(px, na.rm=TRUE)
    return (px)
}


# select_good = function (X) {
#     Xrle = rle(X)
#     value = Xrle$values[Xrle$lengths == max(Xrle$lengths)]
#     if (length(value) > 1) {
#         value = mean(value, na.rm=TRUE)
#     }
#     return (value)
# }

#' @title guess_newline
#' @export
guess_newline = function (text, px=40, nChar=100,
                          PX=NULL, newlineId="\n") {

    if (is.null(px)) {
        lim = nChar
        estimator = nchar
    } else {
        lim = px
        if (is.null(PX)) {
            PX = get_alphabet_in_px()
        }
        estimator = function (text) {
            text2px(text, PX=PX)
        }
    }

    text = paste0(text, " ")

    Newline = text
    distance = estimator(Newline)
    begin = 0
    
    while (distance > lim & sum(grepl(" ", text)) > 0) {        
        posSpace = which(strsplit(Newline, "")[[1]] == " ")
        posSpace_distance = lapply(posSpace, substr, x=Newline, start=1)
        posSpace_distance = sapply(posSpace_distance, estimator)
        idNewline = which.min(abs(posSpace_distance - lim))
        posNewline = posSpace[idNewline] + begin
        text = paste(substring(text,
                               c(1, posNewline + 1),
                               c(posNewline - 1,
                                 nchar(text))),
                     collapse=newlineId)
        if (sum(grepl(" ", text)) == 0) {
            break
        }
        Newline = substr(text,
                         posNewline + 1,
                         nchar(text))
        distance = estimator(Newline)
        begin = nchar(text) - nchar(Newline)
    }
    
    return (text)
}


#' @title convert2TeX
#' @export
convert2TeX = function (Var, size=NULL, is_it_small=FALSE, replace_space=FALSE, bold=TRUE) {
    nVar = length(Var)

    if (is_it_small) {
        ita = "\\\\small{"
        itb = "}"
    } else {
        ita = ""
        itb = ""
    }

    VarTEX = gsub("etiage", "étiage", Var)
    
    for (i in 1:nVar) {
        var = VarTEX[i]

        if (grepl("[_]", var) & !grepl("[_][{]", var)) {
            var = gsub("[_]", ", ", var)
            var = sub("[,] ", "$_{$", var)
            var = paste0(var, "$}$")           
        } else if (grepl("[_]", var) & grepl("[_][{]", var)) {
            var = gsub("[_]", ", ", var)
            var = sub("[,] [{]", "$_{$", var)
            var = sub("[}] ", "$}$", var)
        }
        # if (grepl("[_]", var) & !grepl("[_][{]", var)) {
        #     var = gsub("[_]", ", ", var)
        #     var = sub("[,] ", "$_{$", var)
        #     var = paste0(var, "}")           
        # } else if (grepl("[_]", var) & grepl("[_][{]", var)) {
        #     var = gsub("[_]", ", ", var)
        #     var = sub("[,] [{]", "$_{$", var)
        # }

        if (grepl("\\^[{]", var)) {
            var = gsub("\\^[{]", "$^{$", var)
            var = gsub("[}]", "$}$", var)
        }
        # if (grepl("\\^[{][$][-]", var)) {
        # var = gsub("\\^[{][$][-]", "^{-$", var)
        # }
        if (grepl("\\^[[:alnum:]]", var)) {
            var = gsub("\\^", "$^$", var)
        }

        if (grepl("alpha", var)) {
            var = gsub("alpha", "\\\\bf{\u03b1}", var)
        }

        if (grepl("epsilon", var)) {
            var = gsub("epsilon", "\\\\bf{\u03b5}", var)
        }

        if (grepl("HYP", var)) {
            var = gsub("HYP", "H", var)
        }

        if (grepl("inv", var) & !grepl("inv[{]", var)) {
            var = gsub("inv",
                       paste0(ita, "\\\\textit{inv}", itb),
                       var)
        } else if (grepl("inv", var) & grepl("inv[{]", var)) {
            var = gsub("[}]", "", var)
            var = gsub("inv[{]", 
                       paste0(ita, "\\\\textit{inv}", itb),
                       var)
        } 

        if (grepl("log", var) & !grepl("log[{]", var)) {
            var = gsub("log", 
                       paste0(ita, "\\\\textit{log}", itb),
                       var)
        } else if (grepl("log", var) & grepl("log[{]", var)) {
            var = gsub("[}]", "", var)
            var = gsub("log[{]", 
                       paste0(ita, "\\\\textit{log}", itb),
                       var)
        } 

        if (grepl("moy", var) & !grepl("moy[{]", var)) {
            var = gsub("moy", 
                       paste0(ita, "\\\\textit{moy}", itb),
                       var)
        } else if (grepl("moy", var) & grepl("moy[{]", var)) {
            var = gsub("[}]", "", var)
            var = gsub("moy[{]", 
                       paste0(ita, "\\\\textit{moy}", itb),
                       var)
        } 

        if (grepl("med", var) & !grepl("med[{]", var)) {
            var = gsub("med", 
                       paste0(ita, "\\\\textit{med}", itb),
                       var)
        } else if (grepl("med", var) & grepl("med[{]", var)) {
            var = gsub("[}]", "", var)
            var = gsub("med[{]", 
                       paste0(ita, "\\\\textit{med}", itb),
                       var)
        } 
        
        if (grepl("racine", var) & !grepl("racine[{]", var)) {
            var = gsub("racine", "\u221A", var)
        } else if (grepl("racine", var) & grepl("racine[{]", var)) {
            var = gsub("[}]", "", var)
            var = gsub("racine[{]", "\u221A", var)
        }

        if (grepl("ips", var) & !grepl("ips[{]", var)) {
            var = gsub("ips", 
                       paste0(ita, "\\\\textit{ips}", itb),
                       var)
        } else if (grepl("ips", var) & grepl("ips[{]", var)) {
            var = gsub("[}]", "", var)
            var = gsub("ips[{]", 
                       paste0(ita, "\\\\textit{ips}", itb),
                       var)
        }

        if (grepl("biais", var) & !grepl("biais[{]", var)) {
            var = gsub("biais", 
                       paste0(ita, "\\\\textit{biais}", itb),
                       var)
        } else if (grepl("biais", var) & grepl("biais[{]", var)) {
            var = gsub("[}]", "", var)
            var = gsub("biais[{]", 
                       paste0(ita, "\\\\textit{biais}", itb),
                       var)
        }

        if (replace_space) {
            var = gsub(" ", "\\\\,", var)
        }
        
        VarTEX[i] = var
    }

    if (!is.null(size)) {
        VarTEX = paste0("\\", size, "{", VarTEX, "}")
    }
    
    if (bold) {
        VarTEX = paste0("\\textbf{", VarTEX, "}")
    }
    return (VarTEX)
}





get_breaks_function = function (breaks, isDate=TRUE,
                                d_breaks=0,
                                break_round=-1,
                                add_breaks=NULL,
                                rm_breaks=NULL) {

    get_breaks = function (X) {
        if (isDate) {
            Xmin = round(lubridate::year(min(X)), break_round)
            Xmax = round(lubridate::year(max(X)), break_round)
            if (Xmax-Xmin <= 1) {
                Xmin = lubridate::year(X)[1]
                Xmax = lubridate::year(X)[1] + 1
            }
            res = seq.Date(from=as.Date(paste0(Xmin, "-01-01")) +
                               d_breaks,
                           to=as.Date(paste0(Xmax, "-01-01")) +
                               d_breaks,
                           by=breaks)
        } else {
            Xmin = round(min(X), break_round)
            Xmax = round(max(X), break_round)
            res = seq(from=Xmin + d_breaks,
                      to=Xmax + d_breaks,
                      by=breaks)
        }

        if (!is.null(add_breaks)) {
            res = sort(c(res, add_breaks))
        }

        if (!is.null(rm_breaks)) {
            res = res[!(res %in% rm_breaks)]
        }

        return (res)
    }

    return (get_breaks)
}


to_link = function (x) {
    gsub("ç", "c", 
         gsub("à", "a",
              gsub("é|è|ê|ë", "e",
                   gsub(" ", "_", tolower(x)))))
}



spline_to_date = function (data, Xname, Yname, na.rm=FALSE, ...) {
    isNA = is.na(data[[Yname]])
    X = as.numeric(seq.Date(min(data[[Xname]], na.rm=TRUE),
                            max(data[[Xname]], na.rm=TRUE),
                            "years"))
    SS = predict(smooth.spline(as.numeric(data[[Xname]][!isNA]),
                               data[[Yname]][!isNA], ...),
                 X)
    if (!na.rm) {
        SS$y[isNA] = NA  
    }
    data = dplyr::tibble(!!Xname:=as.Date(SS$x),
                         !!Yname:=SS$y)
    return (data)
}


get_regexp = function (X) {
    X = paste0("(", paste0(X, collapse=")|("), ")")
    X = gsub("[_]", "[_]", X)
    X = gsub("[-]", "[-]", X)
    X = gsub("[{]", "[{]", X)
    X = gsub("[}]", "[}]", X)
    return (X)
}



get_pattern_block = function (block) {
    get_regexp(paste0(rep(c("^", "[.]"), 2),
                      gsub("[.]", "[.]", block),
                      rep(c("[.]", "$"), each=2)))
}
