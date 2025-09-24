

# Google scholar search --------------------------------------------------------

## Download data ---------------------------------------------------------------

# https://jef.works/blog/2024/03/18/querying-google-scholar-with-rvest/
library(rvest)

yrs <- 1960:2024

searches <- data.frame()

searches <- dplyr::bind_rows(
  searches, 
  dplyr::bind_rows(searches, 
                   data.frame(type = "Bottom trawls", 
                              search = ' "bottom trawl" OR "beam trawl" OR "otter trawl" '))) 

searches <- dplyr::bind_rows(
  searches, 
  data.frame(type = "Acoustic", 
             search = ' "acoustic" ')) # "mooring" OR "drone" OR "hydrophone" OR "saildrone" 

searches <- dplyr::bind_rows(
  searches, 
  data.frame(type = "Mid-water trawls", 
             search = ' "Mid water trawl" ')) # OR "Mid-water trawl" OR "Midwater trawl"

searches <- dplyr::bind_rows(
  searches, 
  data.frame(type = "Dredge", 
             search = ' "dredge" '))

searches <- dplyr::bind_rows(
  searches, 
  data.frame(type = "eDNA", 
             search = ' "edna" ')) #  OR "e-dna"

searches <- dplyr::bind_rows(
  searches, 
  data.frame(type = "Stationary gear", 
             search = ' "stationary gear" OR "pot" OR "trap" OR "hook and line" OR "longline" ')) # OR "HOOK & LINE"

searches <- dplyr::bind_rows(
  searches, 
  data.frame(type = "Visual methods", 
             search = ' "camera" OR "scuba" OR "snorkel" OR "video" OR "HABCAM" '))

searches <- dplyr::bind_rows(
  searches, 
  data.frame(type = "Mark-recapture", 
             search = ' "mark recapture" OR "mark recovery" OR "tagging" OR "tracking" ')) # OR "ptag" OR "foy tag" 

searches <- dplyr::bind_rows(
  searches, 
  data.frame(type = "Aerial surveys", 
             search = ' "drone" OR "lidar" OR "airplane" OR "helicopter" OR "aerial" ' )) #  -"pollution" -"oil" -"oil spill" 

searches <- dplyr::bind_rows(
  searches, 
  data.frame(type = "All FIS0", 
             search =  ' "Fishery independent survey" OR "Fisheries independent survey" OR "Fishery independent data" OR "Fisheries-independent data" OR "fish survey" OR "ecosystem survey" ' ))

searches <- dplyr::bind_rows(
  searches, 
  data.frame(type = "All FIS", 
             search = paste0(
               searches$search[searches$type == "All FIS0"], 
               # " ( ", 
               paste0(searches$search[searches$type != "All FIS0"], collapse = " OR "))#, 
             # " ) "
  ) )

searches$search[!(searches$type %in% c("All FIS0", "All FIS"))] <- 
  paste0(searches$search[searches$type == "All FIS0"], 
         # ifelse(grepl(pattern = " OR ", 
         #              x = searches$search[!(searches$type %in% c("All FIS0", "All FIS"))]), 
         # " AND ( ", ""),
         # " OR ", 
         searches$search[!(searches$type %in% c("All FIS0", "All FIS"))]#, 
         # ifelse(grepl(pattern = " OR ", 
         #              x = searches$search[!(searches$type %in% c("All FIS0", "All FIS"))]), ") ", "")
  ) 

searches <- searches[searches$type != "All FIS0", ]

searches$terms <- searches$search

searches$search <- gsub(pattern = '  ', replacement = " ", x = searches$search)
searches$search <- gsub(pattern = '  ', replacement = " ", x = searches$search)
searches$search <- gsub(pattern = '"', replacement = "%22", x = searches$search)
searches$search <- gsub(pattern = ' OR ', replacement = "|", x = searches$search)
# searches$search <- gsub(pattern = ' AND ', replacement = "+", x = searches$search)
searches$search <- gsub(pattern = ' ', replacement = "+", x = searches$search)

for (searches0 in c(1:length(searches$search))) {
  numpubs <- c()
  print(searches0)
  for (yr in yrs) {
    print(yr)
    url <- paste0("https://scholar.google.com/scholar?q=",searches$search[searches0],"&as_ylo=",yr,"&as_yhi=", yr)
    
    page <- read_html(url) |>
      html_nodes(".gs_ab_mdw") |>
      html_text()
    
    numpubs <- dplyr::bind_rows(
      numpubs, 
      data.frame("type" = searches$type[searches0], 
                 "search" = searches$search[searches0], 
                 "terms" = searches$terms[searches0], 
                 "year" = yr, 
                 "pubs" = as.numeric(gsub(",", "", stringr::str_extract(page[[2]], "\\d{1,3}(,\\d{3})*"))),
                 "search_date" = Sys.time()))
    
  }
  write.csv(x = numpubs, file = here::here("data", paste0("numpubs_", searches0,".csv")))
  
}

## Load data -------------------------------------------------------------------

# options(scipen = 999)

numpubs0 <- c()
for (i in c(1:10)) {
  numpubs0 <- dplyr::bind_rows(
    numpubs0, 
    read.csv(file = here::here("data", paste0("numpubs_",i,".csv"))) |> 
      dplyr::select(type, year, pubs))
}

## Wrangle data ----------------------------------------------------------------

# add year_bin

year_bin <- c()
for (i in unique(numpubs0$year)) {
  year_bin <- c(year_bin, 
                paste0( 
                  ifelse(as.numeric(substr(x = i, start = 4, stop = 4)) >= 5,  
                         as.numeric(paste0(substr(x = i, start = 1, stop = 3), 5)), #ifelse(i == 2024, 4, 5))), 
                         as.numeric(paste0(substr(x = i, start = 1, stop = 3), 0)) ), 
                  "-\n", 
                  ifelse(as.numeric(substr(x = i, start = 4, stop = 4)) >= 5,  
                         as.numeric(paste0(substr(x = i, start = 1, stop = 3), 9)), 
                         as.numeric(paste0(substr(x = i, start = 1, stop = 3), 4)) )
                ) )
}

# order by when cumsum > 99
numpubs0 <- numpubs0 |> 
  dplyr::mutate(
      pubs_cumsum = pubs, 
      pubs_cumsum = ifelse(is.na(pubs_cumsum), 0, pubs_cumsum),
  ) |> 
  dplyr::group_by(type) |> 
  dplyr::mutate(pubs_cumsum = cumsum(pubs_cumsum)) |> 
  dplyr::ungroup() |> 
  dplyr::left_join(data.frame(year = unique(numpubs0$year), 
                              year_bin = year_bin)) |> 
  dplyr::filter(pubs_cumsum > 0)

# order by regular min year
# type_order <- c("All FIS", 
#                 numpubs0 |> 
#   dplyr::filter(!is.na(pubs)) |> 
#   dplyr::filter(type != "All FIS") |> 
#   dplyr::group_by(type) |> 
#   dplyr::summarise(year_min = min(year, na.rm = TRUE)) |> 
#   dplyr::ungroup() |> 
#   dplyr::arrange((year_min)) |> 
#   dplyr::select(type) |> 
#   unlist())
# 
# numpubs0$type <- factor(x = numpubs0$type,
#                         levels = type_order, 
#                         labels = type_order, 
#                         ordered = TRUE)

# cumsum to first 100
type_order <- c(
  "All FIS", 
  numpubs0 |> 
    dplyr::filter(!is.na(pubs)) |> 
    dplyr::filter(type != "All FIS") |> 
    dplyr::filter(pubs_cumsum > 99) |> 
    dplyr::group_by(type) |> 
    dplyr::summarise(year_min = min(year, na.rm = TRUE)) |> 
    dplyr::ungroup() |> 
    dplyr::arrange((year_min)) |> 
    dplyr::select(type) |> 
    unlist())

numpubs0$type <- factor(x = numpubs0$type,
                        levels = type_order, 
                        labels = type_order, 
                        ordered = TRUE)

## Plot figures ----------------------------------------------------------------


library(ggplot2)
library(cowplot)
library(scales)

plot_figure <- function(numpubs0, 
                        log_a = FALSE, 
                        allfis_a = FALSE, 
                        yr5bin_b = FALSE, 
                        cumsum0 = FALSE, 
                        voption = "D") {
  
  dat <- numpubs0 |> 
    # dplyr::filter(year != 2024) |>
    dplyr::filter(!(type %in% 
                      c("Worldwide BTS", "NOAA BTS", ifelse(allfis_a, "All FIS", NA))))
  if (cumsum0) {
    pp1 <- ggplot2::ggplot(
      data = dat,
      mapping = aes(x = year, y = pubs_cumsum, color = type)) 
  } else {
    pp1 <- ggplot2::ggplot(      
      data = dat,
      mapping = aes(x = year, y = pubs, color = type)) 
  }
  
  pp1 <- pp1 + 
    ggplot2::geom_line(
      linewidth = 1, 
      na.rm = TRUE, 
      show.legend = TRUE) + 
    ggplot2::scale_color_viridis_d(option = voption, name = "") 
  
  if (allfis_a) {
    if (cumsum0) {
      pp1 <- pp1 +
        ggplot2::geom_point(
          data = numpubs0 |>
            # dplyr::filter(year != 2024) |>
            dplyr::filter(type == "All FIS"),
          mapping = aes(x = year, y = pubs_cumsum , fill = type), 
          size = 2,
          show.legend = TRUE) +
        ggplot2::scale_fill_grey(name = " ")      
    } else {
    pp1 <- pp1 +
      ggplot2::geom_point(
        data = numpubs0 |>
          # dplyr::filter(year != 2024) |>
          dplyr::filter(type == "All FIS"),
        mapping = aes(x = year, y = pubs , fill = type), 
        size = 2,
        show.legend = TRUE) +
      ggplot2::scale_fill_grey(name = " ")
  } 
  }
  
  if (log_a) {
    # pp1 <- pp1 + ggplot2::scale_y_continuous(labels = scales::comma(), trans='log2', name = "")
    pp1 <- pp1 + 
      scale_y_continuous(
        trans = "log2",
        # labels = scales::math_format(2^.x, format = log2), 
        name = "", 
        expand = c(0, 0), 
          # trans = "log2", 
          # Use breaks_log for appropriate log scale breaks
          breaks = scales::breaks_log(n = 6, base = 2),
          # Use label_comma to format labels without scientific notation
        # labels = scales::breaks_log, 
          labels = label_comma(accuracy = 10) 
        )
      
    
    pp1 <- pp1 +
      ggplot2::scale_y_continuous(breaks = function(x) unique(floor(pretty(seq(min(x), (max(x) + 1) * 1.1)))),
                                  trans = 'log2',
                                  name = "",
                                  expand = c(0, 0))  # Removes expansion at both ends of the y-axis
    # pp1 <- pp1 + ggplot2::scale_y_log10(name = "",
    #   expand = c(0, 0))  # Removes expansion at both ends of the y-axis 
  } else {
    pp1 <- pp1 + 
      ggplot2::scale_y_continuous(labels = scales::comma, 
                                  name = "", 
                                  expand = c(0, 0))  # Removes expansion at both ends of the y-axis 
  }  

  # ggplot2::guides(fill = guide_legend( ) + 
  
  pp1 <- pp1  +
    ggplot2::guides(
      color = guide_legend(
        override.aes = list(shape = NA) , 
        title = "", 
        position = "bottom", 
        reverse = FALSE, 
        # drop = TRUE, 
        nrow = 1),
      fill = guide_legend(
        override.aes = list(linetype = c(0) ) , 
        title = "", 
        position = "bottom", 
        # reverse = TRUE,
        nrow = 1)) +  
    ggplot2::ggtitle(paste0("A) ", ifelse(cumsum0, "Cummulative ", ""), "Number of Publications", ifelse(log_a, " (log2 Transformed)", ""))) +
    ggplot2::xlab("Years") +
    # ggplot2::scale_x_continuous()
    ggplot2::theme_classic() + 
    ggplot2::theme(legend.box.margin = margin(0, 0, 0, 0), 
                   panel.grid.major.y = element_line(colour = "grey90", linewidth = 0.5)) 
  
  if (yr5bin_b) {
    pp2 <- ggplot2::ggplot(
      data = numpubs0 |> 
        dplyr::filter(type != "All FIS") |>
        dplyr::group_by(type, year_bin) |>
        dplyr::reframe(pubs = sum(pubs, na.rm = TRUE)) |>
        dplyr::ungroup(),
      # dplyr::select(-year_bin) |>
      # dplyr::rename(year_bin = year),
      mapping = aes(x = year_bin, y = pubs, fill = type)) 
  } else {
    pp2 <- ggplot2::ggplot(
      data = numpubs0 |> 
        dplyr::filter(type != "All FIS") |>
        dplyr::select(-year_bin) |>
        dplyr::rename(year_bin = year),
      mapping = aes(x = year_bin, y = pubs, fill = type))   
  }
  
  pp2 <- pp2 +  
    ggplot2::geom_bar(#position = "fill", 
      position = position_fill(reverse = TRUE), 
      stat="identity") + 
    ggplot2::scale_fill_viridis_d(option = voption) + 
    ggplot2::ggtitle("B) Percent of Publications Types (%)") +
    ggplot2::xlab("5-Year Bins") +
    ggplot2::theme_classic() + 
    ggplot2::scale_y_continuous(
      name = "", 
      expand = c(0, 0), 
      labels = function(x) paste0(x*100, "%") # labels = scales::percent
      ) + # Multiply by 100 & add %  
    ggplot2::guides(fill = guide_legend(
      title = "", 
      position = "bottom", 
      nrow = 1)) +  
    ggplot2::theme(legend.box.margin = margin(0, 0, 0, 0)) 
  
  # pp_legend <- cowplot::get_legend(pp2 + 
  #   ggplot2::guides(fill=guide_legend(position = "bottom")) )
  
  # if (allfis_a) {
  pp1_legend <- get_plot_component(
    plot = pp1, 
    "guide-box", 
    return_all = TRUE) # [[3]]
  # }
  
  # pp2_legend <- get_plot_component(
  #   plot = pp2, 
  #   "guide-box", 
  #   return_all = TRUE)[[3]]
  
  pp <- cowplot::plot_grid(
    pp1 + ggplot2::theme(legend.position = "none"), 
    pp2 + ggplot2::theme(legend.position = "none"), 
    # rel_heights = c(.9, 1), 
    # labels = c('A', 'B'), 
    label_size = 12)
  
  # if (allfis_a) {
  #   pp_legend <- cowplot::plot_grid("", pp1_legend, pp2_legend, "", nrow = 1, rel_widths = c(.3, .3, 1, .3))
  # } else {
  pp_legend <- pp1_legend
  # }
  
  pp <- cowplot::plot_grid(pp, pp_legend, ncol = 1, rel_heights = c(1, .1))
  
  ggsave(
    filename = paste0("numpubs", 
                      "_log_a", log_a, 
                      "_allfis_a", allfis_a, 
                      "_yr5bin_b", yr5bin_b, 
                      "_cumsum", cumsum0, 
                      "_voption", voption, 
                      ".png"), 
    plot = pp, 
    path = here::here("docs", "img"), 
    width = 12, 
    height = 6, 
    dpi = 300)
  
  return(pp)
}

# Okabe & Ito (2008) color-blind proof palette should have 9 categories. That, are align text labels with the lines and fill categories. The paired scheme (or similar) in ColorBrewer might be another option.

for (voption in c("D", "G", "E")) {
  p1 <- plot_figure(numpubs0 = numpubs0, 
                    log_a = FALSE, allfis_a = TRUE, yr5bin_b = TRUE, cumsum0 = FALSE, voption = voption)
  # p2 <- plot_figure(numpubs0 = numpubs0, 
  #                   log_a = FALSE, allfis_a = FALSE, yr5bin_b = FALSE, cumsum0 = FALSE, voption = voption) 
  p3 <- plot_figure(numpubs0 = numpubs0, 
                    log_a = TRUE, allfis_a = TRUE, yr5bin_b = TRUE, cumsum0 = FALSE, voption = voption) # this is the one we landed on
  # p4 <- plot_figure(numpubs0 = numpubs0, 
  #                   log_a = TRUE, allfis_a = FALSE, yr5bin_b = FALSE, cumsum0 = FALSE, voption = voption)
  
  p1c <- plot_figure(numpubs0 = numpubs0, 
                    log_a = FALSE, allfis_a = FALSE, yr5bin_b = TRUE, cumsum0 = TRUE, voption = voption) # this is the one we landed on
  # p2c <- plot_figure(numpubs0 = numpubs0, 
  #                   log_a = FALSE, allfis_a = FALSE, yr5bin_b = FALSE, cumsum0 = TRUE, voption = voption) 
  p3c <- plot_figure(numpubs0 = numpubs0, 
                    log_a = TRUE, allfis_a = TRUE, yr5bin_b = TRUE, cumsum0 = TRUE, voption = voption) 
  # p4c <- plot_figure(numpubs0 = numpubs0, 
  #                   log_a = TRUE, allfis_a = FALSE, yr5bin_b = FALSE, cumsum0 = TRUE, voption = voption)
}



