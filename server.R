library(ggplot2)
library(tidyverse)
library(shinyFiles)
library(cowplot)
library(shinyjs)
library(shinyalert)
library(plotly)
library(shinyBS)
library(gganimate)
library(shinycssloaders)
library(tools)
library(readxl)
library(lubridate)
# library(ggiraph)

# Functions:

plot_progression <- function(csv_path, unit, anc_range){

  # Reading patient csv sheet
  if(file_ext(csv_path) == "csv"){
    MR <- read.csv(csv_path)
    # print("path_read")
  }
  
  if(file_ext(csv_path) %in% c("xls", "xlsx")){
    MR <- readxl::read_excel(csv_path)
    # print("path_read")
  }
  
  # Identifying and rounding up (to 1000) the highest ANC value to use as y axis limit and starting dose
  mx <- round(max(MR$ANC), digits = -3)
  MP_start_dose <- MR$MP[1]
  MTX_start_dose <- MR$MTX[1]
  
  # Selecting columns of interest and melting dataframe together for plot
  melted <- dplyr::select(MR, Weeks, ANC, MP, MTX)
  melted_1 <- reshape2::melt(melted, id.vars = "Weeks")
  
  # Creating Line graph
  plot <- ggplot2::ggplot(melted_1,aes(x= as.numeric(Weeks), y= value, colour = variable, group = variable))+
    ggplot2::geom_hline(yintercept = anc_range[1], color = "grey", linetype = "dashed", size = 0.4)+
    ggplot2::geom_hline(yintercept = anc_range[2], color = "grey", linetype = "dashed", size = 0.4)+
    ggplot2::geom_hline(yintercept = MP_start_dose, color = "#ffd11a", linetype = "dashed", size = 0.4)+
    ggplot2::geom_hline(yintercept = (MTX_start_dose), colour = "#0099ff", linetype = "dashed", size = 0.4)+
    ggplot2::geom_line()+
    ggplot2::geom_point()+
    ggplot2::theme_classic()+
    ggplot2::labs(x= "Weeks",
                  y = "Parameters (log10)")+
    ggplot2::scale_x_continuous(limits = c(0,100), expand = c(0,0), breaks = scales::breaks_width(12))+
    ggplot2::theme(legend.background = ggplot2::element_rect(color = "dark grey",
                                                             fill = "white", linetype = "solid"))+
    ggplot2::theme(legend.position = "bottom",
                   axis.title = element_text(size = 15),
                   axis.text = element_text(size = 12))+
    ggplot2::theme(legend.key.size = unit(1, 'cm'), #change legend key size
                   # legend.title = element_text(size=12), #change legend title font size
                   legend.text = element_text(size=12))+
    ggplot2::theme(plot.margin = unit(c(0.6,0.75,0.4,0.3), "cm"))
    
    # Changing scale and legend label as per "million" or "billion" unit
    if(unit == "million"){
      plot <- plot+
        ggplot2::scale_y_continuous(trans = "log10",
                                    breaks = c(0, 25,50,100,250,500,750,1500,3000,4000,5000,mx+1000))+
        ggplot2::scale_color_manual(name = "Parameters: ", labels = c(bquote("ANC (x10"^~6~"/"~mu~"L)"),
                                                                      "6-MP dose", "MTX dose"), values = c("#00cc66","#ffd11a","#0099ff"))
      # print("plot_created_nonanim_million")
    }
    
    if(unit == "billion"){
      plot <- plot+
        ggplot2::scale_y_continuous(trans = "log10",
                                    breaks = c(0, 0.5, 1, 2, 5, 10, 25,50,100,250,500,750,mx+100))+
        ggplot2::scale_color_manual(name = "Parameters: ", labels = c(bquote("ANC (x10"^~9~"/L)"),
                                                                      "6-MP dose", "MTX dose"), values = c("#00cc66","#ffd11a","#0099ff"))
    }
  
  print("return plot")
  # Returning plot
  return(plot)
}

summarize_cycle_progression <- function(mt_csv_path, unit, anc_range){
  
  # Initializing cycle list
  cycle_list <- c("1", "2", "3", "4", "5", "6", "7", "8")
  
  # Initializing function to plot graph
  Graph <- function(MT_cyc, n, ANC_LowerBoundary, ANC_UpperBoundary, MR_mean){
    
    csSM_plot <- ggplot2::ggplot(MT_cyc ,aes(x=wm_AntiMtb, y=wm_ANC, color = Cycle, size = Cycle, group = 1))+
      ggplot2::labs(x= "Weighted mean Antimetabolite dose intensity (%)",
                    # y= expression(paste("Weighted mean ANC (", x10^{n}, "cells/L)")))+
                    y= bquote("Weighted mean ANC (x10"^~.(n)~ "cells/L)"))+
      ggplot2::scale_x_continuous(expand = c(0,0), limits = c(0,max(MT_cyc$wm_AntiMtb, na.rm = T)+ 20),
                                  breaks = scales::breaks_width(20))+
      ggplot2::scale_y_continuous(limits = c(0,(max(MT_cyc$wm_ANC, na.rm = TRUE)+500)),expand = c(0,0),
                                  breaks = scales::breaks_width(500))+
      ggplot2::annotate("rect", xmin = 80, xmax = 120, ymin = ANC_LowerBoundary, ymax = ANC_UpperBoundary, fill = "grey",
                        alpha = 0.25)+
      ggplot2::theme_classic()+
      ggplot2::scale_linetype(name = "optimal ANC")+
      ggplot2::geom_hline(yintercept = ANC_LowerBoundary, linetype = 2, size = 0.4, color = "black")+
      ggplot2::geom_hline(yintercept = ANC_UpperBoundary, linetype = 2, size = 0.4, color = "black")+
      ggplot2::geom_vline(xintercept = 80, linetype = 2, size = 0.4, color = "black")+
      ggplot2::geom_vline(xintercept = 120, linetype = 2, size = 0.4, color = "black")+
      ggplot2::geom_point()+
      ggplot2::geom_path(size = 0.5, arrow = arrow(length = unit(0.15, "inches")), show.legend = FALSE)+
      # ggplot2::scale_color_manual(labels = c("1", "2", "3", "4", "5", "6", "7", "8", "Overall Mean"),
      #                             values = c("#d9d9d9","#D3D3D3", "#A9A9A9", "#888888", "#707070", "#505050",
      #                                        "#303030", "#000000", "Red"))+
      ggplot2::scale_color_manual(labels = c("1", "5", "2", "6", "3", "7", "4", "8", "Overall Mean"),
                                  values = c("#d9d9d9","#707070","#D3D3D3","#505050", "#A9A9A9", "#303030","#888888", 
                                              "#000000", "Red"))+
      # ggplot2::scale_size_continuous ()+
      ggplot2::geom_point(data = MR_mean, aes(x = wm_AntiMtb, y = wm_ANC), size = 3)+
      ggplot2::theme(legend.position = 'bottom')+
      ggplot2::guides(size = 'none', colour = guide_legend(ncol = 5))+
      ggplot2::theme(axis.title.x = element_text(size = 15, color = "Black"),
                     axis.title.y = element_text(size = 15, color = "Black"),
                     axis.text.x = element_text(size = 12, color = "Black"),
                     axis.text.y = element_text(size = 12, color = "Black"))+
      ggplot2::theme(legend.key.size = unit(1, 'cm'), #change legend key size
                     legend.title = element_text(size=14), #change legend title font size
                     legend.text = element_text(size=14))+
      #                legend.spacing.x = unit(10, 'mm')) #change legend text font size
      ggplot2::theme(plot.margin = unit(c(0.6,0.75,0.4,0.3), "cm"))
    
    return(csSM_plot)
  }
  
  # Reading patient csv sheet
  if(file_ext(mt_csv_path) == "csv"){
    MR_0 <- utils::read.csv(mt_csv_path)
    # print("path_read")
  }
  
  if(file_ext(mt_csv_path) %in% c("xls", "xlsx")){
    MR_0 <- readxl::read_excel(mt_csv_path)
    # print("path_read")
  }
  
  
  # Calculating weights
  W <- NULL
  for (y in 1:(nrow(MR_0)-1))
  {
    Wt <- (MR_0$Weeks[[y+1]] - MR_0$Weeks[[y]])
    W <- rbind(W, Wt)
  }
  
  W <- rbind(W, 1)
  
  # Calculating and adding weighted mean data to data frame
  MR <- MR_0%>%
    dplyr::mutate(wt = W)%>%
    dplyr::mutate(wt_ANC = ANC*wt)%>%
    dplyr::mutate(wt_ANC = ANC*wt)%>%
    dplyr::mutate(N_6MP = MP_adj/100)%>%
    dplyr::mutate(N_MTX = MTX_adj/100)%>%
    dplyr::mutate(N_antiMtb = N_6MP*N_MTX)%>%
    dplyr::mutate(wt_antiMtb = N_antiMtb*wt)
  # filter(wt<=8)
  
  
  # Calculating summary measures per cycle for patient
  MT_cyc <- data.frame()[1:1, ]
  for (b in seq(cycle_list)) {
    
    MR_cyc <- dplyr::filter(MR, MR$Cycle == b)
    
    cyc <- data.frame(Cycle = cycle_list[b],
                      wm_ANC = (sum(MR_cyc$wt_ANC)/sum(MR_cyc$wt)),
                      wm_AntiMtb = ((sum(MR_cyc$wt_antiMtb)/sum(MR_cyc$wt))*100))
    
    MT_cyc <- rbind(MT_cyc, cyc)
  }
  
  # Calculating overall summary measures for patient
  MR_mean <- data.frame(Cycle = "Overall",
                        wm_ANC = (sum(MR$wt_ANC)/sum(MR$wt)),
                        wm_AntiMtb = ((sum(MR$wt_antiMtb)/sum(MR$wt))*100))
  
  MT_cyc$wm_ANC <- as.numeric(MT_cyc$wm_ANC)
  
  # Creating graph and adjusting scales/labels as per unit provided
  if(unit == "million"){
    
    csSM_plot <- Graph(MT_cyc = MT_cyc, n = 6, ANC_LowerBoundary =  anc_range[1],
                       ANC_UpperBoundary = anc_range[2], MR_mean = MR_mean)
    csSM_plot <- csSM_plot+
      ggplot2::scale_y_continuous(limits = c(0,(max(MT_cyc$wm_ANC, na.rm = TRUE)+500)),expand = c(0,0),
                                  breaks = scales::breaks_width(500))
    
    
  }
  if(unit == "billion"){
    
    csSM_plot <-  Graph(MT_cyc = MT_cyc, n = 9, ANC_LowerBoundary =  anc_range[1],
                        ANC_UpperBoundary = anc_range[2], MR_mean = MR_mean)
    
    csSM_plot <- csSM_plot+
      ggplot2::scale_y_continuous(limits = c(0,(max(MT_cyc$wm_ANC, na.rm = TRUE)+0.5)),expand = c(0,0),
                                  breaks = scales::breaks_width(0.5))
  }
  return(csSM_plot)
}

summarize_cohortMT <- function(pat_list,
                               unit, anc_range, dose_intensity_threshold){
  
  message("NOTE: Using dose_intensity_threshold (%) of ", dose_intensity_threshold)
  
  ANC<- Group<- N_6MP<- N_MTX<- N_antiMtb <- if_else <- wm_ANC <- wm_AntiMtb <- wt <- NULL
  
  # Initializing function to plot graph
  Graph <- function(MR_d, n, ANC_LowerBoundary, ANC_UpperBoundary, DI_threshold){
    SM_plot <- ggplot2::ggplot(MR_d,aes(x= wm_AntiMtb*100, y= wm_ANC, label = Pat_ID))+
      ggplot2::labs(x= "Weighted mean Antimetabolite dose intensity (%)\n",
                    y = "")+
      # x= "Weighted mean Antimetabolite dose intensity (%)\n",
      # y= bquote("Weighted mean ANC (x10"^~.(n)~ "cells/L)"))+
      ggplot2::scale_x_continuous(expand = c(0,0), limits = c(0,250))+
      # ggplot2::scale_y_continuous(limits = c(0,4000),expand = c(0,0), breaks = scales::breaks_width(500))+
      ggplot2::annotate("rect", xmin = DI_threshold, xmax = 250, ymin = ANC_LowerBoundary,
                        ymax = ANC_UpperBoundary, fill = "grey",alpha = 0.5)+
      ggplot2::theme_classic()+
      ggplot2::scale_linetype(name = "optimal ANC")+
      ggplot2::geom_hline(yintercept = ANC_LowerBoundary, linetype = 2, size = 0.4, color = "black")+
      ggplot2::geom_hline(yintercept = ANC_UpperBoundary, linetype = 2, size = 0.4, color = "black")+
      ggplot2::geom_vline(xintercept = DI_threshold, linetype = 2, size = 0.4, color = "black")+
      ggplot2::geom_point(shape = 16, size = 1, colour = "Black", aes(text = paste('ID:', Pat_ID,
                                                                                   '<br>ANC:', round(wm_ANC,2),
                                                                                   '<br>Dose Intensity (%):', 
                                                                                   round(wm_AntiMtb*100,2))))+
      ggplot2::theme(legend.background = element_rect(colour = 'dark grey',
                                                      fill = 'white', linetype='solid', size = 0.2))+
      ggplot2::theme(axis.title.x = element_text(size = 10),
                     # axis.title.x.bottom = element_text(margin = margin(0,0,0,0, "in")),
                     axis.text = element_text(size = 8))
    # plot.margin = margin(t = 0.6,r = 0.6,b = 0.8, l= 0.6, "cm"))
    print("Graph function ready")
    return(SM_plot)
  }
  
  MR_d <- NULL
  
  # For each patient:
  for (a in seq(pat_list)) {
    
    # Reading patient MT csv sheet
    # MR <- utils::read.csv(paste0(inputfolder_path, pat_list[a]))
    MR <- pat_list[[a]]
    
    # Calculating weights
    W <- NULL
    for (i in 1:(nrow(MR)-1)) {
      Wt <- (MR$Weeks[[i+1]] - MR$Weeks[[i]])
      W <- rbind(W, Wt)
    }
    print("2_")
    W <- rbind(W, 1)
    
    print("3_")
    # Calculating and adding weighted mean data to data frame
    MR_b <- MR%>%
      dplyr::mutate(wt = W)%>%
      dplyr::mutate(wt_ANC = ANC*wt)%>%
      dplyr::mutate(N_6MP = MR$MP_adj/100)%>%
      dplyr::mutate(N_MTX = MR$MTX_adj/100)%>%
      dplyr::mutate(N_antiMtb = N_6MP*N_MTX) %>%
      dplyr::mutate(wt_antiMtb = N_antiMtb*wt)%>%
      dplyr::mutate(wt_MP = N_6MP*wt) %>%
      dplyr::mutate(wt_MTX = N_MTX*wt)
    # filter(.,wt<=8)
    print("4_")
    print(names(pat_list[a]))
    # Calculating overall summary measures for patient
    MR_c <- data.frame(Pat_ID = gsub(".csv", "", gsub("_", "/", names(pat_list[a]))),
                       wm_ANC = (sum(MR_b$wt_ANC)/sum(MR_b$wt)),
                       wm_AntiMtb = (sum(MR_b$wt_antiMtb)/sum(MR_b$wt)),
                       wm_MP = (sum(MR_b$wt_MP)/sum(MR_b$wt)),
                       wm_MTX = (sum(MR_b$wt_MTX)/sum(MR_b$wt)))
    print("4sub_")
    # Combining data for all patients
    MR_d <- rbind(MR_d, MR_c)
    print("5_")
  }
  
  print("just before million")
  # Creating graph and adjusting scales/labels as per unit provided
  if(unit == "million") {
    print("Mill")
    SM_plot <- Graph(MR_d, n = 6,
                      ANC_LowerBoundary =  anc_range[1], ANC_UpperBoundary = anc_range[2],
                      DI_threshold = dose_intensity_threshold)
    SM_plot <- SM_plot+
      ggplot2::scale_y_continuous(limits = c(0,4000),expand = c(0,0), breaks = scales::breaks_width(500))
    print( "Graph_ready")
    
    SM_plotly <- ggplotly(SM_plot, tooltip = "text") %>% 
      layout(yaxis = list(title = list(text = "Weighted mean ANC (x10<sup>6</sup> cells/L)", 
                                       font = list(size = 13))))
             # autosize = TRUE)
             # xaxis = list(standoff = 30))
             # xaxis = list(title = list(text = "Weighted mean Antimetabolite dose intensity (%)",
             #                           font = list(size = 15))))

  }
  if(unit == "billion") {
    
    SM_plot <- Graph(MR_d, n = 9,
                     ANC_LowerBoundary =  anc_range[1], ANC_UpperBoundary = anc_range[2],
                     DI_threshold = dose_intensity_threshold)
    SM_plot <- SM_plot+
      ggplot2::scale_y_continuous(limits = c(0,4),expand = c(0,0), breaks = scales::breaks_width(0.5))
    
    SM_plotly <- ggplotly(SM_plot, tooltip = "text") %>% 
      # c("label", "x", "y")
      layout(yaxis = list(title = list(text = "Weighted mean ANC (x10<sup>9</sup> cells/uL)", 
                                       font = list(size = 13))))
             # autosize = TRUE)
    
  }
  
  print("Returning plot")
  # Returning plot
  return(SM_plotly)
  
}

compare_cohortsMT <- function(pat_list, intervention_date,
                              unit, anc_range, dose_intensity_threshold, n){
  
  # Initializing function to plot graph
  Graph <- function(wt_all1, n, ANC_LowerBoundary, ANC_UpperBoundary, DI_threshold){
    
    SMI_plot <- ggplot2::ggplot(wt_all1,aes(x= wm_AntiMtb*100, y= wm_ANC, label = Pat_ID))+
      ggplot2::labs(x= "Weighted mean Antimetabolite dose intensity (%)",
                    y = "")+
      # y = bquote("Weighted mean ANC (x10"^~.(n)~ "cells/L)"))+
      ggplot2::scale_x_continuous(expand = c(0,0), limits = c(0,250))+
      ggplot2::annotate("rect", xmin = DI_threshold, xmax = 250, ymin = ANC_LowerBoundary,
                        ymax = ANC_UpperBoundary, fill = "grey",alpha = 0.5)+
      ggplot2::theme_classic()+
      ggplot2::scale_linetype(name = "optimal ANC")+
      ggplot2::geom_hline(yintercept = ANC_LowerBoundary, linetype = 2, size = 0.4, color = "black")+
      ggplot2::geom_hline(yintercept = ANC_UpperBoundary, linetype = 2, size = 0.4, color = "black")+
      ggplot2::geom_vline(xintercept = DI_threshold, linetype = 2, size = 0.4, color = "black")+
      ggplot2::geom_point(shape = 16, size = 1, colour = "Black", aes(text = paste('ID:', Pat_ID,
                                                                                   '<br>ANC:', round(wm_ANC,2),
                                                                                   '<br>Dose Intensity (%):', 
                                                                                   round(wm_AntiMtb*100,2))))+
      ggplot2::theme(legend.background = element_rect(colour = 'dark grey',
                                                      fill = 'white', linetype='solid', size = 0.2))+
      ggplot2::facet_grid(~Group)+
      ggplot2::theme(strip.text.x = element_text(size = 11), 
                     axis.title.x = element_text(size = 11),
                     axis.text = element_text(size = 8),
                     # plot.margin = unit(c(0.6,0.75,0.4,0.3), "cm"),
                     panel.spacing = unit (1.5, "lines"))
  }
  
  
  MR_d <- NULL
  
  # Calculating summary data for each patient:
  for (a in seq(pat_list)) {
    
    # Reading patient MT csv sheet
    # MR <- utils::read.csv(paste0(inputfolder_path, pat_list[a]))
    MR <- pat_list[[a]]
    
    # Calculating weights
    W = NULL
    for (i in 1:(nrow(MR)-1)) {
      Wt <- (MR$Weeks[[i+1]] - MR$Weeks[[i]])
      W <- rbind(W, Wt)
    }
    
    W <- rbind(W, 1)
    
    # Calculating and adding weighted mean data to data frame
    MR_b <- MR%>%
      dplyr::mutate(wt = W)%>%
      dplyr::mutate(wt_ANC = ANC*wt)%>%
      dplyr::mutate(N_6MP = MR$MP_adj/100)%>%
      dplyr::mutate(N_MTX = MR$MTX_adj/100)%>%
      dplyr::mutate(N_antiMtb = N_6MP*N_MTX) %>%
      dplyr::mutate(wt_antiMtb = N_antiMtb*wt)%>%
      dplyr::mutate(wt_MP = N_6MP*wt) %>%
      dplyr::mutate(wt_MTX = N_MTX*wt)
    # filter(.,wt<=8)
    
    # Calculating overall summary measures for patient
    MR_c <- data.frame(Pat_ID = gsub(".csv", "", gsub("_", "/", names(pat_list[a]))),
                       StartDate = as.Date(MR_b$Dates[1], format = "%d/%m/%Y"),
                       wm_ANC = (sum(MR_b$wt_ANC)/sum(MR_b$wt)),
                       wm_AntiMtb = (sum(MR_b$wt_antiMtb)/sum(MR_b$wt)),
                       wm_MP = (sum(MR_b$wt_MP)/sum(MR_b$wt)),
                       wm_MTX = (sum(MR_b$wt_MTX)/sum(MR_b$wt)))
    
    # Combining data for all patients
    MR_d <- rbind(MR_d, MR_c)
  }
  
  
  # Analyzing and creating plot with Method 1 (intervention date provided)
  
  if(missing(intervention_date)){
    stop("intervention_Date missing")
  }
  
  if(!missing(intervention_date)){
    
    int_date <- as.Date(intervention_date)
    
    # Assigning groups based on intervention date
    wt_all1 <- MR_d %>%
      dplyr::mutate(Group = ifelse(StartDate < int_date, "Pre-Intervention", "Post-Intervention"))
    wt_all1$Group <- factor(wt_all1$Group, levels = c("Pre-Intervention", "Post-Intervention"))
    
    # Creating graph and adjusting scales/labels as per unit provided
    if(unit == "million") {
      
      SMI_plot <- Graph(wt_all1 = wt_all1, n = 6,  ANC_LowerBoundary = anc_range[1], ANC_UpperBoundary = anc_range[2],
                        DI_threshold = dose_intensity_threshold)
      SMI_plot <- SMI_plot+
        ggplot2::scale_y_continuous(limits = c(0,4000),expand = c(0,0), breaks = scales::breaks_width(500))
      
      SMI_plotly <- ggplotly(SMI_plot, tooltip = "text") %>% 
        # , height = 530
        # c("label", "x", "y")
        layout(yaxis = list(title = list(text = "Weighted mean ANC (x10<sup>6</sup> cells/L)", 
                                         font = list(size = 15))))
               # autosize = TRUE)

    }
    
    if(unit == "billion") {
      
      SMI_plot <- Graph(wt_all1 = wt_all1, n = 9,  ANC_LowerBoundary = anc_range[1], ANC_UpperBoundary = anc_range[2],
                        DI_threshold = dose_intensity_threshold)
      SMI_plot <- SMI_plot+
        ggplot2::scale_y_continuous(limits = c(0,5),expand = c(0,0), breaks = scales::breaks_width(0.5))
      
      SMI_plotly <- ggplotly(SMI_plot, tooltip = "text") %>% 
        # c("label", "x", "y")
        layout(yaxis = list(title = list(text = "Weighted mean ANC (x10<sup>9</sup> cells/uL)", 
                                         font = list(size = 15))))
               # autosize = TRUE)
      
    }
    
    # Returning plot
    return(SMI_plotly)
  }
}

assess_doses  <- function(pat_list, s_anc_threshold= NA, s_plt_threshold=NA,
                          red_anc_range=c(NA, NA), red_plt_range=c(NA, NA) , reduction_factor=NA,
                          inc_anc_threshold= NA, inc_plt_threshold=NA, escalation_factor=NA, tolerated_dose_duration=NA){

  # STOP DOSE:
  A1_d <- NULL
  A1_n <- NULL
  A2_d <- NULL
  A2_n <- NULL
  
  # REDUCE DOSE:    
  B1_n <- NULL
  B1_d <- NULL
  B1_n_c <- NULL
  B1_n_s <- NULL
  
  B2_n <- NULL
  B2_d <- NULL
  B2_n_s <- NULL
  B2_n_c <- NULL
  
  # INCREASE DOSE:
  C_n <- NULL
  C_d <- NULL
  C1_d <- NULL
  C1_n <- NULL
  C1_c <- NULL
  C1_s <- NULL
  C2_d <- NULL
  C2_n <- NULL
  
  
  for(i in seq(pat_list)){
    
    # Reading MT csv sheet for each patient
    # MR <- utils::read.csv(paste0(inputfolder_path, pat_list[[i]]))
    MR <- pat_list[[i]]
    # print(MR)
    
    ########## STOP DOSE ###########################
    if(is.na(s_anc_threshold) & is.na(s_plt_threshold)){
      A1_d <- NULL
      A1_n <- NULL
      A2_d <- NULL
      A2_n <- NULL
      
    } else {
      print("stop dose analyzing")
    ### 1: counts indicate stop, but doses not stopped:
    
    # Filtering for all rows that have counts below ANC or PLT thresholds (denominator)
    A1_a <- dplyr::filter(MR, ANC<=s_anc_threshold|PLT<=s_plt_threshold)
    A1_d <- rbind(A1_d, dplyr::count(A1_a))
    # FROM THE ABOVE DF, filtering for rows where dose not stopped (numerator)
    A1_b <- dplyr::filter(A1_a, MP!=0 & MTX!=0)
    A1_n <- rbind(A1_n, dplyr::count(A1_b))
    
    # print("A1")
    ### 2: counts are high, but doses stopped:
    
    # Filtering for all rows with stop dose decision (denominator)
    A2_a <- dplyr::filter(MR, MP==0 & MTX==0)
    A2_d <- rbind(A2_d, dplyr::count(A2_a))
    # FROM THE ABOVE DF, filtering for all rows where counts are above threshold - ie: counts not support decision (numerator)
    A2_b <- dplyr::filter(A2_a, ANC > s_anc_threshold & PLT > s_plt_threshold)
    A2_n <- rbind(A2_n, dplyr::count(A2_b))
    
    # print("A2")
    }
    ############## REDUCE DOSE #############################
    if(is.na(red_anc_range[1]) & is.na(red_anc_range[2]) & is.na(red_plt_range[1]) & is.na(red_plt_range[2])){
      B1_n <- NULL
      B1_d <- NULL
      B1_n_c <- NULL
      B1_n_s <- NULL
      
      B2_n <- NULL
      B2_d <- NULL
      B2_n_s <- NULL
      B2_n_c <- NULL
      print("All reduce null")
    } else {
      print("reduce dose analyzing")
    # Identifying starting dose (to be considered as max dose)
    max_dose <- MR$MP[1]
    
    # B1: counts indicate decrease (Denominator), but dose not decreased (Numerator)
    
    # Filtering rows with target ANC and PLT blood counts (no. of rows to be used as denominator)
    # This frame (B1_a) will be used as parent frame for below filters
    # B1_a <- dplyr::filter (MR, (ANC > anc_range[1] & ANC < anc_range[2]) |
    #                          (PLT > plt_range[1] & PLT < plt_range[2]))
    # B1_d <- rbind(B1_d, dplyr::count(B1_a))
    # Filtering rows where dose was NOT reduced by provided reduction factor (numerator)
    # B1_b <- dplyr::filter(B1_a, ((MP > ((reduction_factor/100)* max_dose)) | MP < 1))
    # B1_n <- rbind(B1_n, dplyr::count(B1_b))
    # Filtering rows where dose was INSTEAD continued/increased (numerator)
    # B1_n_a <- dplyr::filter(B1_a, (MP > ((reduction_factor/100)* max_dose)))
    # B1_n_c <- rbind(B1_n_c, dplyr::count(B1_n_a))
    # Filtering rows where dose was INSTEAD stopped (numerator)
    # B1_n_b <- dplyr::filter(B1_a, (MP < 1))
    # B1_n_s <- rbind(B1_n_s, dplyr::count(B1_n_b))
    
    # B2: dose decreased (Denominator), but the counts do not support it (Numerator)
    
    #Filtering rows where physician's decision IS to reduce dose (denominator)
    # This frame (B2_a) will be used as parent frame for below filters
    B2_a <- dplyr::filter(MR, (MP <= (dplyr::lag(MP, n=1) - ((reduction_factor/100)* dplyr::lag(MP, n = 1))) & MP > 0))
    B2_d <- rbind(B2_d, dplyr::count(B2_a))
    # Filtering rows where blood counts do NOT support a reduce dose decision (numerator)
    B2_b <- dplyr::filter(B2_a, (ANC <= red_anc_range[1] | ANC > red_anc_range[2]))%>%
      dplyr::filter(PLT <= red_plt_range[1] | PLT > red_plt_range[2])
    B2_n <- rbind(B2_n, dplyr::count(B2_b))
    # print("B1")
    # Filtering rows where bloods INSTEAD support stop dose
    B2_n_a <- dplyr::filter(B2_a, (ANC <= red_anc_range[1] | PLT <= red_plt_range[1]))
    B2_n_s <- rbind(B2_n_s, dplyr::count(B2_n_a))
    # Filtering rows where bloods INSTEAD support continue/increase dose
    B2_n_b <- dplyr::filter(B2_a, ANC > red_anc_range[2] & PLT > red_plt_range[2])
    B2_n_c <- rbind(B2_n_c, dplyr::count(B2_n_b))
    # print("B2")
    }
    ################# INCREASE DOSE ##############
    if(is.na(inc_anc_threshold) & is.na(inc_plt_threshold)){
      C_b <- NULL
      C1_d <- NULL
      C1_n <- NULL
      C1_c <- NULL
      C1_s <- NULL
      C2_d <- NULL
      C2_n <- NULL
      print("increase dose null")
    } else{

    dose1 <- as.numeric(MR$MP[1])
    
    MR1 <- MR %>%
      dplyr::mutate(Esc = NA)
    
    for(x in seq_len(nrow(MR1))){
      
      
      if(MR1$Weeks[x]>=(tolerated_dose_duration+1)){
        # print(paste0(i, "_", x))
        dif <- min(MR1$Weeks[x] - tolerated_dose_duration)
        # print(paste0(i, "_b"))
        if(any(MR$Weeks <= dif)){
        if(all(MR1$ANC[which(abs(MR1$Weeks - dif) == min(abs(MR1$Weeks - dif)))[1]
                       :x] >= inc_anc_threshold) &
           all(MR1$PLT[which(abs(MR1$Weeks - dif) == min(abs(MR1$Weeks - dif)))[1]
                       :x] >= inc_plt_threshold) &
           all(MR1$MP[which(abs(MR1$Weeks - dif) == min(abs(MR1$Weeks - dif)))[1]
                      :(x-1)] == MR$MP[x-1]) &
           all(MR1$MTX[which(abs(MR1$Weeks - dif) == min(abs(MR1$Weeks - dif)))[1]
                      :(x-1)] == MR$MTX[x-1])){
          
          # print(paste0(i,"_c"))
          MR1$Esc[x] <- "Y"
         }
        }
      }
    }
    # print("C1_a")
    # print(MR1)
    
    MR2 <- MR1 %>%
      dplyr::mutate(Esc = ifelse(is.na(Esc), "N", Esc))
    
    # print("ESC")
    print(MR2)
    
    # C: Counts indicate increase (D), and dose increased (N):
    
    C_a <- dplyr::filter(MR2, (Esc == "Y"))
    C_d <- rbind(C_d, dplyr::count(C_a))

    C_b<- MR2 %>%
          filter((Esc == "Y") &
                  MP >= dose1 &
                  MP >= (lag(MP, n = 1) + (escalation_factor/100)*lag(MP, n = 1)))

    print(C_b)

    C_n <- rbind(C_n, dplyr::count(C_b))
    
    
    # C1: Protocol indicates increase (D) ("Y" from previous loop), dose not increased (N):
    
    # C1_a <- dplyr::filter(MR2, (Esc == "Y"))
    # C1_d <- rbind(C1_d, dplyr::count(C1_a))
    # print(C1_a)
    # 
    # C1_b <- dplyr::filter(C1_a,  MP <= (dplyr::lag(MP, n = 1) + (escalation_factor/100)*dplyr::lag(MP, n = 1)) | MP <= dose1)
    # C1_n <- rbind(C1_n, dplyr::count(C1_b))
    # print(C1_n)

    # # dose continued instead:
    # C_c <- dplyr::filter(C1_a, MP == (dplyr::lag(MP, n = 1)) & MP > dose1)
    # C1_c <- rbind(C1_c, C_c)
    # 
    # # dose stopped instead:
    # C_s <- dplyr::filter(C1_a, MP == 0)
    # C1_s <- rbind(C1_s, C_s)
    
    # print("C2a")
    # C2: Dose increased (D), but counts do not support it(N):
    
    # C2_a <- dplyr::filter(MR2,
    #                       MP >= (dplyr::lag(MP, n = 1) + ((escalation_factor/100)*dplyr::lag(MP, n = 1))) &
    #                         MP > dose1)
    # 
    # C2_d <- rbind(C2_d, dplyr::count(C2_a))
    # # print("C2a")
    # 
    # C2_b <- dplyr::filter(C2_a, ANC < inc_anc_threshold | PLT < inc_plt_threshold & Esc == "N")
    # C2_n <- rbind(C2_n, dplyr::count(C2_b))
    # 
    
    # print("C2b")
    }
    #################### RETURN ###################
    
  }
  
  if(!is.null(A2_n) & !is.null(A2_d)){
    print("!null_df_stop)")
  df_stop <-    data.frame(Parameter = c("No. of patients analyzed",
                                         "No. of 'dose stop' decisions",
                                         "No. of times the counts did \n not support dose decision",
                                         "Stop Discordance"),
                           Results = c(i, sum(A2_d), sum(A2_n), paste0(round(((sum(A2_n)/sum(A2_d))*100),2), "%")))
  } else{
    print("null_df_stop)")
    df_stop <-    data.frame(Parameter = c("No. of patients analyzed",
                                           "No. of 'dose stop' decisions",
                                           "No. of times the counts did \n not support dose decision",
                                           "Stop Discordance"),
                             Results = c(NA,NA,NA,NA))
  }
  if(!is.null(B2_n) & !is.null(B2_d)){
    print("!null_df_red)")
  df_reduce <- data.frame(Parameter = c("No. of patients analyzed",
                                        "No. of 'dose reduce' decisions",
                                        "No. of times the counts did \n not support dose decision",
                                        "Reduce Discordance (%)"),
                          Results = c(i, sum(B2_d), sum(B2_n), paste0(round(((sum(B2_n)/sum(B2_d))*100),2), "%")))
  } else{
    print("null_df_red)")
  df_reduce <-  data.frame(Parameter = c("No. of patients analyzed",
                                         "No. of 'dose reduce' decisions",
                                         "No. of times the counts did \n not support dose decision",
                                         "Reduce Discordance (%)"),
                             Results = c(NA,NA,NA,NA))
  }
  
  if(!is.null(C_n) & !is.null(C_d)){
    print("!null_df_inc)")
  df_increase <- data.frame(Parameter = c("No. of patients analyzed",
                                          "No. of times protocol recommended an increase",
                                          "No. of 'dose increase' decisions",
                                          "Increase Discordance (%)"),
                            Results = c(i, sum(C_d), sum(C_n), paste0(round(((1-(sum(C_n)/sum(C_d)))*100),2), "%")))
  } else{
    print("null_df_inc)")
  df_increase <- data.frame(Parameter = c("No. of patients analyzed",
                                          "No. of times protocol recommended an increase",
                                          "No. of 'dose increase' decisions",
                                          "Increase Discordance (%)"),
                            Results = c(NA, NA, NA, NA))
  }
  
  df <- list(s = df_stop, r = df_reduce, inc = df_increase)
  print(names(df))
  
  # print(df)
  # Returning final dataframe
  #e6e6e6
  return(df)
}
Hemat_tox <- function(pat_list, anc_range= c(NA, NA), duration_anc = NA,
                      plt_range= c(NA, NA), duration_plt = NA, 
                      hb_range= c(NA, NA), duration_hb = NA){
  
  all_tox <- NULL
  all_neut1 <- NULL
  all_thromb1 <- NULL
  all_anaem1 <- NULL
  for(z in 1:length(pat_list)){
    # for(z in 1:1){
    
    pat_csv <- pat_list[[z]]
    
    print("pat_csv ready")
    
    if(nrow(pat_csv) >= 1){
      # for (i in 1:nrow(pat_csv)) {
      MT_N <- NULL
      
      MR <- pat_csv
      
      W = NULL
      for (j in 1:(nrow(MR)-1))
      {
        # print(nrow(MR)-1)
        Wt <- as.numeric(MR$Weeks[[j+1]] - MR$Weeks[[j]])
        # print(Wt)
        W <- rbind(W, Wt)
      }
      
      W <- rbind(W, 1)
      
      MR <- MR%>%
        dplyr::mutate(wt = W)
      
      print(MR)
      
      print("starting neut")
      # Neutropenia ######
      # print(anc_range[1])
      # print(anc_range[2])
      
      if(!is.na(anc_range[1]) & !is.na(anc_range[2])){
        print("analyzing neutropenia")
        
        l=1
        # dur=NULL
        neut_dur <- data.frame(matrix(ncol = 1, nrow = 0))
        colnames(neut_dur) <- c("Neut_duration")
        flag=0
        while (l<=nrow(MR))
        {
          j=l
          
          flag=0
          if(MR$ANC[l]<=anc_range[1])
          {
            j=l
            while (j<=nrow(MR))
            {
              
              if(MR$ANC[j]>= anc_range[2] | j==nrow(MR))
              {
                d_neut <- as.numeric(MR$Weeks[j] - MR$Weeks[l])
                neut_dur = rbind(neut_dur, d_neut)
                colnames(neut_dur) <- c("Neut_duration")
                print(neut_dur)
                flag=1
                jj=j
                break
              }
              j=j+1
            }
          }
          
          if(j == nrow(MR))
          {
            break
          }
          
          if(flag=="1")
          {
            # print(j)
            l=j
          }
          
          if(flag=="0")
          {l = l+1}
          
          # print(MR$ANC[l])
          
        }
      } else {neut_dur <- NULL}
      
      
      print(paste0("Neut_dur null? - ", neut_dur))
      
      ########
      
      print("starting thromb")
      # Thrombocytopenia #######
      if(!is.na(plt_range[1]) & !is.na(plt_range[2])){
        print("analyzing thromb")
        m = 1
        thromb_dur <- data.frame(matrix(ncol = 1, nrow = 0))
        colnames(thromb_dur) <- c("thromb_duration")
        flag= 
          while (m<=nrow(MR))
          {
            n=m
            
            flag= 3
            if(MR$PLT[m]<=plt_range[1])
            {
              n=m
              while (n<=nrow(MR))
              {
                
                if(MR$PLT[n]>=plt_range[2] | n==nrow(MR))
                {
                  thromb_d <- as.numeric(MR$Weeks[n] - MR$Weeks[m])
                  thromb_dur = as.data.frame(rbind(thromb_dur, thromb_d))
                  colnames(thromb_dur) <- c("thromb_duration")
                  flag=4
                  nn=n
                  break
                }
                n=n+1
              }
            }
            
            if(n == nrow(MR))
            {
              break
            }
            
            if(flag=="4")
            {
              # print(j)
              m=n
            }
            
            if(flag=="3")
            {m = m+1}
            
          }
      } else {thromb_dur <- NULL}
      
      print(paste0("thromb_dur null? ", thromb_dur))
      #########
      
      print("starting Anaemia")
      if("Hb" %in% colnames(MR)){
        # Anaemia #########
        if(!is.na(hb_range[1]) & !is.na(hb_range[2])){
          print("1")
          p=1
          # dur=NULL
          anaem_dur <- data.frame(matrix(ncol = 1, nrow = 0))
          colnames(anaem_dur) <- c("anaem_duration")
          flag= 6
          while (p<=nrow(MR))
          {
            q=p
            
            flag=6
            if(MR$Hb[p]<=hb_range[1])
            {
              print("2")
              q=p
              while (q<=nrow(MR))
              {
                
                if(MR$Hb[q]>= hb_range[2] | q==nrow(MR))
                {
                  print("3")
                  anaem_d <- as.numeric(MR$Weeks[q] - MR$Weeks[p])
                  anaem_dur = as.data.frame(rbind(anaem_dur, anaem_d))
                  colnames(anaem_dur) <- c("anaem_duration")
                  flag=7
                  qq=q
                  break
                }
                q=q+1
              }
            }
            
            if(q == nrow(MR))
            {
              print("4")
              break
            }
            
            if(flag=="7")
            {
              print("5")
              # print(j)
              p=q
            }
            
            if(flag=="6")
            {p = p+1}
          }
          
        } else {anaem_dur <- NULL}
        
        print(paste0("anaem_dur null?", anaem_dur))
        
      } else{anaem_dur <- NULL}
      ###########
      
    } # end of if (nrow >=1) 
    
    print("calculations")
    
    # Neutropenia analysis:
    if(!is.null(neut_dur)){ 
      colnames(neut_dur) <- c("Neut_duration")
      
      Neut_total_episodes <- data.frame(neut_ep = nrow(neut_dur))
      Neut_total_duration <- data.frame(neut_dur = sum(neut_dur$Neut_duration))
      
      if(!is.na(duration_anc)){
        # print("starting long dur anc")
        neut_Long_dur <- neut_dur%>%
          dplyr::filter(neut_dur$Neut_duration >= duration_anc)
        
        Long_dur_neutropenia_ep <- data.frame(neut_Long_ep = nrow(neut_Long_dur))
        Long_dur_neutropenia_duration <- data.frame(neut_Long_duration = sum(neut_Long_dur$Neut_duration))
      } else {
        Long_dur_neutropenia_ep <- data.frame(neut_Long_ep = NA)
        Long_dur_neutropenia_duration <- data.frame(neut_Long_duration = NA)
      }
      # print("Neut: putting it together")
      all_neut <- as.data.frame(cbind(Neut_total_episodes = Neut_total_episodes$neut_ep,
                                      Neut_total_duration = Neut_total_duration$neut_dur,
                                      Long_dur_neutropenia_ep = Long_dur_neutropenia_ep$neut_Long_ep,
                                      Long_dur_neutropenia_duration = Long_dur_neutropenia_duration$neut_Long_duration))
      print("neut_each patient:")
      print(all_neut)
    } else{ all_neut <- NULL}
    
    # Thrombocytopenia analysis:
    if(!is.null(thromb_dur)){
      colnames(thromb_dur) <- c("thromb_duration")
      
      Thromb_total_episodes <- data.frame(thromb_ep = nrow(thromb_dur))
      Thromb_total_duration <- data.frame(thromb_dur = sum(thromb_dur$thromb_duration))
      if(!is.na(duration_plt)){
        # print("starting long dur plt")
        thromb_Long_dur <- thromb_dur%>%
          dplyr::filter(thromb_dur$thromb_duration >= duration_plt)
        
        Long_dur_Thrombocytopenia_ep <- data.frame(thromb_Long_ep = nrow(thromb_Long_dur))
        Long_dur_Thrombocytopenia_duration <- data.frame(throm_Long_duration = sum(thromb_Long_dur$thromb_duration))
      } else {
        Long_dur_Thrombocytopenia_ep <- data.frame(thromb_Long_ep = NA)
        Long_dur_Thrombocytopenia_duration <- data.frame(throm_Long_duration = NA)
      }
      # print("Thromb: putting it together")
      all_thromb <- as.data.frame(cbind(Thromb_total_episodes = Thromb_total_episodes$thromb_ep,
                                        Thromb_total_duration = Thromb_total_duration$thromb_dur,
                                        Long_dur_Thromb_ep = Long_dur_Thrombocytopenia_ep$thromb_Long_ep,
                                        Long_dur_Thromb_duration = Long_dur_Thrombocytopenia_duration$throm_Long_duration))
      print("thromb_each patient:")
      print(all_thromb)
    } else{ all_thromb <- NULL}
    
    # Anaemia analysis:
    # if("Hb" %in% colnames(MR)){
      if(!is.null(anaem_dur)){
        print("!null")
        colnames(anaem_dur) <- c("anaem_duration")
        
        Anaem_total_episodes <- data.frame(anaem_ep = nrow(anaem_dur))
        Anaem_total_duration <- data.frame(anaem_dur = sum(anaem_dur$anaem_duration))
        
        if(!is.na(duration_hb)){
          # print("starting long duration hb")
          anaem_Long_dur <- anaem_dur%>%
            dplyr::filter(anaem_dur$anaem_duration >= duration_hb)
          
          Long_dur_Anaemia_ep <- data.frame(anaem_Long_ep = nrow(anaem_Long_dur))
          Long_dur_Anaemia_duration <- data.frame(anaem_Long_duration = sum(anaem_Long_dur$anaem_duration))
        } else{
          Long_dur_Anaemia_ep <- data.frame(anaem_Long_ep = NA)
          Long_dur_Anaemia_duration <- data.frame(anaem_Long_duration = NA)
        }
        # print("Anaem: putting it together")
        all_anaem <- as.data.frame(cbind(Anaem_total_episodes = Anaem_total_episodes$anaem_ep,
                                         Anaem_total_duration = Anaem_total_duration$anaem_dur,
                                         Long_dur_Anaem_ep = Long_dur_Anaemia_ep$anaem_Long_ep,
                                         Long_dur_Anaem_duration = Long_dur_Anaemia_duration$anaem_Long_duration))
        print("anaem_each patient:")
        print(all_anaem)
      } else{ all_anaem <- NULL}
    # } else{ all_anaem <- NULL}
    
    all_neut1 <- rbind(all_neut1, all_neut)
    all_thromb1 <- rbind(all_thromb1, all_thromb)
    all_anaem1 <- rbind(all_anaem1, all_anaem)
  } # end of for loop
  
  print("Starting to put it together")
  if(length(pat_list) <=1){
    
    if(!is.null(all_neut1)){
      all_neut2 <- as.data.frame(t(all_neut1))
      all_neut3 <- data.frame(cbind("Parameter" = c("Number of episodes", "Duration (weeks)", 
                                                    "Number of long duration toxicity episodes",
                                                    "Duration of long duration toxicity (weeks)"),
                                    "Value" = all_neut2$V1))
    } else{
      all_neut3 <- data.frame(cbind("Parameter" = c("Number of episodes", "Duration (weeks)", 
                                                    "Number of long duration toxicity episodes",
                                                    "Duration of long duration toxicity (weeks)"),
                                    "Value" = "0"))
    }
    
    if(!is.null(all_thromb1)){
      all_thromb2 <- as.data.frame(t(all_thromb1))
      all_thromb3 <- data.frame(cbind("Parameter" = c("Number of episodes", "Duration (weeks)", 
                                                      "Number of long duration toxicity episodes",
                                                      "Duration of long duration toxicity (weeks)"),
                                      "Value" = all_thromb2$V1))
    } else{
      all_thromb3 <- data.frame(cbind("Parameter" = c("Number of episodes", "Duration (weeks)", 
                                                      "Number of long duration toxicity episodes",
                                                      "Duration of long duration toxicity (weeks)"),
                                      "Value" = "0"))
    }
    
    if("Hb" %in% colnames(MR)){
      if(!is.null(all_anaem1)){
        all_anaem2 <- as.data.frame(t(all_anaem1))
        all_anaem3 <- data.frame(cbind("Parameter" = c("Number of episodes", "Duration (weeks)", 
                                                       "Number of long duration toxicity episodes",
                                                       "Duration of long duration toxicity (weeks)"),
                                       "Value" = all_anaem2$V1))
      } else{
        all_anaem3 <- data.frame(cbind("Parameter" = c("Number of episodes", "Duration (weeks)", 
                                                       "Number of long duration toxicity episodes",
                                                       "Duration of long duration toxicity (weeks)"),
                                       "Value" = "0"))
      }
    } else {
      all_anaem3 <- data.frame(cbind("Parameter" = c("Number of episodes", "Duration (weeks)", 
                                                     "Number of long duration toxicity episodes",
                                                     "Duration of long duration toxicity (weeks)"),
                                     "Value" = "Not applicable"))
    }
    
  }
  
  
  if(length(pat_list) > 1){
    if(!is.null(all_neut1)){
      neutVars <- c(names(all_neut1))
      all_neut2 <- tableone::CreateTableOne(vars = neutVars, data = all_neut1)
      all_neut3 <- as.data.frame(print(all_neut2, nonnormal = neutVars, contDigits = 0))
      all_neut3 <- all_neut3 %>% 
        rownames_to_column(var = "Parameter") %>%
        mutate(Overall = gsub(",", " -", Overall)) %>% 
        rename("Value\n (Median[IQR])" = Overall)
      all_neut3[1:5,1] <- c("Number of patients analyzed", 
                            "Number of episodes", "Duration (weeks)", 
                            "Number of long duration toxicity episodes",
                            "Duration of long duration toxicity (weeks)")
    } else{
      all_neut3 <- data.frame(cbind("Parameter" = c("Number of episodes", "Duration (weeks)", 
                                                    "Number of long duration toxicity episodes",
                                                    "Duration of long duration toxicity (weeks)"),
                                    "Value" = "0"))
    }
    
    if(!is.null(all_thromb1)){
      thrombVars <- c(names(all_thromb1))
      all_thromb2 <- tableone::CreateTableOne(vars = thrombVars, data = all_thromb1)
      all_thromb3 <- as.data.frame(print(all_thromb2, nonnormal = thrombVars, contDigits = 0))
      all_thromb3 <- all_thromb3 %>% 
        rownames_to_column(var = "Parameter") %>%
        mutate(Overall = gsub(",", " -", Overall)) %>% 
        rename("Value\n (Median[IQR])" = Overall)
      all_thromb3[1:5,1] <- c("Number of patients analyzed", 
                              "Number of episodes", "Duration (weeks)", 
                              "Number of long duration toxicity episodes",
                              "Duration of long duration toxicity (weeks)")
    } else{
      all_thromb3 <- data.frame(cbind("Parameter" = c("Number of episodes", "Duration (weeks)", 
                                                      "Number of long duration toxicity episodes",
                                                      "Duration of long duration toxicity (weeks)"),
                                      "Value" = "0"))
    }
    
    if("Hb" %in% colnames(MR)){
      if(!is.null(all_anaem1)){
        anaemVars <- c(names(all_anaem1))
        all_anaem2 <- tableone::CreateTableOne(vars = anaemVars, data = all_anaem1)
        all_anaem3 <- as.data.frame(print(all_anaem2, nonnormal = anaemVars, contDigits = 0))
        all_anaem3 <- all_anaem3 %>% 
          rownames_to_column(var = "Parameter") %>%
          mutate(Overall = gsub(",", " -", Overall)) %>% 
          rename("Value\n (Median[IQR])" = Overall)
        all_anaem3[1:5,1] <- c("Number of patients analyzed", 
                               "Number of episodes", "Duration (weeks)", 
                               "Number of long duration toxicity episodes",
                               "Duration of long duration toxicity (weeks)")
      } else {
        all_anaem3 <- data.frame(cbind("Parameter" = c("Number of episodes", "Duration (weeks)", 
                                                       "Number of long duration toxicity episodes",
                                                       "Duration of long duration toxicity (weeks)"),
                                       "Value" = "0")) 
      }
    } else{
      all_anaem3 <- data.frame(cbind("Parameter" = c("Number of episodes", "Duration (weeks)", 
                                                     "Number of long duration toxicity episodes",
                                                     "Duration of long duration toxicity (weeks)"),
                                     "Value" = "Not applicable"))
    }
    
  }
  
  haem_tox_results <- list(neut = all_neut3, thromb = all_thromb3, anaem = all_anaem3)
  
  return(haem_tox_results)
}

time_to_doseinc <- function(pat_list, escalation_factor) {
  
  # if(missing(escalation_factor)){
  #   message("NOTE: Using default escalation factor (%) = 25")
  #   escalation_factor <- 25
  # }
  print("Start")
  DoseInc <- NULL
  # For each patient
  for(i in seq(pat_list)){
    print(i)
    MR_0 <- pat_list[[i]]
    # Creating dose increase "status" column. Setting "status" default to 1.
    MR <- MR_0 %>%
      dplyr::mutate(status = "1")
    
    # Identifying row with first dose increase for each patient
    MR_doseI <- data.frame(matrix(ncol = ncol(MR), nrow = 1))
    colnames(MR_doseI) <- names(MR)
    MR_doseI <- MR %>%
      dplyr::filter(MP > (MP[1] + (escalation_factor/100)*MP[1])) %>%
      dplyr::slice_head()
    print("status")
    # Converting status to "0" if dose was never increased beyond starting dose.
    if(nrow(MR_doseI) <1) {
      
      MR_doseI <- MR[nrow(MR),]
      MR_doseI <- MR_doseI %>%
        dplyr::mutate(status = "0")
      
    }
    # Combining data for all patients
    DoseInc <- rbind(DoseInc, MR_doseI)
  }
  
  # Confirming "Status" to be numeric
  DoseInc$status <- as.numeric(DoseInc$status)
  print("Plotting")
  # Calculating and plotting time to dose increase
  
    fit_dose <- survival::survfit(survival::Surv(Weeks, status) ~ 1, data = DoseInc)
    
    print(fit_dose)
    
    DoseInc_plot <- survminer::ggsurvplot(fit_dose,
                                  data = DoseInc,
                                  conf.int = TRUE,
                                  cumevents = TRUE,
                                  cumevents.y.text = FALSE,
                                  cumevents.title = "Cumulative no.of patients experiencing first dose increase",
                                  fontsize = 4,
                                  tables.y.text = TRUE,
                                  tables.theme = survminer::theme_cleantable(),
                                  ggtheme = ggplot2::theme_classic(),
                                  palette = c("#00e6ac", "#2E9FDF"),
                                  fun = "event",
                                  surv.median.line = "hv",
                                  xlab = "Time (weeks)",
                                  ylab = "Probability of first attempted 6-MP dose increase",
                                  break.time.by = 12,
                                  break.y.by = 0.2,
                                  xlim = c(0,100), ylim = c(0, 1.0),
                                  axes.offset = FALSE,
                                  legend.title = "",
                                  surv.plot.height = 7, tables.height = 0.15)
  
  doseinc_return <- list(Plot = DoseInc_plot, fitdose = fit_dose)
  
  # Returning graph
  # return(DoseInc_plot)
  return(doseinc_return)
  
}

extendedCheckboxGroup <- function(..., extensions = list()) {
  cbg <- checkboxGroupInput(...)
  nExtensions <- length(extensions)
  nChoices <- length(cbg$children[[2]]$children[[1]])
  
  if (nExtensions > 0 && nChoices > 0) {
    lapply(1:min(nExtensions, nChoices), function(i) {
      # For each Extension, add the element as a child (to one of the checkboxes)
      cbg$children[[2]]$children[[1]][[i]]$children[[2]] <<- extensions[[i]]
    })
  }
  cbg
}

bsButtonRight <- function(...) {
  btn <- bsButton(...)
  # Directly inject the style into the shiny element.
  btn$attribs$style <- "float: right;"
  btn
}

# Code: 

server <- function(input, output, session) {
  
# initializing inputs into reactive values  
  tabs_rv = reactiveValues(n = 1, current_MT_level = "0",
                           help_start = 0, help_num = 0,
                           patdata_flag = 0, patinput = 0, pat_num = 0,
                           plots_flag = 0, plots_num = 0, help_go = 0, help_go1 = 0, 
                           assdose_flag = 0, assdose_num = 0,help_ind_assdose = 0, help_c_assdose = 0,
                           tox_flag = 0, tox_num = 0, help_ind_tox = 0, help_c_tox = 0,
                           helpinput = 0, help_flag = 0)
  
  input_vals = reactiveValues(
    input_status = "reset",
    c_input_status = "reset",
    input_df = NULL,
    cohort_IDs = NULL,
    function_list = NULL,
    Func_choice = NULL,
    #action buttons:
    # go2 = NULL,
    # go3 = NULL,
    # help_go = 0,
    # help_ind_assdose = 0,
    # help_ind_tox = 0, 
    # help_go1 = 0,
    # help_c_assdose = 0,
    # help_c_tox = 0,
    #assess dose decision inputs:
    stop_ANC_lower = character(0),
    stop_PLT_lower = character(0),
    red_ANC_lower = character(0),
    red_ANC_upper = character(0),
    red_PLT_lower = character(0),
    red_PLT_upper = character(0),
    red_factor = 0,
    inc_ANC_upper = character(0),
    inc_PLT_upper = character(0),
    inc_factor = 0,
    inc_tld = 0,
    #assess hemat tox inputs:
    neut_anc = character(0),
    anc_recov = character(0),
    dur_anc = 0,
    thromb_plt = character(0),
    plt_recov = character(0),
    dur_plt = 0,
    anaem_Hb = character(0),
    Hb_recov = character(0),
    dur_hb = 0)

  
  #Tracking level of analysis (individual/cohort)
  observeEvent(eventExpr = input$MT_level, {
    
    tabs_rv$previous_MT_level = tabs_rv$current_MT_level
    print(paste0("Previous level = ", tabs_rv$previous_MT_level))
    
    tabs_rv$current_MT_level = input$MT_level
    print(paste0("Current level = ", tabs_rv$current_MT_level))
    
    if(tabs_rv$current_MT_level == "Individual" & tabs_rv$previous_MT_level == "Cohort"){
      print("Individual from Cohort")
      
      shinyjs::reset("cohort_IDs")
      input_vals$cohort_IDs <- NULL
      print(input_vals$cohort_IDs)
      input_vals$c_input_status == "reset"
      
      shinyjs::reset("cMT_Func")
      input_vals$Func_choice <- NULL
      
      output$StopDose <- NULL
      output$ReduceDose <- NULL
      output$IncreaseDose <- NULL
      
      output$NeutTox <- NULL
      output$ThrombTox <- NULL
      output$AnaemTox <- NULL
    }
    
    if(tabs_rv$current_MT_level == "Cohort" & tabs_rv$previous_MT_level == "Individual"){
      print("Cohort from Individual")
      
      shinyjs::reset("ID") # only clears what is shown in the ui. Does not remove/clear the input stored. 
      input_vals$input_df <- NULL # removes stored input that is used in all functions
      print(input_vals$input_df)
      input_vals$input_status = "reset"
      
      shinyjs::reset("MT_Func")
      input_vals$function_list <- NULL
      
      output$StopDose <- NULL
      output$ReduceDose <- NULL
      output$IncreaseDose <- NULL
      
      output$NeutTox <- NULL
      output$ThrombTox <- NULL
      output$AnaemTox <- NULL
    }
    
  })
  
  #Tracking previous and current tabs and flagging patient and help tabs
  observeEvent(eventExpr = input$PanelID, {
    
    tabs_rv$previous_tab = tabs_rv$current_tab
    print(paste0("Previous tab = ", tabs_rv$previous_tab))
    tabs_rv$current_tab = input$PanelID
    print(paste0("Current tab = ", tabs_rv$current_tab))

    # Tracking patient data tab
    if(tabs_rv$current_tab == "Patient Data"){
      tabs_rv$patdata_flag = 1
    }
    
    # Tracking help tab
    if(tabs_rv$current_tab == "Help"){
      tabs_rv$help_flag = 1
    }
    
    # Tracking Plots tab
    if(tabs_rv$current_tab == "Plots"){
      tabs_rv$plots_flag = 1
    } 
    
    # Tracking assess dose tab
    if(tabs_rv$current_tab == "Dose decisions"){
      tabs_rv$assdose_flag = 1
    }
    
    # Tracking hemat toxicity tab
    if(tabs_rv$current_tab == "Toxicity"){
      tabs_rv$tox_flag = 1
    }
    
  })
  
  # Tracking file(s) upload
  observeEvent(input$ID,{
    print("input!")
    input_vals$input_status = "uploaded"
    print(input_vals$input_status)
  })
  
  input_df <- reactive({
    if (input_vals$input_status == 'uploaded') {
      return(input$ID)
    } else if (input_vals$input_status =='reset') {
      return(NULL)
    }
  })
  
  observeEvent(input$cohort_IDs, {
    print("cohort input!")
    input_vals$c_input_status = "uploaded"
    print(input_vals$c_input_status)
    if(all(any(file_ext(input$cohort_IDs$datapath) == "csv") & any(file_ext(input$cohort_IDs$datapath) %in% c("xls", "xlsx")))){
      alert("Ensure all files are either in csv format or all files are in excel format")
    }
  })
  
  cohort_IDs <- reactive({
    if (input_vals$c_input_status == 'uploaded') {
      return(input$cohort_IDs)
    } else if (input_vals$c_input_status =='reset') {
      return(NULL)
    }
    
  })
  
  # Tracking tabs
  tabs_n <- reactive({
    # message(tabs_rv$plots_flag)
    # message(tabs_rv$assdose_flag)
    # message(tabs_rv$tox_flag)
    
    return(sum(tabs_rv$plots_flag, tabs_rv$assdose_flag, tabs_rv$tox_flag, tabs_rv$patdata_flag, tabs_rv$help_flag))
    
    # message(sum(tabs_rv$plots_flag, tabs_rv$assdose_flag, tabs_rv$tox_flag, tabs_rv$patdata_flag, tabs_rv$help_flag))
  })
  
  # Help & video buttons for get started page:
  
  output$start_help <- renderUI({
    actionButton(inputId = "help_start", label = "Further Explanation",
                 # style = "position: absolute; right: 40px; background-color: dark grey")
                 style = "background-color: dark grey")
  })
  
  output$start_video <- renderUI({
    actionButton(inputId = "help_vid", label = div("Video Explanation", icon("circle-play")),
                 # style = "position: absolute; right: 40px; background-color: dark grey")
                   style = "background-color: dark grey")
  })
  
  # Video pop-up
  
  # Open modal
  observeEvent(input$help_vid, {

    showModal(modalDialog(
      title = "How to navigate ALL-MAVEN (ALL MT App)",
      HTML('<p style="text-align:center;">
            <iframe width="560" height="315" src="https://www.youtube.com/embed/AVW1-wr8VOM" title="How to use the ALL MT App" 
            frameborder="0" allow="accelerometer; autoplay; clipboard-write; encrypted-media; gyroscope; picture-in-picture; 
            web-share" allowfullscreen></iframe>
          </p>'),
      size = "l",
      footer = actionButton("close_video", "Close"),
      easyClose = TRUE) 
    )
  })
  # Close modal
  observeEvent(input$close_video, {
    removeModal()
  })
  

  ## LEVEL: INDIVIDUAL : 
  
  #Individual functions - LG, cycSM, Assess dose decisions and/or Hemat toxicity input ui
  observeEvent(eventExpr = input$go, {

    input_vals$function_list <- input$MT_Func
    print(input_vals$function_list)

    print(input_vals$input_status)

    if(input_vals$input_status != 'uploaded'){
      print(input_vals$input_status)
      alert(text = "Please select a patient file")
      return(NULL)
      } else{
        
    if(!file_ext(input_df()$name) %in% c("csv", "xls", "xlsx")){
      print("Should alert")
      alert(text = "Please select a '.csv', '.xls' or '.xlsx' file")
      return(NULL)
    } else{   
      
    if(is.null(input_vals$function_list)){
      alert("Please select analysis function")
      return(NULL)
      } else{
        
    if(any(("Ind_LG" %in% input_vals$function_list)|("Ind_csSM" %in% input_vals$function_list))){
    if(is.null(input$unit) | is.na(input$ANC_lower) | is.na(input$ANC_upper)){
      alert("Please select all input values")
      return(NULL)
    } else{
      
    print("file ready to run")
 
    if("Ind_LG" %in% input_vals$function_list){
      
      output$LG_Graph <- renderPlot({
        if (input_vals$input_status == "uploaded") {
          df <- input_df()
          print(df)
          plot_progression(df$datapath, input$unit,
                           anc_range = c(input$ANC_lower, input$ANC_upper))
        }
      })
      
    } else{
      output$LG_Graph <- NULL

    } 
      
    if("Ind_csSM" %in% input_vals$function_list){
      
      output$Cyc_Graph <- renderPlot({
        if (input_vals$input_status == "uploaded") {
          df <- input_df()
          summarize_cycle_progression(df$datapath, input$unit,
                                      anc_range = c(input$ANC_lower, input$ANC_upper))
        }
        
      })
    } else{
      output$Cyc_Graph <- NULL
    }

    }
      
    # Append Plots tab
    if(tabs_rv$plots_flag == 0){
      print("append plots")
      appendTab(inputId = "PanelID", select = TRUE,
                tab = tabPanel(title = "Plots",
                               HTML("<br>"),
                               actionButton(inputId = "close_plots_tab", label = " Close tab", icon = icon("arrow-left"),
                                            style = "position: absolute; right: 40px"),
                               actionButton(inputId = "see_pat", label = "Load Patient Data",
                                            style = "position: absolute; left;"),
                               actionButton(inputId = "help_go", label = "Help",
                                            style = "position: absolute; left: 550px; background-color: dark grey"),
                               HTML("<br>"),
                               HTML("<br>"),
                               column(6,
                                      # h4("Longitudinal Patient History", align = "center"),
                                      # tags$style("#LG_Graph { max-width: 200px; }"),
                                      offset = 0.5, plotOutput("LG_Graph", width ="100%", height = 500), style = "margin-top: 40px",
                                      HTML("<br>")
                               ),
                               column(6, 
                                      # h4("Cycle-based progression", align = "center"),
                                      plotOutput("Cyc_Graph", width = "100%", height = 500), style = "margin-top: 40px",
                                      HTML("<br>"))))
      
      tabs_rv$plots_flag = 1
      tabs_rv$plots_num = tabs_n()
      print(paste0("Plots tab num = ", tabs_rv$plots_num))
      
      print("Appended")
    } 
      
      
      showTab(inputId = "PanelID",  target = "Plots", select = TRUE)
      
    } else{
      removeTab(inputId = "PanelID", target = "Plots")
      tabs_rv$plots_flag = 0
    }
    
    if("ass_dose" %in% input_vals$function_list) {
      
        if(tabs_rv$assdose_flag == 0){
          print("dose append")
          appendTab(inputId = "PanelID", select = TRUE,
                    tab = tabPanel(title = "Dose Decisions", 
                                   # Row 1: Title box --------
                                   fluidRow(
                                     box(solidHeader = TRUE, width = 12,
                                         actionButton(inputId = "close_ass_dose_tab", label = " Close tab", icon = icon("arrow-left"),
                                                      style = "position: absolute; right: 40px"),
                                           actionButton(inputId = "see_pat1", label = "Load Patient Data",
                                                        style = "position: absolute; left: 20px; background-color: dark grey"),
                                         actionButton(inputId = "help_ind_assdose", label = "Help",
                                                      style = "position: absolute; left: 200px; background-color: dark grey"),
                                         h4(strong("For dose decision analysis:"), style = "margin-top: 20px;", align = "center"),
                                         h5(strong(HTML("Fill in <u>protocol-based</u> target blood count values")), align = "center"),
                                         # h5(HTML("<b>Key</b>: 'ANC' = Absolute neutrophil count (x10<sup>6</sup>/L or x10<sup>9</sup>/uL); <br> 
                                         h5(HTML("<b>Key</b>: 'ANC' = Absolute neutrophil count; <br> 
                                                  &emsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp; 'PLT' = Platelet count"), align = "left",style = 'line-height:1.5')
                                     )
                                   ),
                                   # ------
                                   
                                   
                                   
                                   # Row 2: Blood count thresholds --------
                                   fluidRow(
                                     tags$style(HTML("
                                                    .tooltip > .tooltip-inner {
                                                    background-color: white;
                                                    color: black;
                                                    border: 1px black;
                                                    padding: 5px;
                                                    font-size: 12px;
                                                    margin-top: 5px;
                                                    width: 500px;
                                                    }
                                                    ")),
                                     # Info icons:
                                     tags$head(
                                       tags$style(HTML("
                                                   #tld_help {
                                                   display: inline;
                                                   margin: 5px;
                                                   color: black;
                                                   width: 500px;
                                                   }
                                                   ")
                                       )
                                     ),
                                     # Box 1: Stop Dose:
                                     box(solidHeader = TRUE, width = 4,
                                         h5(strong("(A) STOP decision is taken when the count is BELOW:")),
                                         splitLayout(cellWidths = c('50%', '50%'),
                                                     numericInput(
                                                       inputId = "stop_ANC_lower",
                                                       label = "ANC maximum threshold",
                                                       value = input_vals$stop_ANC_lower,
                                                       width = '100px'),
                                                     
                                                     numericInput(
                                                       inputId = "stop_PLT_lower",
                                                       label = "PLT maximum threshold",
                                                       value = input_vals$stop_PLT_lower,
                                                       width = '100px'))
                                     ),
                                     # Box 2: Reduce Dose:
                                     box(solidHeader = TRUE, width = 4,
                                         h5(strong("(B) REDUCE decision is taken when the count is BETWEEN:")),
                                         splitLayout(cellWidths = c('50%', '50%'),
                                                     numericInput(
                                                       inputId = "red_ANC_lower",
                                                       label = "ANC minimum threshold",
                                                       value = input_vals$red_ANC_lower,
                                                       width = '100px'),
                                                     
                                                     numericInput(
                                                       inputId = "red_ANC_upper",
                                                       label = "ANC maximum Threshold",
                                                       value = input_vals$red_ANC_upper,
                                                       width = '100px'),
                                         ),
                                         splitLayout(cellWidths = c('50%', '50%'),
                                                     numericInput(
                                                       inputId = "red_PLT_lower",
                                                       label = "PLT minimum threshold",
                                                       value = input_vals$red_PLT_lower,
                                                       width = '100px'),
                                                     
                                                     numericInput(
                                                       inputId = "red_PLT_upper",
                                                       label = "PLT maximum Threshold",
                                                       value = input_vals$red_PLT_upper,
                                                       width = '100px'),
                                         ),
                                         sliderInput(inputId = "red_factor",
                                                     label = "Reduce dose by (%)",
                                                     min = 0,max = 100, value = input_vals$red_factor,
                                                     step = 0.5, ticks = TRUE),
                                     ),
                                     
                                     # Box 3: Increase Dose:
                                     box(solidHeader = TRUE, width = 4,
                                         h5(HTML("<b>(C) INCREASE decision can be taken when the count is ABOVE:</b>")),
                                         splitLayout(cellWidths = c('50%', '50%'),
                                                     numericInput(
                                                       inputId = "inc_ANC_upper",
                                                       label = "ANC minimum threshold",
                                                       value = input_vals$inc_ANC_upper,
                                                       width = '100px'),
                                                     
                                                     numericInput(
                                                       inputId = "inc_PLT_upper",
                                                       label = "PLT minimum Threshold",
                                                       value = input_vals$inc_PLT_upper,
                                                       width = '100px'),
                                         ),
                                         
                                         sliderInput(inputId = "inc_factor",
                                                     label = "Escalate dose by (%)",
                                                     min = 0,max = 100, value = input_vals$inc_factor,
                                                     step = 0.5, ticks = TRUE),
                                         numericInput(inputId = "tld",
                                                      label = HTML("Tolerated dose duration (weeks)",
                                                                   as.character(actionLink(inputId = "tld_help", label = "", 
                                                                              icon = icon("circle-info")))),
                                                      value = input_vals$inc_tld, min = 1,
                                                      step = 1, width = '100%'),
                                         bsTooltip(id = "tld_help", title= "Number of weeks of consistent doses, following which dose can be increased", 
                                                   placement = "bottom", trigger = "hover"),
                                     ),
                                   ),
                                   
                                   # -------
                                   
                                   # Row 3: Disclaimer and submit ------
                                   p("Note : starting dose will be considered as the 100% dose intensity for each patient" ,
                                     style = "margin-top: 10px;", align = "center"),
                                   h2(actionButton(inputId = "go2", label = "Assess Decisions"),align = "center"),
                                   # --------
                                   
                                   # Output ---------
                                   fluidRow(
                                     tags$style(HTML("
                                                  .tooltip > .tooltip-inner {
                                                  background-color: white;
                                                  color: black;
                                                  border: 1px black;
                                                  padding: 5px;
                                                  font-size: 12px;
                                                  margin-top: 5px;
                                                  width: 600px;
                                                  }
                                                  ")),
                                     tags$head(
                                       tags$style(
                                         HTML("
                                             #inc_ass_help {
                                             display: inline;
                                             margin: 5px;
                                             color: white;
                                             width: 500px;
                                             }
                                             #reducedose_help {
                                             display: inline;
                                             margin: 10px;
                                             color: black;
                                             width: 500px;
                                             }
                                             #stopdose_help {
                                             display: inline;
                                             margin: 10px;
                                             color: black;
                                             width: 500px;
                                             }
                                             #increasedose_help {
                                             display: inline;
                                             margin: 10px;
                                             color: black;
                                             width: 500px;
                                             }
                                              ")
                                       )
                                     ),
                                     box(uiOutput("StopDose"),width = 4, title = HTML("Stop Dose Decision Analysis", 
                                                                                      as.character(actionLink(inputId = "stopdose_help",
                                                                                                              label = "", 
                                                                                                              icon = icon("circle-info")))),
                                         align = "center",
                                         bsTooltip(id = "stopdose_help", title= "Assess decisions to stop 6MP and MTX doses", 
                                                   placement = "bottom", trigger = "hover"),
                                     ),
                                     box(uiOutput("ReduceDose"),width = 4, title = HTML("Reduce Dose Decision Analysis",   
                                                                                        as.character(actionLink(inputId = "reducedose_help",
                                                                                                                label = "", 
                                                                                                                icon = icon("circle-info")))),
                                         align = "center",
                                         bsTooltip(id = "reducedose_help", title= "Assess decisions to reduce 6MP and MTX doses below a reduction factor below 100\\%\\ dose", 
                                                   placement = "bottom", trigger = "hover"),
                                     ),
                                     box(uiOutput("IncreaseDose"),width = 4, title = HTML("Increase Dose Decision Analysis",  
                                                                                          as.character(actionLink(inputId = "increasedose_help",
                                                                                                                  label = "", 
                                                                                                                  icon = icon("circle-info")))), 
                                         align = "center",
                                         bsTooltip(id = "increasedose_help", title= "Assess decisions to increase 6MP and MTX doses above a escalation factor of the 100\\%\\ dose", 
                                                   placement = "left", trigger = "hover"),
                                     )
                                   ),
                                   h3(actionButton(inputId = "DoseInc", 
                                                   label = HTML("Assess time to dose escalation", 
                                                                as.character(actionLink(inputId = "inc_ass_help",
                                                                                        label = "", 
                                                                                        icon = icon("circle-info")))),
                                                   style = "background-color: #188dcf; color: white;"), 
                                      align = "right", style = "margin-right: 90px;",
                                   bsTooltip(id = "inc_ass_help", title= "How long did it take for the first dose increase?", 
                                             placement = "top", trigger = "hover"),
                                   )
                                   
                                   # ------
                    )
          )
          tabs_rv$assdose_flag = 1
          tabs_rv$assdose_num = tabs_n()
          print(paste0("ass dose tab num = ", tabs_rv$assdose_num))
          
        }
        
        showTab(inputId = "PanelID",  target = "Dose Decisions", select = TRUE)
        print(input$go2)
      } else{
        removeTab(inputId = "PanelID", target = "Dose Decisions")
        tabs_rv$assdose_flag = 0
      }
        
    if("hemat_tox" %in% input_vals$function_list){
        
        # if(is.null(tabs_rv$tox)){
        if(tabs_rv$tox_flag == 0){
          print("hemat_append_ind")
          appendTab(inputId = "PanelID", select = TRUE,
                    tab = tabPanel(title = "Toxicity",
                                   # Row 1: Title box --------
                                   fluidRow(
                                     box(solidHeader = TRUE, width = 12,
                                         actionButton(inputId = "close_tox_tab", label = " Close tab", icon = icon("arrow-left"),
                                                      style = "position: absolute; right: 40px"),
                                                          actionButton(inputId = "see_pat2", label = "Load Patient Data",
                                                      style = "position: absolute; left: 20px; background-color: dark grey"),
                                         actionButton(inputId = "help_ind_tox", label = "Help",
                                                      style = "position: absolute; left: 200px;  background-color: dark grey"),
                                         h4(strong("For toxicity analysis:"), style = "margin-top: 20px;", align = "center"),
                                         h5(strong(HTML("Fill in <u>protocol-based</u> toxicity threshold and recovery blood count values")), align = "center"),
                                         # h5(HTML("<b>Key</b>: 'ANC' = Absolute neutrophil count (x10<sup>6</sup>/L or x10<sup>9</sup>/uL); <br> 
                                         h5(HTML("<b>Key</b>: 'ANC' = Absolute neutrophil count; <br> 
                                                  &emsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp; 'PLT' = Platelet count"), align = "left",style = 'line-height:1.5')
                                     )
                                   ),
                                   # ------
                                   
                                   # Row 2: Blood count thresholds --------
                                   fluidRow(
                                     tags$style(HTML("
                                                    .tooltip > .tooltip-inner {
                                                    background-color: white;
                                                    color: black;
                                                    border: 1px black;
                                                    padding: 5px;
                                                    font-size: 12px;
                                                    margin-top: 5px;
                                                    width: 500px;
                                                    }
                                                    ")),
                                     # Info icons:
                                     tags$head(
                                       tags$style(HTML("
                                                   #dur_anc_help {
                                                   display: inline;
                                                   margin: 5px;
                                                   color: black;
                                                   width: 500px;
                                                   }
                                                   #dur_plt_help {
                                                   display: inline;
                                                   margin: 5px;
                                                   color: black;
                                                   width: 500px;
                                                   }
                                                   #dur_hb_help {
                                                   display: inline;
                                                   margin: 5px;
                                                   color: black;
                                                   width: 500px;
                                                   }
                                                     ")
                                       )
                                     ),
                                     # Box 1: Neutropenia:
                                     box(solidHeader = TRUE, width = 4,
                                         h5(strong("Neutropenia occurs when")),
                                         splitLayout(cellWidths = c('50%', '50%'),
                                                     numericInput(
                                                       inputId = "neut_anc",
                                                       label = "ANC below:",
                                                       value = input_vals$neut_anc,
                                                       width = '100px'),
                                                     
                                                     numericInput(
                                                       inputId = "anc_recov",
                                                       label = "Recovery Threshold",
                                                       value = input_vals$anc_recov,
                                                       width = '100px')),
                                         sliderInput(inputId = "dur_anc",
                                                     label = HTML("Long duration neutropenia",
                                                                  as.character(actionLink(inputId = "dur_anc_help", label = "", 
                                                                                          icon = icon("circle-info")))),
                                                     min = 1, max = 15, value = input_vals$dur_anc, 
                                                     step = 1, width = '380px', post = " week(s)"),
                                         bsTooltip(id = "dur_anc_help", title= "Number of weeks to consider toxicity as \\'prolonged\\' neutropenia", 
                                                   placement = "right", trigger = "hover"),
                                     ),
                                     # Box 2: Thrombocytopenia:
                                     box(solidHeader = TRUE, width = 4,
                                         h5(strong("Thrombocytopenia occurs when")),
                                         splitLayout(cellWidths = c('50%', '50%'),
                                                     numericInput(
                                                       inputId = "thromb_plt",
                                                       label = "PLT below:",
                                                       value = input_vals$thromb_plt,
                                                       width = '100px'),
                                                     
                                                     numericInput(
                                                       inputId = "plt_recov",
                                                       label = "Recovery Threshold",
                                                       value = input_vals$plt_recov,
                                                       width = '100px')),
                                         sliderInput(inputId = "dur_plt",
                                                     label = HTML("Long duration thrombocytopenia", 
                                                                  as.character(actionLink(inputId = "dur_plt_help", label = "", 
                                                                                          icon = icon("circle-info")))),     
                                                     min = 1, max = 15, value = input_vals$dur_plt, 
                                                     step = 1, width = '380px', post = " week(s)"),
                                         bsTooltip(id = "dur_plt_help", title= "Number of weeks to consider toxicity as \\'prolonged\\' thrombocytopenia", 
                                                   placement = "right", trigger = "hover"),
                                     ),
                                     #Box 3: Anaemia:
                                     box(solidHeader = TRUE, width = 4,
                                         h5(strong("Anaemia occurs when ")),
                                         splitLayout(cellWidths = c('50%', '50%'),
                                                     numericInput(
                                                       inputId = "anaem_Hb",
                                                       label = "Hb below:",
                                                       value = input_vals$anaem_Hb,
                                                       width = '100px'),
                                                     
                                                     numericInput(
                                                       inputId = "Hb_recov",
                                                       label = "Recovery Threshold",
                                                       value = input_vals$Hb_recov,
                                                       width = '100px')),
                                         sliderInput(inputId = "dur_hb",
                                                     label = HTML("Long duration anaemia", 
                                                                  as.character(actionLink(inputId = "dur_hb_help", label = "", 
                                                                                          icon = icon("circle-info")))),
                                                     min = 1, max = 15, value = input_vals$dur_hb, 
                                                     step = 1, width = '380px', post = " week(s)"),
                                         bsTooltip(id = "dur_hb_help", title= "Number of weeks to consider toxicity as \\'prolonged\\' anaemia", 
                                                   placement = "right", trigger = "hover"),
                                     )
                                   ),
                                   
                                   # -------
                                   
                                   # Row 3: Submit ------
                                   h2(actionButton(inputId = "go3", label = "Assess Toxicity"), align = "center"),
                                   # --------
                                   
                                   # Output ---------
                                   fluidRow(
                                     box(uiOutput("NeutTox"),width = 4, title = "Neutropenia Analysis", align = "center"),
                                     box(uiOutput("ThrombTox"),width = 4, title = "Thrombocytopenia Analysis",  align = "center"),
                                     box(uiOutput("AnaemTox"),width = 4, title = "Anaemia Analysis",  align = "center")
                                   )
                                   # ------
                                   
                    ))
          # print(paste0("old number of tabs open = ", tabs_rv$n))
          tabs_rv$tox_flag = 1
          tabs_rv$tox_num = tabs_n()
          print(paste0("tox tab num = ", tabs_rv$tox_num))
          # print(paste0("new number of tabs open= ", tabs_rv$n))
        }
      
        showTab(inputId = "PanelID",  target = "Toxicity", select = TRUE)
      } else{
        removeTab(inputId = "PanelID", target = "Toxicity")
        tabs_rv$tox_flag = 0
        # print(paste0("tox_new number of tabs open= ", tabs_n()))
      }
     
     }
    }
    
      }
  })

  #Patient data tab
  observeEvent(eventExpr = c(input$see_pat, input$see_pat1, input$see_pat2), {
    
    print(paste0("input see pat = ", input$see_pat))
    print(input$see_pat1)
    print(input$see_pat2)
    
    if(tabs_rv$current_tab == "Plots"){
      if(input$see_pat > 0){
        tabs_rv$patinput = 1} else{tabs_rv$patinput = 0}
    }
    
    if(tabs_rv$current_tab == "Dose Decisions"){
      if(input$see_pat1 > 0){
        tabs_rv$patinput = 1} else{tabs_rv$patinput = 0}
    }
    
    if(tabs_rv$current_tab == "Toxicity"){
      if(input$see_pat2 > 0){
        tabs_rv$patinput = 1} else{tabs_rv$patinput = 0}
    }
    
    print(paste0("pat data tab : ",tabs_rv$current_tab))
    print(tabs_rv$patinput)
    
    if(tabs_rv$patinput > 0){
      
      if(input_vals$input_status == "uploaded"){
        
        df <- input_df()
        
        if(file_ext(df$datapath) == "csv"){
          print("file path = csv, reading pat data file for display")
          df_read <- read.csv(df$datapath)
        }
        if(file_ext(df$datapath) %in% c("xls", "xlsx")){
          # print("file path = excel, reading data file for display")
          df_read <- read_excel(df$datapath)
        }
        
        output$pat_data <- renderTable({
          df_read},
          striped = TRUE, colnames = TRUE, rownames = FALSE, spacing = "l", width = "80%", align = "c")

      }
      
      print(tabs_rv$current_tab)
      print(paste0("tabs_rv$patdata_flag = ", tabs_rv$patdata_flag))
      
      if(tabs_rv$patdata_flag == 1){
        disable("see_pat")
        disable("see_pat1")
        disable("see_pat2")
        
      }
      
      if(all(tabs_rv$patdata_flag != 1 & tabs_rv$current_tab != "Patient Data")){
        
        appendTab(inputId = "PanelID", select = TRUE,
                  tab = tabPanel(title = "Patient Data",
                                 h4("Patient history"),
                                 actionButton(inputId = "close_pat_tab", label = "close tab", icon = icon("arrow-left"),
                                              style = "position: absolute; right: 40px"),
                                 tableOutput("pat_data")
                                 # DT::dataTableOutput("pat_data"),
                  )
        )
        tabs_rv$patdata_flag = 1
        tabs_rv$pat_num = tabs_n()
        print(paste0("pat tab num = ", tabs_rv$pat_num))
        
        # shinyjs::hide("see_pat")
        # shinyjs::hide("see_pat1")
        # shinyjs::hide("see_pat2")
        
      }
      
    }
    
  })
  
  #Close patient data tab
  observeEvent(eventExpr = input$close_pat_tab, handlerExpr = {
    
    removeTab("PanelID", target = "Patient Data")
    tabs_rv$patdata_flag = 0
    updateTabsetPanel(inputId = "PanelID", selected = tabs_rv$previous_tab)
    
    #Changing other tab numbers as required
    if(tabs_rv$plots_num > tabs_rv$pat_num){
      tabs_rv$plots_num = tabs_rv$plots_num-1
      print(paste0("new plots num = ", tabs_rv$plots_num))
    }
    if(tabs_rv$help_num > tabs_rv$pat_num){
      tabs_rv$help_num = tabs_rv$help_num-1
      print(paste0("new help num = ", tabs_rv$help_num))
    }
    if(tabs_rv$assdose_num > tabs_rv$pat_num){
      tabs_rv$assdose_num = tabs_rv$assdose_num-1
      print(paste0("new ass dose num = ", tabs_rv$assdose_num))
    }
    if(tabs_rv$tox_num > tabs_rv$pat_num){
      tabs_rv$tox_num = tabs_rv$tox_num-1
      print(paste0("new tox num = ", tabs_rv$tox_num))
    }

    tabs_rv$pat_num = 0
   
    tabs_rv$patinput = 0

    shinyjs::show("see_pat")
    shinyjs::show("see_pat1")
    shinyjs::show("see_pat2")

    enable("see_pat")
    enable("see_pat1")
    enable("see_pat2")
    
  })
  
  ##LEVEL: COHORT : 

  #Cohort functions - SM, CC, Assess dose decisions and/or Hemat toxicity input ui
  observeEvent(eventExpr = input$go1, priority = 1, handlerExpr = {
    
    print("go1")
    
    # input_vals$cohort_IDs <- input$cohort_IDs
    input_vals$Func_choice <- c(input$cMT_Func)
    
    print(input_vals$c_input_status)
    
    # Checking if files present

    if(input_vals$c_input_status != "uploaded"){
      alert(text = "Please select patient files and then analyze")
      return(NULL)
    } else{
  
    # Warning if only one file selected
    if(nrow(cohort_IDs()) == 1){
      # output$cohort_warning <- renderText("Warning: only 1 patient selected")
      alert ("Note: only 1 patient selected")
      print("Note: only 1 patient selected")
    }
    
    #Checking if all files in correct format
    if(!all(file_ext(cohort_IDs()$name) %in% c("csv", "xls", "xlsx"))){
      print("Should alert")
      alert(text = "Please select '.csv', '.xls' or '.xlsx' files")
      return(NULL)
    } else{

    if(is.null(input_vals$Func_choice)){
        alert("Please select analysis function")
        return(NULL)
      } else{
      
    print(input_vals$Func_choice)
    
    # Running functions if all validations passed
        
    if(any(("Cohort_SM" %in% input_vals$Func_choice)|("Comp_cohorts" %in% input_vals$Func_choice))){
      
      if("Cohort_SM" %in% input_vals$Func_choice){
        print("cohort_SM")
        
        if(any(is.null(input$c_unit) | is.na(input$cANC_lower) |is.na(input$cANC_upper) |
               is.na(input$c_DI))){
          alert("Please select all input values.")
          return(NULL)
        } else{
          
          output$SM_Graph <- renderPlotly({
            if (input_vals$c_input_status == "uploaded") {
              print(input_vals$cohort_IDs)
              cID_list <- cohort_IDs()
              print(cID_list$name)
              
              if(all(file_ext(cID_list$datapath) == "csv")){
                df <- lapply(cID_list$datapath, read.csv)
              }
              
              if(all(file_ext(cID_list$datapath) %in%  c("xls", "xlsx"))){
                df <- lapply(cID_list$datapath, read_excel)
              }
              
              df <- setNames(df, cID_list$name)
              print("listed")
              print(c(input$cANC_lower, input$cANC_upper))
              summarize_cohortMT(pat_list = df, unit = input$c_unit,
                                 anc_range = c(input$cANC_lower, input$cANC_upper),
                                 dose_intensity_threshold = input$c_DI)
              # print("Summary")
            }
          })
        } 
        
        # updateTabsetPanel(session, "PanelID", selected = "Plots")
      } else{
        output$SM_Graph <- NULL
      }
      
      # Append Plots tab
      if(tabs_rv$plots_flag == 0){
      print("append cohort plots")
      appendTab(inputId = "PanelID", select = TRUE,
                tab = tabPanel("Plots", 
                               HTML("<br><br>"),
                               actionButton(inputId = "close_plots_tab", label = " Close tab", icon = icon("arrow-left"),
                                            style = "position: absolute; right: 40px"),
                               # actionButton(inputId = "pat_see", label = "Load Patient Data",
                               #              style = "position: absolute; left: 20px; background-color: dark grey"),
                               actionButton(inputId = "help_go1", label = "Help",
                                            style = "position: absolute; left; background-color: dark grey"),
                               textOutput("cohort_warning"),
                               tags$head(tags$style("cohort_warning{color: red;font-size: 20px;}")),
                               HTML("<br><br>"),
                               conditionalPanel(
                                 condition = "input.go1 >= 1 &&  input.cMT_Func.includes('Comp_cohorts')",
                                 dateInput(inputId = "int_date",
                                           label = HTML("Enter date of intervention",  
                                                        as.character(actionLink(inputId = "int_date_help", label = "", icon = icon("circle-info")))),
                                           autoclose = TRUE), style = "position: absolute; right: 240px; top: 120px",
                                 bsTooltip(id = "int_date_help", 
                                           title= "Start date of MT is the considered date for each patient to group into \\'pre-\\' or \\'post-\\' intervention", 
                                           placement = "right", trigger = "hover"),
                                 uiOutput("csSM_apply"),
                               ),
                               
                               column(5, 
                                      # h4("Treatment Summary", align = "center"),
                                      # offset = 0.5, plotlyOutput("SM_Graph", width = "400px", height = "500px"), style = "margin-top: 40px"),
                                      offset = 0.5, plotlyOutput("SM_Graph",  height = "500px"), style = "margin-top: 40px"),
                               column(7, 
                                      # h4("Treatment Comparison", align = "center"),
                                      # plotlyOutput("CC_Graph", width = "600px", height = "500px"), style = "margin-top: 40px")
                                      plotlyOutput("CC_Graph",  height = "500px"), style = "margin-top: 40px")))
      tabs_rv$plots_flag = 1
      tabs_rv$plots_num = tabs_n()
      print(paste0("Plots tab num = ", tabs_rv$plots_num))
      
      print("Appended")
      } 
      
        showTab(inputId = "PanelID",  target = "Plots", select = TRUE)
        
        } else{
          removeTab(inputId = "PanelID", target = "Plots")
          tabs_rv$plots_flag = 0
        }

    if("ass_dose" %in% input_vals$Func_choice) {

     # if(is.null(tabs_rv$assdose)){
      if(tabs_rv$assdose_flag == 0){
        print("dose append")
            appendTab(inputId = "PanelID", select = TRUE,
                      tab = tabPanel(title = "Dose Decisions", 
                     # Row 1: Title box --------
                     fluidRow(
                       box(solidHeader = TRUE, width = 12,
                           actionButton(inputId = "close_ass_dose_tab", label = " Close tab", icon = icon("arrow-left"),
                                        style = "position: absolute; right: 40px"),
                           actionButton(inputId = "help_c_assdose", label = "Help",
                                        # style = "position: absolute; right: 160px; background-color: dark grey"),
                                        style = "position: absolute; left; background-color: dark grey"),
                           h4(strong("For decision analysis:"), style = "margin-top: 20px;", align = "center"),
                           h5(strong(HTML("Fill in <u>protocol-based</u> target blood count values")), align = "center"),
                           # h5(HTML("<b>Key</b>: 'ANC' = Absolute neutrophil count (x10<sup>6</sup>/L or x10<sup>9</sup>/uL); <br> 
                           h5(HTML("<b>Key</b>: 'ANC' = Absolute neutrophil count; <br> 
                                                  &emsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp; 'PLT' = Platelet count"), align = "left",style = 'line-height:1.5')
                       )
                     ),
                     # ------
                     
                     # Row 2: Blood count thresholds --------
                     fluidRow(
                       tags$style(HTML("
                                                    .tooltip > .tooltip-inner {
                                                    background-color: white;
                                                    color: black;
                                                    border: 1px black;
                                                    padding: 5px;
                                                    font-size: 12px;
                                                    margin-top: 5px;
                                                    width: 500px;
                                                    }
                                                    ")),
                       # Info icons:
                       tags$head(
                         tags$style(HTML("
                                                   #tld_help {
                                                   display: inline;
                                                   margin: 5px;
                                                   color: black;
                                                   width: 500px;
                                                   }
                                                   ")
                         )
                       ),
                       # Box 1: Stop Dose:
                       box(solidHeader = TRUE, width = 4,
                           h5(strong("(A) STOP decision is taken when the count is BELOW :")),
                           splitLayout(cellWidths = c('50%', '50%'),
                                       numericInput(
                                         inputId = "stop_ANC_lower",
                                         label = "ANC threshold",
                                         value = input_vals$stop_ANC_lower,
                                         width = '100px'),
                                       
                                       numericInput(
                                         inputId = "stop_PLT_lower",
                                         label = "PLT Threshold",
                                         value = input_vals$stop_PLT_lower,
                                         width = '100px'))
                       ),
                       # Box 2: Reduce Dose:
                       box(solidHeader = TRUE, width = 4,
                           h5(strong("(B) REDUCE decision is taken when the count is BETWEEN :")),
                           splitLayout(cellWidths = c('50%', '50%'),
                                       numericInput(
                                         inputId = "red_ANC_lower",
                                         label = "ANC lower threshold",
                                         value = input_vals$red_ANC_lower,
                                         width = '100px'),
                                       
                                       numericInput(
                                         inputId = "red_ANC_upper",
                                         label = "ANC upper Threshold",
                                         value = input_vals$red_ANC_upper,
                                         width = '100px'),
                           ),
                           splitLayout(cellWidths = c('50%', '50%'),
                                       numericInput(
                                         inputId = "red_PLT_lower",
                                         label = "PLT lower threshold",
                                         value = input_vals$red_PLT_lower,
                                         width = '100px'),
                                       
                                       numericInput(
                                         inputId = "red_PLT_upper",
                                         label = "PLT upper Threshold",
                                         value = input_vals$red_PLT_upper,
                                         width = '100px'),
                           ),
                           sliderInput(inputId = "red_factor",
                                       label = "Reduce dose by (%)",
                                       min = 0,max = 100, value = input_vals$red_factor,
                                       step = 0.5, ticks = TRUE),
                       ),
                       
                       # Box 3: Increase Dose:
                       box(solidHeader = TRUE, width = 4,
                           h5(strong("(C) INCREASE decision can be taken when the count is ABOVE :")),
                           splitLayout(cellWidths = c('50%', '50%'),
                                       numericInput(
                                         inputId = "inc_ANC_upper",
                                         label = "ANC upper threshold",
                                         value = input_vals$inc_ANC_upper,
                                         width = '100px'),
                                       
                                       numericInput(
                                         inputId = "inc_PLT_upper",
                                         label = "PLT upper Threshold",
                                         value = input_vals$inc_PLT_upper,
                                         width = '100px'),
                           ),
                           
                           sliderInput(inputId = "inc_factor",
                                       label = "Escalate dose by (%)",
                                       min = 0,max = 100, value = input_vals$inc_factor,
                                       step = 0.5, ticks = TRUE),
                           numericInput(inputId = "tld",
                                        label = HTML("Tolerated dose duration (weeks)",
                                                     as.character(actionLink(inputId = "tld_help", label = "", 
                                                                             icon = icon("circle-info")))),
                                        value = input_vals$inc_tld, min = 1,
                                        step = 1, width = '100%'),
                           bsTooltip(id = "tld_help", title= "Number of weeks of consistent doses, following which dose can be increased", 
                                     placement = "bottom", trigger = "hover"),
                       ),
                     ),
                     
                     # -------
                     
                     # Row 3: Disclaimer and submit ------
                     p("Note : starting dose will be considered as the 100% dose intensity for each patient" ,
                       style = "margin-top: 10px;", align = "center"),
                     h2(actionButton(inputId = "go2", label = "Assess Decisions"), align = "center"),
                     # --------
                     
                     # Output ---------
                     fluidRow(
                       tags$style(HTML("
                                                  .tooltip > .tooltip-inner {
                                                  background-color: white;
                                                  color: black;
                                                  border: 1px black;
                                                  padding: 5px;
                                                  font-size: 12px;
                                                  margin-top: 5px;
                                                  width: 500px;
                                                  }
                                                  ")),
                       tags$head(
                         tags$style(
                           HTML("
                                             #inc_ass_help {
                                             display: inline;
                                             margin: 5px;
                                             color: white;
                                             width: 500px;
                                             }
                                             #stopdose_help {
                                             display: inline;
                                             margin: 10px;
                                             color: black;
                                             width: 500px;
                                             }
                                             #reducedose_help {
                                             display: inline;
                                             margin: 10px;
                                             color: black;
                                             width: 500px;
                                             }
                                             #increasedose_help {
                                             display: inline;
                                             margin: 10px;
                                             color: black;
                                             width: 500px;
                                             }
                                               ")
                         )
                       ),
                       
                       box(uiOutput("StopDose"),width = 4, title = HTML("Stop Dose Decision Analysis", 
                                                                        as.character(actionLink(inputId = "stopdose_help",
                                                                                                label = "", 
                                                                                                icon = icon("circle-info")))),
                           align = "center",
                           bsTooltip(id = "stopdose_help", title= "Assess decisions to stop 6MP and MTX doses", 
                                     placement = "bottom", trigger = "hover"),
                       ),
                       box(uiOutput("ReduceDose"),width = 4, title = HTML("Reduce Dose Decision Analysis",   
                                                                          as.character(actionLink(inputId = "reducedose_help",
                                                                                                  label = "", 
                                                                                                  icon = icon("circle-info")))),
                           align = "center",
                           bsTooltip(id = "reducedose_help", title= "Assess decisions to reduce 6MP and MTX doses below a reduction factor below 100\\%\\ dose", 
                                     placement = "bottom", trigger = "hover"),
                       ),
                       box(uiOutput("IncreaseDose"),width = 4, title = HTML("Increase Dose Decision Analysis",  
                                                                            as.character(actionLink(inputId = "increasedose_help",
                                                                                                    label = "", 
                                                                                                    icon = icon("circle-info")))), 
                           align = "center",
                           bsTooltip(id = "increasedose_help", title= "Assess decisions to increase 6MP and MTX doses above a escalation factor of the 100\\%\\ dose", 
                                     placement = "left", trigger = "hover"),
                       )
                     ),
                     h3(actionButton(inputId = "DoseInc", 
                                     label = HTML("Assess time to dose escalation", 
                                                  as.character(actionLink(inputId = "inc_ass_help",
                                                                          label = "", 
                                                                          icon = icon("circle-info")))),
                                     style = "background-color: #188dcf; color: white;"), 
                        align = "right", style = "margin-right: 90px;",
                        bsTooltip(id = "inc_ass_help", title= "How long did it take for the first dose increase?", 
                                  placement = "top", trigger = "hover"),
                     )
                     # ------
            )
          )
            tabs_rv$assdose_flag = 1
            # print(paste0("new number of tabs open= ", tabs_n()))
            tabs_rv$assdose_num = tabs_n()
            print(paste0("ass dose tab num = ", tabs_rv$assdose_num))
  
          }
          
          showTab(inputId = "PanelID",  target = "Dose Decisions", select = TRUE)
          tabs_rv$helpinput = 0
          print(paste0("Return to assdose, helpinput = ", tabs_rv$helpinput))
          
        } else{
          removeTab(inputId = "PanelID", target = "Dose Decisions")
          tabs_rv$assdose_flag = 0
        }
        
    if("hemat_tox" %in% input_vals$Func_choice){
          
     # if(is.null(tabs_rv$tox)){
      if(tabs_rv$tox_flag == 0) {
            print("hemat_append_cohort")
            appendTab(inputId = "PanelID", select = TRUE,
                      tab = tabPanel(title = "Toxicity",
                                     # Row 1: Title box --------
                                     fluidRow(
                                       box(solidHeader = TRUE, width = 12,
                                           actionButton(inputId = "close_tox_tab", label = " Close tab", icon = icon("arrow-left"),
                                                        style = "position: absolute; right: 40px"),
                                           actionButton(inputId = "help_c_tox", label = "Help",
                                                        style = "position: absolute; left; background-color: dark grey"),
                                           h4(strong("For toxicity analysis:"), style = "margin-top: 20px;", align = "center"),
                                           h5(strong(HTML("Fill in <u>protocol-based</u> toxicity threshold and recovery blood count values")), align = "center"),
                                           # h5(HTML("<b>Key</b>: 'ANC' = Absolute neutrophil count (x10<sup>6</sup>/L or x10<sup>9</sup>/uL); <br> 
                                           h5(HTML("<b>Key</b>: 'ANC' = Absolute neutrophil count; <br> 
                                                  &emsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp; 'PLT' = Platelet count"), align = "left",style = 'line-height:1.5')
                                       )
                                     ),
                                     # ------
                                     
                                     # Row 2: Blood count thresholds --------
                                     fluidRow(
                                       tags$style(HTML("
                                                    .tooltip > .tooltip-inner {
                                                    background-color: white;
                                                    color: black;
                                                    border: 1px black;
                                                    padding: 5px;
                                                    font-size: 12px;
                                                    margin-top: 5px;
                                                    width: 500px;
                                                    }
                                                    ")),
                                       # Info icons:
                                       tags$head(
                                         tags$style(HTML("
                                                   #dur_anc_help {
                                                   display: inline;
                                                   margin: 5px;
                                                   color: black;
                                                   width: 500px;
                                                   }
                                                   #dur_plt_help {
                                                   display: inline;
                                                   margin: 5px;
                                                   color: black;
                                                   width: 500px;
                                                   }
                                                   #dur_hb_help {
                                                   display: inline;
                                                   margin: 5px;
                                                   color: black;
                                                   width: 500px;
                                                   }
                                                     ")
                                         )
                                       ),
                                       # Box 1: Neutropenia:
                                       box(solidHeader = TRUE, width = 4,
                                           h5(strong("Neutropenia occurs when")),
                                           splitLayout(cellWidths = c('50%', '50%'),
                                                       numericInput(
                                                         inputId = "neut_anc",
                                                         label = "ANC below:",
                                                         value = input_vals$neut_anc,
                                                         width = '100px'),
                                                       
                                                       numericInput(
                                                         inputId = "anc_recov",
                                                         label = "Recovery Threshold",
                                                         value = input_vals$anc_recov,
                                                         width = '100px')),
                                           sliderInput(inputId = "dur_anc",
                                                       label = HTML("Long duration neutropenia",
                                                                    as.character(actionLink(inputId = "dur_anc_help", label = "", 
                                                                                            icon = icon("circle-info")))),
                                                       min = 1, max = 15, value = input_vals$dur_anc, 
                                                       step = 1, width = '380px', post = " week(s)"),
                                           bsTooltip(id = "dur_anc_help", title= "Number of weeks to consider toxicity as \\'prolonged\\' neutropenia", 
                                                     placement = "right", trigger = "hover"),
                                       ),
                                       # Box 2: Thrombocytopenia:
                                       box(solidHeader = TRUE, width = 4,
                                           h5(strong("Thrombocytopenia occurs when")),
                                           splitLayout(cellWidths = c('50%', '50%'),
                                                       numericInput(
                                                         inputId = "thromb_plt",
                                                         label = "PLT below:",
                                                         value = input_vals$thromb_plt,
                                                         width = '100px'),
                                                       
                                                       numericInput(
                                                         inputId = "plt_recov",
                                                         label = "Recovery Threshold",
                                                         value = input_vals$plt_recov,
                                                         width = '100px')),
                                           sliderInput(inputId = "dur_plt",
                                                       label = HTML("Long duration thrombocytopenia", 
                                                                    as.character(actionLink(inputId = "dur_plt_help", label = "", 
                                                                                            icon = icon("circle-info")))),     
                                                       min = 1, max = 15, value = input_vals$dur_plt, 
                                                       step = 1, width = '380px', post = " week(s)"),
                                           bsTooltip(id = "dur_plt_help", title= "Number of weeks to consider toxicity as \\'prolonged\\' thrombocytopenia", 
                                                     placement = "right", trigger = "hover"),
                                       ),
                                       #Box 3: Anaemia:
                                       box(solidHeader = TRUE, width = 4,
                                           h5(strong("Anaemia occurs when ")),
                                           splitLayout(cellWidths = c('50%', '50%'),
                                                       numericInput(
                                                         inputId = "anaem_Hb",
                                                         label = "Hb below:",
                                                         value = input_vals$anaem_Hb,
                                                         width = '100px'),
                                                       
                                                       numericInput(
                                                         inputId = "Hb_recov",
                                                         label = "Recovery Threshold",
                                                         value = input_vals$Hb_recov,
                                                         width = '100px')),
                                           sliderInput(inputId = "dur_hb",
                                                       label = HTML("Long duration anaemia", 
                                                                    as.character(actionLink(inputId = "dur_hb_help", label = "", 
                                                                                            icon = icon("circle-info")))),
                                                       min = 1, max = 15, value = input_vals$dur_hb, 
                                                       step = 1, width = '380px', post = " week(s)"),
                                           bsTooltip(id = "dur_hb_help", title= "Number of weeks to consider toxicity as \\'prolonged\\' anaemia", 
                                                     placement = "right", trigger = "hover"),
                                       )
                                     ),
                                     
                                     # -------
                                     
                                     # Row 3: Submit ------
                                     h2(actionButton(inputId = "go3", label = "Assess Toxicity"), align = "center"),
                                     # --------
                                     
                                     # Output ---------
                                     fluidRow(
                                       box(uiOutput("NeutTox"),width = 4, title = "Neutropenia Analysis", align = "center"),
                                       box(uiOutput("ThrombTox"),width = 4, title = "Thrombocytopenia Analysis",  align = "center"),
                                       box(uiOutput("AnaemTox"),width = 4, title = "Anaemia Analysis",  align = "center")
                                     )
                                     # ------
                                     
                      ))
            tabs_rv$tox_flag = 1
            tabs_rv$tox_num = tabs_n()
            print(paste0("tox tab num = ", tabs_rv$tox_num))
      }
      
          showTab(inputId = "PanelID",  target = "Toxicity", select = TRUE)
          tabs_rv$helpinput = 0
          print(paste0("Return to assdose, helpinput = ", tabs_rv$helpinput))
          
        } else{
          removeTab(inputId = "PanelID", target = "Toxicity")
          tabs_rv$tox_num = 0
        }

      }
    }
      
    }
  })
  
  #Apply tab for compare cohorts (after intervention date)
  observeEvent(input$int_date, {
    output$csSM_apply <- renderUI({
      actionButton(inputId = "cssm_apply", label = "Apply", style = "position: absolute; right: -80px; top: 25px",
      )
    })
  })
  
  #Compare Cohorts:
  observeEvent(input$cssm_apply, priority = 2, {
    
    # input_vals$cohort_IDs <- input$cohort_IDs
    input_vals$Func_choice <- c(input$cMT_Func)
    
    if("Comp_cohorts" %in% input_vals$Func_choice){
      
      if(any(is.null(input$c_unit) | is.na(input$cANC_lower) |is.na(input$cANC_upper) |
             is.na(input$c_DI))){
        alert("Please select all input values.")
        return(NULL)
      } else{
       
        if("Comp_cohorts" %in% input_vals$Func_choice){
          print("CC")
          # print(input$cc_method)
          
          output$CC_Graph <- renderPlotly({
            if (input_vals$c_input_status == "uploaded") {
              print(input_vals$cohort_IDs)
              cID_list <- cohort_IDs()
              
              if(all(file_ext(cID_list$datapath) == "csv")){
                df <- lapply(cID_list$datapath, read.csv)
              }
              
              if(all(file_ext(cID_list$datapath) %in%  c("xls", "xlsx"))){
                df <- lapply(cID_list$datapath, read_excel)
              }
              
              df <- setNames(df, cID_list$name)
              compare_cohortsMT(pat_list = df, 
                                unit = input$c_unit,
                                anc_range = c(input$cANC_lower, input$cANC_upper),
                                intervention_date = input$int_date, 
                                dose_intensity_threshold = input$c_DI)
            }
          })
        } else {
          output$CC_Graph <- NULL
        }
        
        updateTabsetPanel(session, "PanelID", selected = "Plots")
      }
    }
    
  })
  
  ## BOTH 
  
  #Dose decision assessment server function 
  observeEvent(eventExpr = input$go2, handlerExpr = {
    
    # setting/updating input values
    # input_vals$input_df = input$ID
    # input_vals$cohort_IDs = input$cohort_IDs
 
    input_vals$stop_ANC_lower = input$stop_ANC_lower
    print(input$stop_ANC_lower)
    print(input_vals$stop_ANC_lower)
    input_vals$stop_PLT_lower = input$stop_PLT_lower
    input_vals$red_ANC_lower = input$red_ANC_lower
    input_vals$red_ANC_upper = input$red_ANC_upper
    input_vals$red_PLT_lower = input$red_PLT_lower
    input_vals$red_PLT_upper = input$red_PLT_upper
    input_vals$red_factor = input$red_factor
    input_vals$inc_ANC_upper = input$inc_ANC_upper
    input_vals$inc_PLT_upper = input$inc_PLT_upper
    input_vals$inc_factor = input$inc_factor
    input_vals$inc_tld = input$tld
    
    if(is.na(input_vals$stop_ANC_lower) & is.na(input_vals$stop_PLT_lower) &
       is.na(input_vals$red_ANC_lower) & is.na(input_vals$red_ANC_upper) & is.na(input_vals$red_PLT_lower) & is.na(input_vals$red_PLT_upper) & 
       is.na(input_vals$inc_ANC_upper) & is.na(input_vals$inc_PLT_upper)){
      alert("Please select all input values for atleast one section of analysis")
      return(NULL)
    } else{
    
    print("running ass_dose")
    print(input$go2)
    # Ensuring analysis performed for correct patient/patients (in case switch between individual and cohort without refresh)    
    
    if(tabs_rv$current_MT_level == "Individual"){
      
      if(file_ext(input_df()$datapath) == "csv"){
      df <- lapply(input_df()$datapath, read.csv)
      print("df_ready")
      } 
      
      if(file_ext(input_df()$datapath) %in% c("xls", "xlsx")){
        df <- lapply(input_df()$datapath, read_excel)
        # print("path_read")
      }
      
    }
    
    if(tabs_rv$current_MT_level == "Cohort"){
      print(input_vals$cohort_IDs)
      cID_list <- cohort_IDs()
      print("file extensions:")
      print(cohort_IDs()$datapath)
      
      if(all(file_ext(cohort_IDs()$datapath) == "csv")){
      df <- lapply(cID_list$datapath, read.csv)
      print("cohort dfs ready")
      }
      
      if(all(file_ext(cohort_IDs()$datapath) %in% c("xls", "xlsx"))){
        df <- lapply(cID_list$datapath, read_excel)
        print("cohort dfs ready")
      }
      
    } 
    
    if(!is.null(df)){
      
    #   # Running assess dose function
      
      print("function starting")
      a <- assess_doses(pat_list = df, s_anc_threshold = input_vals$stop_ANC_lower, s_plt_threshold = input_vals$stop_PLT_lower,
                        red_anc_range = c(input_vals$red_ANC_lower, input_vals$red_ANC_upper),  red_plt_range = c(input_vals$red_PLT_lower,input_vals$red_PLT_upper),
                        reduction_factor = input_vals$red_factor,
                        inc_anc_threshold = input_vals$inc_ANC_upper, inc_plt_threshold = input_vals$inc_PLT_upper, escalation_factor = input_vals$inc_factor,
                        tolerated_dose_duration = input_vals$inc_tld)
      print("Function has run")
      
      # print(input_vals$inc_ANC_upper)
      # print(input_vals$inc_PLT_upper)
      # print(input_vals$inc_factor)
      # print(input_vals$inc_tld)
      
      # Creating and cleaning up output tables
      if(!is.na(input_vals$stop_ANC_lower) & !is.na(input_vals$stop_PLT_lower)){
      stop_d <- a[[1]] %>%
        htmlTable::addHtmlTableStyle(align = "lc", col.rgroup = c("white", "#e6e6e6"),
                                     css.cell = "width: 2000px", css.header = "background-color: #e6e6e6") %>%
        htmlTable::htmlTable(rnames = FALSE)
      print("Stop applicable")
      }
      
      if(!is.na(input_vals$red_ANC_lower) & !is.na(input_vals$red_ANC_upper) & !is.na(input_vals$red_PLT_lower) & !is.na(input_vals$red_PLT_upper)){
      reduce_d <- a[[2]] %>%
        htmlTable::addHtmlTableStyle(align = "lc", col.rgroup = c("white", "#e6e6e6"),
                                     css.cell = "width: 180px", css.header = "background-color: #e6e6e6") %>%
        htmlTable::htmlTable(rnames = FALSE)
      print("reduce applicable")
      } 
      
      if(!is.na(input_vals$inc_ANC_upper) & !is.na(input_vals$inc_PLT_upper)){
      increase_d <-a[[3]] %>%
        htmlTable::addHtmlTableStyle(align = "lc", col.rgroup = c("white", "#e6e6e6"),
                                     css.cell = "width: 180px", css.header = "background-color: #e6e6e6") %>%
        htmlTable::htmlTable(rnames = FALSE)
      print("increase applicable")
      }
      
    }
    
    if(!is.na(input_vals$stop_ANC_lower) & !is.na(input_vals$stop_PLT_lower)){
      output$StopDose <- renderUI ({stop_d})
    } 
      else if(any(!is.na(input_vals$stop_ANC_lower) | !is.na(input_vals$stop_PLT_lower))){
      alert("Please select all required inputs for this section")
      output$StopDose <- NULL
    }
      
    if(!is.na(input_vals$red_ANC_lower) & !is.na(input_vals$red_ANC_upper) & !is.na(input_vals$red_PLT_lower) &!is.na(input_vals$red_PLT_upper)){
      output$ReduceDose <- renderUI({reduce_d})
    }
      else if(any(!is.na(input_vals$red_ANC_lower) | !is.na(input_vals$red_ANC_upper) | !is.na(input_vals$red_PLT_lower) | !is.na(input_vals$red_PLT_upper))){
      alert("Please select all required inputs for this section")
      output$ReduceDose <- NULL
    }
      
    if(!is.na(input_vals$inc_ANC_upper) & !is.na(input_vals$inc_PLT_upper)){
     output$IncreaseDose <- renderUI({increase_d})
    }
      else if(any(!is.na(input_vals$inc_ANC_upper) | !is.na(input_vals$inc_PLT_upper))){
      alert("Please select all required inputs for this section")
      output$IncreaseDose <- NULL
    }
    
    shinyjs::show("StopDose")
    shinyjs::show("ReduceDose")
    shinyjs::show("IncreaseDose")
    
    }

  })
  
  #Hematological toxicity server function
  observeEvent(eventExpr = input$go3, handlerExpr = {
    
     # Setting/updating input values
       # input_vals$input_df = input$ID
       # input_vals$cohort_IDs = input$cohort_IDs
       
       input_vals$neut_anc = input$neut_anc
       input_vals$anc_recov = input$anc_recov
       input_vals$dur_anc = input$dur_anc
       input_vals$thromb_plt = input$thromb_plt
       input_vals$plt_recov = input$plt_recov
       input_vals$dur_plt = input$dur_plt
       input_vals$anaem_Hb = input$anaem_Hb
       input_vals$Hb_recov = input$Hb_recov
       input_vals$dur_hb = input$dur_hb
       
    # Error if no input values selected
      if(is.na(input_vals$neut_anc) & is.na(input_vals$anc_recov) &
         is.na(input_vals$thromb_plt) & is.na(input_vals$plt_recov) & is.na(input_vals$anaem_Hb) & is.na(input_vals$Hb_recov)){
        alert("Please select all input values for atleast one section of analysis")
        return(NULL)
      } else{
        
      print("ass_hemat")
        
    # Ensuring analysis performed for correct patient/patients (in case switch between individual and cohort without refresh)    

        if(tabs_rv$current_MT_level == "Individual"){
          
          if(file_ext(input_df()$datapath) == "csv"){
            df <- lapply(input_df()$datapath, read.csv)
            print("df_ready")
            print(colnames(df[[1]]))
          } 
          
          if(file_ext(input_df()$datapath) %in% c("xls", "xlsx")){
            df <- lapply(input_df()$datapath, read_excel)
            print(colnames(df[[1]]))
            # print("path_read")
          }
          
        }
        
        if(tabs_rv$current_MT_level == "Cohort"){
          print(input_vals$cohort_IDs)
          cID_list <- cohort_IDs()
          print("file extensions:")
          print(cohort_IDs()$datapath)
          
          if(all(file_ext(cohort_IDs()$datapath) == "csv")){
            df <- lapply(cID_list$datapath, read.csv)
            print("cohort dfs ready")
          }
          
          if(all(file_ext(cohort_IDs()$datapath) %in% c("xls", "xlsx"))){
            df <- lapply(cID_list$datapath, read_excel)
            print("cohort dfs ready")
          }
          
        } 
        

    # Reading patient csv sheets
      if(!is.null(df)){
      
      # Carrying out hematological toxicity function
      print("hemat_tox function starting")
      b <- Hemat_tox(pat_list = df, 
                     anc_range = c(input_vals$neut_anc, input_vals$anc_recov), duration_anc = input_vals$dur_anc, 
                     plt_range = c(input_vals$thromb_plt, input_vals$plt_recov), duration_plt = input_vals$dur_plt,
                     hb_range = c(input_vals$anaem_Hb, input_vals$Hb_recov), duration_hb = input_vals$dur_hb)
      print("hemat tox function has run")
      
      # Creating output - Neutropenia analysis
      if (!is.na(input_vals$neut_anc) & !is.na(input_vals$anc_recov)) {
      neut <- b[[1]] %>%
        htmlTable::addHtmlTableStyle(align = "lc", col.rgroup = c("white", "#e6e6e6"),
                                     css.cell = "width: 2000px", css.header = "background-color: #e6e6e6") %>%
        htmlTable::htmlTable(rnames = FALSE)
      print("Neut applicable")
      }
      
      # Creating output - Thrombocytopenia analysis
      if (!is.na(input_vals$thromb_plt) & !is.na(input_vals$plt_recov)) {
      thromb <- b[[2]] %>%
        htmlTable::addHtmlTableStyle(align = "lc", col.rgroup = c("white", "#e6e6e6"),
                                     css.cell = "width: 180px", css.header = "background-color: #e6e6e6") %>%
        htmlTable::htmlTable(rnames = FALSE)
      print("Thromb applicable")
      }
      
      # Creating output - Anaemia analysis
      if (!is.na(input_vals$anaem_Hb) & !is.na(input_vals$Hb_recov)) {
      anaem <-b[[3]] %>%
        htmlTable::addHtmlTableStyle(align = "lc", col.rgroup = c("white", "#e6e6e6"),
                                     css.cell = "width: 180px", css.header = "background-color: #e6e6e6") %>%
        htmlTable::htmlTable(rnames = FALSE)
      print("Anaem applicable")
      }
      
    }
    
    if(!is.na(input_vals$neut_anc) & !is.na(input_vals$anc_recov)){
    output$NeutTox <- renderUI ({neut})
    }
        else if(any(!is.na(input_vals$neut_anc) | !is.na(input_vals$anc_recov))){
      alert("Please select all neutropenia inputs")
      output$NeutTox <- NULL
    }
        
    if(!is.na(input_vals$thromb_plt) & !is.na(input_vals$plt_recov)){
    output$ThrombTox <- renderUI({thromb})
    }
        else if(any(!is.na(input_vals$thromb_plt) | !is.na(input_vals$plt_recov))){
      alert("Please select all thrombocytopenia inputs")
      output$ThrombTox <- NULL
        }
        
    print(tabs_rv$current_MT_level)
    print(colnames(df[[1]]))
    
    if(any((tabs_rv$current_MT_level == "Individual" & "Hb" %in% colnames(df[[1]])) |
           (tabs_rv$current_MT_level == "Cohort" & "Hb" %in% colnames(df[[1]])))) {
      # print("Level and colnames TRUE")
      # print(input_vals$anaem_Hb, input_vals$Hb_recov)
      
            if(!is.na(input_vals$anaem_Hb) & !is.na(input_vals$Hb_recov)){
            output$AnaemTox <- renderUI({anaem})
            print("anaem output rendered")
            
            } else if(any(!is.na(input_vals$anaem_Hb) | !is.na(input_vals$Hb_recov))){
              alert("Please select all anaemia inputs")
              output$AnaemTox <- NULL
          }
      } else {
        print("Else for level and colnames")
      if(!is.na(input$anaem_Hb) & !is.na(input$Hb_recov)){
      alert ("Ensure Haemoglobin column is labelled 'Hb'")
      output$AnaemTox <- NULL
      }
    }
    
    shinyjs::show("NeutTox")
    shinyjs::show("ThrombTox")
    shinyjs::show("AnaemTox")
    
      # }
    }
  })
  
  #Assess time to dose increase function (with popup)
  observeEvent(eventExpr = input$DoseInc, handlerExpr = {
 
    # input_vals$input_df <- input$ID
    # input_vals$cohort_IDs <- input$cohort_IDs
    input_vals$inc_factor <- input$inc_factor
    
    if(tabs_rv$current_MT_level == "Individual"){
      print("Assess dose Inc ind")
      print(input$DoseInc)
      
      if((input_vals$inc_factor <= 0)){
        alert("Please select dose escalation factor > 0%")
        return(NULL)
      } else{
      
      print("in")
      ID_list <- input_df()
      print(ID_list)
      
      if(file_ext(ID_list$datapath) == "csv"){
      df <- read.csv(ID_list$datapath)
      }
      
      if(file_ext(ID_list$datapath) %in% c("xls", "xlsx")){
        df <- read_excel(ID_list$datapath)
      }
      # print(df)
      
      print(input$inc_factor)
      print(df$MP[1])
      print(df$MP[1]*(input$inc_factor/100)+df$MP[1])
      
      ind_inc <- df %>% 
        dplyr::filter(MP>= (df$MP[1]*(input$inc_factor/100)+df$MP[1]))
      print(ind_inc)
      ind_inc_n <- ind_inc$Weeks[1]
      print(ind_inc$Weeks[1])
      
      if(!is.na(ind_inc_n)){
        output$DoseInc_text <- renderText(paste0("Time to first attempted dose increase = ", ind_inc_n, " weeks"))
      }
      if(is.na(ind_inc_n)){
        output$DoseInc_text <- renderText(paste0("Dose was not increased above selected escalation factor"))
      }

      showModal(modalDialog(
        title = paste0("Time to first attempted dose increase for ", file_path_sans_ext(input_df()$name)),
        textOutput("DoseInc_text"),
        size = "l",
        footer = actionButton("close_popup", "Close"),
        easyClose = TRUE) 
      )
      
      }
    }

    if(tabs_rv$current_MT_level == "Cohort"){
      print("Assess dose Inc cohort")
      print(paste0("input$DoseInc = ", input$DoseInc))

      if(input_vals$inc_factor <= 0){
        alert("Please select escalation factor > 0%")
        return(NULL)
      } else{

        print("in")
        cID_list <- cohort_IDs()
        print(cID_list)
        if(all(file_ext(cID_list$datapath) == "csv")){
        df <- lapply(cID_list$datapath, read.csv)
        print(df)
        }
        
        if(all(file_ext(cID_list$datapath) %in% c("xls", "xlsx"))){
          df <- lapply(cID_list$datapath, read_excel)
          print(df)
        }
        
        p <- time_to_doseinc(pat_list = df, escalation_factor = input$inc_factor)
        
        print("a ready")
        
        plot <- p[[1]]
        print(plot)
        text <- p[[2]]
        print(text)
        med <- as.numeric(median(text))
        print(med)
        LCL <- as.numeric(summary(text)$table["0.95LCL"])
        print(LCL)
        UCL <- as.numeric(summary(text)$table["0.95UCL"])
        print(UCL)
        
        output$DoseInc_Graph <- renderPlot(expr = {
        plot
        print(plot)
        print("Dose Inc output ready")
      })
       # print("Text starting")
       
        if(!is.na(med)){
        output$DoseInc_text <- renderText(
          paste0("Median time to first dose increase = ", med,"[", LCL, " - ", UCL,  "] weeks"))
        }
        if(is.na(med)){
          output$DoseInc_text <- renderText(
            paste0("Median time to first dose increased was not reached"))
        }

      showModal(modalDialog(
        title = "Time to first attempted dose increase for selected cohort",
        withSpinner(plotOutput("DoseInc_Graph")),
        textOutput("DoseInc_text"),
        size = "l",
        footer = actionButton("close_popup", "Close"),
        easyClose = TRUE) 
      )
      }
    }

  })
  
  #Close dose increase popup modal
  observeEvent(eventExpr = input$close_popup, {
    removeModal()
  })
  
  #Close Plots tab
  observeEvent(eventExpr = input$close_plots_tab, handlerExpr = {
    
    removeTab("PanelID", target = "Plots")
    tabs_rv$plots_flag = 0
    updateTabsetPanel(inputId = "PanelID", selected = tabs_rv$previous_tab)
    
    #Changing other tab numbers as required
    if(tabs_rv$help_num > tabs_rv$plots_num){
      tabs_rv$help_num = tabs_rv$help_num-1
      print(paste0("new help num = ", tabs_rv$help_num))
    }
    if(tabs_rv$pat_num > tabs_rv$plots_num){
      tabs_rv$pat_num = tabs_rv$pat_num-1
      print(paste0("new pat num = ", tabs_rv$pat_num))
    }
    if(tabs_rv$tox_num > tabs_rv$plots_num){
      tabs_rv$tox_num = tabs_rv$tox_num-1
      print(paste0("new tox num = ", tabs_rv$tox_num))
    }
    if(tabs_rv$assdose_num > tabs_rv$plots_num){
      tabs_rv$assdose_num = tabs_rv$assdose_num-1
      print(paste0("new ass dose num = ", tabs_rv$assdose_num))
    }
    
    
    tabs_rv$plots_num = 0
    
    output$LG_Graph <- NULL
    output$Cyc_Graph <- NULL
    output$SM_Graph <- NULL
    output$CC_Graph <- NULL
    
  })

  #Close Ass dose tab
  observeEvent(eventExpr = input$close_ass_dose_tab, handlerExpr = {

    removeTab("PanelID", target = "Dose Decisions")
    tabs_rv$assdose_flag = 0
    updateTabsetPanel(inputId = "PanelID", selected = tabs_rv$previous_tab)

    #Changing other tab numbers as required
    if(tabs_rv$help_num > tabs_rv$assdose_num){
      tabs_rv$help_num = tabs_rv$help_num-1
      print(paste0("new help num = ", tabs_rv$help_num))
    }
    if(tabs_rv$pat_num > tabs_rv$assdose_num){
      tabs_rv$pat_num = tabs_rv$pat_num-1
      print(paste0("new pat num = ", tabs_rv$pat_num))
    }
    if(tabs_rv$tox_num > tabs_rv$assdose_num){
      tabs_rv$tox_num = tabs_rv$tox_num-1
      print(paste0("new tox num = ", tabs_rv$tox_num))
    }
    if(tabs_rv$plots_num > tabs_rv$assdose_num){
      tabs_rv$plots_num = tabs_rv$plots_num-1
      print(paste0("new plots num = ", tabs_rv$plots_num))
    }

    tabs_rv$assdose_num = 0

   output$StopDose <- NULL
   output$ReduceDose <- NULL
   output$IncreaseDose <- NULL
    
  })
  
  #Close hemat tox tab
  observeEvent(eventExpr = input$close_tox_tab, handlerExpr = {
    
    # tabs_rv$tox = input$go3
    removeTab("PanelID", target = "Toxicity")
    tabs_rv$tox_flag = 0
    updateTabsetPanel(inputId = "PanelID", selected = tabs_rv$previous_tab)
    
    #Changing other tab numbers as required
    if(tabs_rv$pat_num > tabs_rv$tox_num){
      tabs_rv$pat_num = tabs_rv$pat_num-1
      print(paste0("new pat num = ", tabs_rv$pat_num))
    }
    if(tabs_rv$assdose_num > tabs_rv$tox_num){
      tabs_rv$assdose_num = tabs_rv$assdose_num-1
      print(paste0("new ass dose num = ", tabs_rv$assdose_num))
    }
    if(tabs_rv$help_num > tabs_rv$tox_num){
      tabs_rv$help_num = tabs_rv$help_num-1
      print(paste0("new help num = ", tabs_rv$help_num))
    }
    if(tabs_rv$plots_num > tabs_rv$tox_num){
      tabs_rv$plots_num = tabs_rv$plots_num-1
      print(paste0("new plots num = ", tabs_rv$plots_num))
    }
    
    tabs_rv$tox_num = 0
    
    output$NeutTox <- NULL
    output$ThrombTox <- NULL
    output$AnaemTox <- NULL
  })
  
  # Help tab
  observeEvent(eventExpr = c(input$help_start, input$help_go, input$help_ind_assdose, input$help_ind_tox, 
                             input$help_go1, input$help_c_assdose, input$help_c_tox), handlerExpr = {
    
    print(paste0("tabs_rv$help_flag = ", tabs_rv$help_flag))
    print(c(input$help_start, input$help_go, input$help_ind_assdose, input$help_ind_tox, 
             input$help_go1, input$help_c_assdose, input$help_c_tox))

    #Ensuring everything happens only after a help button is selected on the page of interest
    if(tabs_rv$current_tab == "Get Started"){
      print("Get started for help")
      if(input$help_start > 0){
        print(paste0("help start = ", input$help_start))
        
        if(input$help_start > tabs_rv$help_start){
          tabs_rv$help_start = input$help_start
          print(paste0("tabs_rv$help_start = ", tabs_rv$help_start))
          tabs_rv$helpinput = 1
          
        } else{tabs_rv$helpinput = 0}
      }  else{tabs_rv$helpinput = 0}
      print(paste0("start_help help input = ", tabs_rv$helpinput))
    }
    
    print("issue?")
    
    if(tabs_rv$current_MT_level == "Individual" & tabs_rv$current_tab == "Plots"){
      print("individual+Plots for help")
      if(input$help_go > 0){
        print(paste0("help go = ", input$help_go))
        
        if(input$help_go > tabs_rv$help_go){
          tabs_rv$help_go = input$help_go
          print(paste0("tabs_rv$help_go = ", tabs_rv$help_go))
          tabs_rv$helpinput = 1
          
      } else{tabs_rv$helpinput = 0}
      }  else{tabs_rv$helpinput = 0}
      print(paste0("plots_help help input = ", tabs_rv$helpinput))
    }

    if(tabs_rv$current_MT_level == "Individual" & tabs_rv$current_tab == "Dose Decisions"){
      print("individual+dose decisions for help")
      if(input$help_ind_assdose > 0){
          print(input$help_ind_assdose)
          
          if(input$help_ind_assdose > tabs_rv$help_ind_assdose){
            tabs_rv$help_ind_assdose = input$help_ind_assdose
            print(paste0("tabs_rv$help_ind_assdose = ", tabs_rv$help_ind_assdose))
            tabs_rv$helpinput = 1
            
      } else{tabs_rv$helpinput = 0}
      } else{tabs_rv$helpinput = 0}
    }
                               
    if(tabs_rv$current_MT_level == "Individual" & tabs_rv$current_tab == "Toxicity"){
      print("individual+toxicity for help")
      if(input$help_ind_tox > 0){
        print(input$help_ind_tox)
        
        if(input$help_ind_tox > tabs_rv$help_ind_tox){
          tabs_rv$help_ind_tox = input$help_ind_tox
          print(paste0("tabs_rv$help_ind_tox = ", tabs_rv$help_ind_tox))
          tabs_rv$helpinput = 1
          
        } else{tabs_rv$helpinput = 0}
      } else{tabs_rv$helpinput = 0}
    }
    
    if(tabs_rv$current_MT_level == "Cohort" & tabs_rv$current_tab == "Plots"){
      print("Cohort+Plots for help")
      if(input$help_go1 > 0){
        print(input$help_go1)
        
        if(input$help_go1 > tabs_rv$help_go1){
          tabs_rv$help_go1 = input$help_go1
          print(paste0("tabs_rv$help_go1 = ", tabs_rv$help_go1))
          tabs_rv$helpinput = 1
          
        } else{tabs_rv$helpinput = 0}
      }else{tabs_rv$helpinput = 0}
      # print(paste0("helpinput =", tabs_rv$helpinput))
    }
    
    if(tabs_rv$current_MT_level == "Cohort" & tabs_rv$current_tab == "Dose Decisions"){
      print("Cohort+Dose Decisions for help")
      if(input$help_c_assdose > 0){
        print(input$help_c_assdose)
        
        if(input$help_c_assdose > tabs_rv$help_c_assdose){
        tabs_rv$help_c_assdose = input$help_c_assdose
        print(paste0("tabs_rv$help_c_assdose = ", tabs_rv$help_c_assdose))
        tabs_rv$helpinput = 1
        
        }else {tabs_rv$helpinput = 0}
        } else {tabs_rv$helpinput = 0}
      
      print(paste0("helpinput =", tabs_rv$helpinput))
    }
    
    if(tabs_rv$current_MT_level == "Cohort" & tabs_rv$current_tab == "Toxicity"){
      print("Cohort+Toxicity for help")
      if(input$help_c_tox > 0){
        print(input$help_c_tox)
        
        if(input$help_c_tox > tabs_rv$help_c_tox){
          tabs_rv$help_c_tox = input$help_c_tox
          print(paste0("tabs_rv$help_c_tox = ", tabs_rv$help_c_tox))
          tabs_rv$helpinput = 1
          
        } else{tabs_rv$helpinput = 0}
      } else{tabs_rv$helpinput = 0}
    }
    
    if(tabs_rv$helpinput == 1){
    
    # Creating ui for pdf file:
    output$help_pdf <- renderUI({
      tags$iframe(style="height:800px; width:100%", src="App_allMT_Reference-Manual_v4_30-05-23.pdf")  # src needs to be in a folder called "www"
    })

    print(tabs_rv$current_tab)
    print(paste0("Inside help input tabs_rv$help_flag = ", tabs_rv$help_flag))
    
    #Disabling help buttons if help page is open
    if(tabs_rv$help_flag == 1){
      
      disable("help_start")
      disable("help_go")
      disable("help_ind_assdose")
      disable("help_ind_tox")
      disable("help_go1")
      disable("help_c_assdose")
      disable("help_c_tox")
    }
    
    # Appending tab only if help page not already open and not currently in help page
    if(all(tabs_rv$help_flag != 1 & tabs_rv$current_tab != "Help")){

    appendTab(inputId = "PanelID", select = TRUE, 
              tab = tabPanel(title = "Help", 
                             h4("Help Section"),
                             actionButton(inputId = "close_help_tab", label = "close tab", icon = icon("arrow-left"),
                                          # style = "position: absolute;  right;"),
                                          style = "position: absolute; top: 110px; right:80px"),
                             uiOutput("help_pdf")
              )
    )
      
    tabs_rv$help_flag = 1
    tabs_rv$help_num = tabs_n()
    
    print(paste0("Appened_help tabs_rv$help_num = ", tabs_rv$help_num))
    
      shinyjs::hide("help_start")
      shinyjs::hide("help_go")
      shinyjs::hide("help_ind_assdose")
      shinyjs::hide("help_ind_tox")
      shinyjs::hide("help_go1")
      shinyjs::hide("help_c_assdose")
      shinyjs::hide("help_c_tox")
    }
    
    }
  })

  #Close help tab
  observeEvent(eventExpr = input$close_help_tab, handlerExpr = {

    
    removeTab("PanelID", target = "Help")
    tabs_rv$help_flag = 0
    updateTabsetPanel(inputId = "PanelID", selected = tabs_rv$previous_tab)
    
    tabs_rv$helpinput = 0
    
    message("Changing other tab numbers as required")
    
    if(tabs_rv$pat_num > tabs_rv$help_num){
      tabs_rv$pat_num = tabs_rv$pat_num-1
      print(paste0("new pat num = ", tabs_rv$pat_num))
    }
    if(tabs_rv$plots_num > tabs_rv$help_num){
      tabs_rv$plots_num = tabs_rv$plots_num-1
      print(paste0("new plots num = ", tabs_rv$plots_num))
    }
    if(tabs_rv$assdose_num > tabs_rv$help_num){
      tabs_rv$assdose_num = tabs_rv$assdose_num-1
      print(paste0("new ass dose num = ", tabs_rv$assdose_num))
    }
    if(tabs_rv$tox_num > tabs_rv$help_num){
      tabs_rv$tox_num = tabs_rv$tox_num-1
      print(paste0("new tox num = ", tabs_rv$tox_num))
    }
    
    tabs_rv$help_num = 0
    
    shinyjs::show("help_start")
    shinyjs::show("help_go")
    shinyjs::show("help_ind_assdose")
    shinyjs::show("help_ind_tox")
    shinyjs::show("help_go1")
    shinyjs::show("help_c_assdose")
    shinyjs::show("help_c_tox")
    
    enable("help_start")
    enable("help_go")
    enable("help_ind_assdose")
    enable("help_ind_tox")
    enable("help_go1")
    enable("help_c_assdose")
    enable("help_c_tox")
    
    print(paste0("tabs_rv$help_flag = ", tabs_rv$help_flag))
    
  })
  
  #Refresh individual
  observeEvent(input$refresh1, {
    print("refresh")

    shinyjs::reset("ID") # only clears what is shown in the ui. Does not remove/clear the input stored. 
    input_vals$input_df <- NULL # removes stored input that is used in all functions
    print(input_vals$input_df)
    input_vals$input_status = "reset"
    
    shinyjs::reset("MT_Func")
    input_vals$function_list <- NULL
    input$unit ==  "million"
    shinyjs::reset("LG_anim")
    output$LG_Graph <- NULL
    output$Cyc_Graph <- NULL
    output$StopDose <- NULL
    output$ReduceDose <- NULL
    output$IncreaseDose <- NULL
    output$pat_see <- NULL
    output$NeutTox <- NULL
    output$ThrombTox <- NULL
    output$AnaemTox <- NULL
    output$anim_button <- NULL
    output$plot1 <- NULL
    
    # resetting tabs:
    removeTab(inputId = "PanelID", target = "Plots")
    tabs_rv$plots_flag = 0
    tabs_rv$plots_num = 0
    removeTab(inputId = "PanelID", target = "Dose Decisions")
    tabs_rv$assdose_flag = 0
    tabs_rv$assdose_num = 0
    removeTab(inputId = "PanelID", target = "Patient Data")
    tabs_rv$patdata_flag = 0
    tabs_rv$pat_num = 0
    removeTab(inputId = "PanelID", target = "Toxicity")
    tabs_rv$tox_flag = 0
    tabs_rv$tox_num = 0
    removeTab(inputId = "PanelID", target = "Help")
    tabs_rv$help_flag = 0
    tabs_rv$help_num = 0
    tabs_rv$helpinput = NULL
    
  
  })

  #Refresh cohort
  observeEvent(input$refresh2, {
    print("refresh2")
    
    shinyjs::reset("cohort_IDs")
    input_vals$cohort_IDs <- NULL
    print(input_vals$cohort_IDs)
    input_vals$c_input_status == "reset"
    
    shinyjs::reset("cMT_Func")
    input_vals$Func_choice <- NULL
    input$c_unit == "million"
    # shinyjs::reset("cANC_lower")
    # shinyjs::reset("cANC_upper")
    # shinyjs::reset("c_DI")
    output$SM_Graph <- NULL
    output$CC_Graph <- NULL
    output$StopDose <- NULL
    output$ReduceDose <- NULL
    output$IncreaseDose <- NULL
    output$NeutTox <- NULL
    output$ThrombTox <- NULL
    output$AnaemTox <- NULL
    output$anim_button <- NULL
    output$plot1 <- NULL
    output$cohort_warning <- NULL
    
    # resetting tabs:
    
    removeTab(inputId = "PanelID", target = "Plots")
    tabs_rv$plots_flag = 0
    tabs_rv$plots_num = 0
    removeTab(inputId = "PanelID", target = "Dose Decisions")
    tabs_rv$assdose_flag = 0
    tabs_rv$assdose_num = 0
    removeTab(inputId = "PanelID", target = "Patient Data")
    tabs_rv$patdata_flag = 0
    tabs_rv$pat_num = 0
    removeTab(inputId = "PanelID", target = "Toxicity")
    tabs_rv$tox_flag = 0
    tabs_rv$tox_num = 0
    removeTab(inputId = "PanelID", target = "Help")
    tabs_rv$help_flag = 0
    tabs_rv$help_num = 0
    tabs_rv$helpinput = NULL
  })

}





