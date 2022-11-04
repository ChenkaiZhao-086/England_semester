
# get Rt value ------------------------------------------------------------
crawlOne <- function(location,
                     prefix = "/Users/chenkaizhao/Documents/600_Project/620_England semester/data/raw/Rt_dat/LTLA_public/"){ # https://raw.githubusercontent.com/ImperialCollegeLondon/covid19local/master/LTLA_public/
  webpage <- read_html(paste(prefix, location, sep = ""))
  body_nodes <- webpage %>% 
    html_node("#reproduction-number") %>%
    html_children()
  raw.data <- fromJSON(html_text(body_nodes[4]))
  data.length <- (length(raw.data$x$data$x[[1]])-1)/2
  res.data <- data.frame(
    x = raw.data$x$data$x[[1]][-(data.length*2+1)],
    bound = rep(c("lower", "upper"), each = data.length),
    ci.90 = raw.data$x$data$y[[1]][-(data.length*2+1)],
    ci.60 = raw.data$x$data$y[[2]][-(data.length*2+1)],
    ci.30 = raw.data$x$data$y[[3]][-c(data.length*2+2, data.length+1)],
    x.upper = raw.data$x$data$x[[5]][1]
  )
  res.data <- res.data[res.data$x <= res.data$x.upper,]
  res.data <- res.data %>% pivot_wider(names_from = bound, values_from = c(ci.90, ci.60, ci.30))
  res.data$Imperial <- location
  message(location)
  return(res.data)
}

### sample Rt
reSample5 <- function(A,B,C,D,E){
  return(sample(size = 1, x = c(A, B, C, C, D, E)))
}

### calculate CI
CI.cal <- function(vector, 
                   digits = 2) {
  basic <- data.frame(date = vector[1,"date"], Imperial = vector[1,"Imperial"])
  CI <- format(epi.conf(vector$sample.R, ctype = "mean.single"),digits = digits,nsmall = digits)
  result <- cbind(basic, CI)
  return(result)
}


# Save map ----------------------------------------------------------------
x11.save <- function(fig = fig,
                 file = file,
                 width = w,
                 height = h) {
  plot(fig)
  dev.copy2pdf(file = file, width = w, height = h, out.type = "pdf")
  dev.off
}

# For match LTLA ----------------------------------------------------------

prep.psm.dat <- function(data) {
  psm_dat1 <- get_matches(data) %>% 
    as.data.frame() %>% 
    arrange(subclass)
  psm_name <- psm_dat1 %>% 
    distinct(., subclass, .keep_all = T) %>% 
    transmute(psm_name1 = paste(subclass, LTLA_ID, LTLA_name, sep = "_")) %>%
    as.data.frame()
  psm_name <- psm_name$psm_name1
  
  psm_dat <- psm_dat1 %>% 
    dplyr::select(subclass, LTLA_ID, LTLA_name, type) %>% 
    group_by(subclass)
  psm_dat <- setNames(split(psm_dat, psm_dat1$subclass), psm_name)
  
  return(psm_dat)
}


m.match <- function(m_mat) {
  
  case_data <- m_mat %>% filter(type == 1) %>% mutate(index1 = NA, index2 = NA, index3 = NA)
  control_data <- m_mat %>% filter(type == 0)
  
  for (i in 1:length(case_data$type)) {
    mat <- which(between(control_data$Pop_Den_km2, case_data$Pop_Den_km2[i]*0.8, case_data$Pop_Den_km2[i]*1.2) &
                   between(control_data$Prosperity, case_data$Prosperity[i]*0.8, case_data$Prosperity[i]*1.2) &
                   control_data$N_num == case_data$N_num[i])
    mat1 <- m_mat[mat, "LTLA_ID"] %>% unlist() %>% unname()
    if (length(mat1) > 0) {
      #set.seed(1)
      case_data$index1[i] <- as.numeric(sample(mat1, 3, replace = T)[1])
      #set.seed(1)
      case_data$index2[i] <- as.numeric(sample(mat1, 3, replace = T)[2])
      #set.seed(1)
      case_data$index3[i] <- as.numeric(sample(mat1, 3, replace = T)[3])
    } else{
      case_data$index1[i] <- 999
    }
  }
  
  con_name <- LTLA_dat_combine %>% 
    mutate(c.LTLA_ID = LTLA_ID, c.LTLA_name = LTLA_name) %>% 
    dplyr::select(c.LTLA_ID, c.LTLA_name)
  
  
  matched <- case_data %>% 
    filter(index1 != 999) %>% 
    mutate(subclass = 1:length(LTLA_ID), c.type = 0, c.LTLA_ID = index1,
           k.LTLA_ID = LTLA_ID, k.LTLA_name = LTLA_name, k.type = type) %>% 
    left_join(., con_name) %>% 
    dplyr::select(subclass, k.LTLA_ID, k.LTLA_name, k.type, c.LTLA_ID, c.LTLA_name, c.type) %>% 
    as.data.frame() %>% 
    reshape(., direction = "long", 
            varying = list(LTLA_ID = c(2,5), name = c(3,6), type = c(4,7)),
            v.names = c("LTLA_ID", "LTLA_name", "type"),
            idvar = "subclass") %>% 
    dplyr::select(! time) %>% 
    arrange(subclass, desc(type)) 
  
  m_name <- matched %>% 
    distinct(., subclass, .keep_all = T) %>% 
    transmute(matched = paste(subclass, LTLA_ID, LTLA_name, sep = "_")) %>%
    as.data.frame() %>% .$matched
  
  matched <- setNames(split(matched, matched$subclass), m_name)
  return(matched)
}


case.cal <- function(start_date = "2020-06-01",
                     end_date = "2020-12-01", 
                     type = c("MA", "RS"),
                     data) {
  if (type == "MA") {
    dat <- data %>% 
      group_by(LTLA_ID) %>% 
      arrange(LTLA_ID, date) %>% 
      mutate(ma = rollmean(newCasesBySpecimenDate, 7, align = "right", fill = NA),
             newCasesBySpecimenDate = ma) %>% 
      dplyr::select(areaCode, LTLA_ID, LTLA_name, date, newCasesBySpecimenDate) %>% 
      drop_na() %>% 
      filter(date >= as.Date(start_date) & date <= as.Date(end_date)) %>% 
      ungroup()
    
    return(dat)
  } else{
    dat <- data %>% 
      group_by(LTLA_ID) %>% 
      arrange(LTLA_ID, date) %>% 
      mutate(rs = rollsum(newCasesBySpecimenDate, 7, align = "right", fill = NA),
             newCasesBySpecimenDate = rs)%>% 
      dplyr::select(areaCode, LTLA_ID, LTLA_name, date, newCasesBySpecimenDate) %>% 
      drop_na() %>% 
      filter(date >= as.Date(start_date) & date <= as.Date(end_date)) %>% 
      ungroup()
    
    return(dat)
  }
} 


# Do causal impact analysis -----------------------------------------------
get.info <- function(data = data, # this parameter need to be a indexed list, like thisform: a[[1]]
                     open_data = NULL,
                     type = c("case", "control", "open")) {
  if (is.numeric(data)) {
    open_list <- open_data[open_data$LTLA_ID == data, "HEI_opening"]
    open <- open_list %>% 
      distinct() %>% 
      arrange(HEI_opening) %>% 
      as.data.frame()
    return(as.Date(open[1,1]))
    print("As the data  is numeric, the returned value is the opening time")
  } else if (type == "case") {
    if (data[["type"]][1] == 1) {
      case <- data[["LTLA_ID"]][1]
      return(case)
    } else{
      stop("!!! Check the CASE NUMBER !!!")
    }
  } else if (type == "control") {
    if (data[["type"]][2] == 0) {
      control <- data[["LTLA_ID"]][2]
      return(control)
    } else{
      stop("!!! Check the CONTROL NUMBER !!!")
    }
  } else if (type == "open") {
    if (data[["type"]][1] == 1) {
      case <- data[["LTLA_ID"]][1]
      if (is.null(open_data)) {
        stop("!!! Inpute the open_data dataset !!!")
      }
    } else{
      stop("!!! Check the CASE NUMBER !!!")
    }
    open_list <- open_data[open_data$LTLA_ID == case, "HEI_opening"]
    open <- open_list %>% 
      distinct() %>% 
      arrange(HEI_opening) %>% 
      as.data.frame()
    return(as.Date(open[1,1]))
  }
}


prep.Causal <- function(data = data,
                        case = case, # only one
                        control = control # could be a vactor
                        ) {
  leng <- length(control)
  
  case_dat <- data %>% filter(LTLA_ID == case) %>% dplyr::select(date, LTLA_name, newCasesBySpecimenDate) %>% 
    summarise(Date = date, LTLA_name = LTLA_name, y = newCasesBySpecimenDate) %>% 
    arrange(Date)
  control_dat <- vector("list", length = leng)
  for (i in 1:leng) {
    control_dat[[i]] <- data %>% filter(LTLA_ID == control[i]) %>% dplyr::select(date, LTLA_name, newCasesBySpecimenDate) %>% 
      arrange(date)
  }
  
  control_dat <- as.data.frame(control_dat)
  
  if (leng == 1) {
    casual_data <- cbind(case_dat, control_dat)
    if (all(casual_data[,1] == casual_data[,4])) { # check case and control
      casual_data <- casual_data %>% dplyr::select(Date, y, contains("newCasesBy"))
      return(arrange(casual_data, Date))
    }else{
      warning("!!! Check case and control date !!!")
    }
  } else{
    if (all(control_dat[,1] == control_dat[,4])) { # check case and control  # all 代表全部比较
      casual_data <- cbind(case_dat, control_dat)
      if (all(casual_data[,1] == casual_data[,4])) { # check case and control
        casual_data <- casual_data %>% dplyr::select(Date, y, contains("newCasesBy"))
        return(arrange(casual_data, Date))
      }else{
        warning("!!! Check case and control date !!!")
      }
    }else{
      warning("!!! Check control date !!!")
    }
  }
}


do.Causal <- function(start_date = NULL,
                      intervention_date = NULL,
                      end_date = NULL,
                      data = data,
                      seed = 1,
                      show.fig = T,
                      get.table = F,
                      save.fig = F,
                      path = NULL,
                      ...) {
  if (is.POSIXct(data[1,1]) | is.Date(data[1,1])) {
    start_date <- data[1,1]
    end_date <- data[length(data[,1]), 1]
    x.date <- as.Date(data[,1])
    data <- zoo(data[,-1], x.date)
  } else {
    time.points <- seq.Date(from = as.Date(start_date), 
                            to = as.Date(end_date), by = 1)
    data <- zoo(data, time.points)
  }
  
  pre.period <- c(as.Date(start_date), as.Date(intervention_date))
  post.period <- c(as.Date(intervention_date)+1, as.Date(end_date))
  
  set.seed(seed)
  impact <- CausalImpact(data, pre.period, post.period, ...)
  #show(summary(impact))
  
  if (show.fig == T) {
    show(plot(impact))
  }
  
  if (save.fig == T) {
    ggsave(plot(impact), filename = path)
  }
  
  if (get.table == T) {
    table <- data.frame(abs_eff = round(impact[["summary"]][["AbsEffect"]][1], 2),
                     abs_lci = round(impact[["summary"]][["AbsEffect.lower"]][1], 2),
                     abs_uci = round(impact[["summary"]][["AbsEffect.upper"]][1], 2),
                     abs_sd = round(impact[["summary"]][["AbsEffect.sd"]][1], 2),
                     relative_eff = round(impact[["summary"]][["RelEffect"]][1]*100, 2),
                     rel_lci = round(impact[["summary"]][["RelEffect.lower"]][1]*100, 2),
                     rel_uci = round(impact[["summary"]][["RelEffect.upper"]][1]*100, 2),
                     rel_sd = round(impact[["summary"]][["RelEffect.sd"]][1]*100, 2))
    return(table)
  } else {
    return(impact)
  }
} 



# Auto correlation --------------------------------------------------------

get.decay.par <- function(sim.data) {
  par <- nls(y ~ SSasymp(t, yf, y0, log_alpha), data = sim.data) %>% 
    tidy() %>% 
    dplyr::select(term, estimate) %>% 
    spread(term, estimate) %>% 
    mutate(alpha = exp(log_alpha))
  return(par)
}

gen.decay <- function(decay.period,
                      decay.fun,
                      incubation) {
  x <- 1:decay.period
  y <- eval(parse(text = decay.fun))[1:(decay.period-2)]
  inc.time <- rep(0,incubation)
  time1 <- c(inc.time, 1, y, 0)
  return(time1)
}

get.index <- function(dat,
                      decay) {
  h.indes_matrix <- cbind(
    expand.grid(h.index = dat$h_index,
                weight = decay),
    expand.grid(h.index_c = c(1:length(dat$h_index)), 
                weight_c = 1:length(decay)))
  h.indes_matrix$value <- h.indes_matrix$h.index * h.indes_matrix$weight
  h.indes_matrix$day <- h.indes_matrix$h.index_c + h.indes_matrix$weight_c - 1
  
  h_dat <- dat %>% 
    dplyr::select(!contains("h_index"))
  
  h_index_dat <- h.indes_matrix %>% 
    group_by(day) %>% 
    filter(day <= length(dat$h_index)) %>% 
    summarise(h_index = sum(value))
  
  result <- cbind(h_dat, h_index_dat[,2])
  return(result)
}
