
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
                 width = width,
                 height = height) {
  plot(fig)
  dev.copy2pdf(file = file, width = width, height = height, out.type = "pdf")
  dev.off()
} # When change default graphic device from Quartz to Cairo, we can save plot by gg.save. It's seems we do not need this function anymore. By the way, open a separate Cairo window is much faster than the bottom right on mac

# For match LTLA ----------------------------------------------------------
# Calculate moving average OR rolling sum case number
case.cal <- function(start_date = "2020-06-01",
                     end_date = "2020-12-01", 
                     type = c("MA", "RS", "week"),
                     data) {
  switch (type,
          "MA" = {
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
          },
          "RS" = {
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
          },
          "week" = {
            data %>% 
              mutate(week = isoweek(date)) %>% 
              group_by(LTLA_ID, week) %>% 
              arrange(LTLA_ID, date) %>% 
              mutate(newCasesBySpecimenDate = sum(newCasesBySpecimenDate)) %>% 
              dplyr::select(areaCode, LTLA_ID, LTLA_name, date, newCasesBySpecimenDate) %>% 
              drop_na() %>% 
              filter(date >= as.Date(start_date) & date <= as.Date(end_date)) %>% 
              ungroup()
          }
  )
} 



m.match <- function(m_mat, prop = 0.2, control_num = 3) { # control_num is the max number need to match
  
  ## Separate dataset to case dataset and control dataset
  m_mat <- m_mat %>% dplyr::select(LTLA_ID, LTLA_name, type, Pop_Den_km2, Prosperity, N_num)
  case_data <- m_mat %>% filter(type == 1) %>% mutate(all.mat = NA, sampled = NA, index1 = NA)
  control_data <- m_mat %>% filter(type == 0)
  
  for (i in 1:length(case_data$type)) {
    mated_line <- which(between(control_data$Pop_Den_km2, case_data$Pop_Den_km2[i]*(1-0.2), case_data$Pop_Den_km2[i]*(1+0.2)) &
                          between(control_data$Prosperity, case_data$Prosperity[i]*(1-0.2), case_data$Prosperity[i]*(1+0.2)) &
                          control_data$N_num == case_data$N_num[i])
    if (length(mated_line) > 0) {
      n.mat <- length(mated_line)
      if (control_num == 1) {
        if (n.mat == 1) {
          case_data[i, "index1"] <- control_data[mated_line, "LTLA_ID"] %>% unlist() %>% unname()
          case_data[i,"all.mat"] <- as.character(mated_line) # 所有符合匹配规则的行号
          case_data[i,"sampled"] <- as.character(mated_line) # 在符合匹配规则的行中根据对照数量随机抽取到的行号
          next # 这个很重要，不加next的话，在只匹配一个的时候会再走一遍下面的 if (n.mat == 1) 
        } else {
          sampler <- as.numeric(sample(mated_line, 1, replace = F))
          sampler_ID <- control_data[sampler, "LTLA_ID"] %>% unlist() %>% unname()
          case_data[i, "index1"] <- sampler_ID
          case_data[i,"all.mat"] <- str_flatten_comma(mated_line) # Label all the sample results
          case_data[i,"sampled"] <- as.character(sampler)
          next
        }
      } else if (is.na(control_num)) {
        sampler <- as.numeric(sample(mated_line, n.mat, replace = F))
        sampler_ID <- control_data[sampler, "LTLA_ID"] %>% unlist() %>% unname()
        } else if(is.na(control_num)==F & n.mat>control_num){
        n.mat <- control_num
        sampler <- as.numeric(sample(mated_line, n.mat, replace = F))
        sampler_ID <- control_data[sampler, "LTLA_ID"] %>% unlist() %>% unname()
        } else if(is.na(control_num)==F & n.mat<=control_num){
          if (n.mat == 1) {
            case_data[i, "index1"] <- control_data[mated_line, "LTLA_ID"] %>% unlist() %>% unname()
            case_data[i,"all.mat"] <- as.character(mated_line) 
            case_data[i,"sampled"] <- as.character(mated_line)
            next
          } else {
        sampler <- as.numeric(sample(mated_line, n.mat, replace = F))
        sampler_ID <- control_data[sampler, "LTLA_ID"] %>% unlist() %>% unname()
          }
        }
        case_data[i,"all.mat"] <- str_flatten_comma(mated_line) # Label all the sample results
        case_data[i,"sampled"] <- str_flatten_comma(sampler) # Label the selected results
        for (j in 1:n.mat) {
        case_data[i, paste0("index",j)] <- sampler_ID[j]
         }
    } else {
      case_data[i,"index1"] <- NA
    }
  }
  # 上面的代码比较绕，mated_line是根据三个指定的规则在control数据集中找到的对照行的行号，mated_ID则是一直使用的LTLA_ID，后续index列是为了将ID写入，而all.mat和sampled是为了在制作对照的名单时，从control数据集中找到相应的行号
  
  # for (i in 1:length(case_data$type)) {
  #   mat <- which(between(control_data$Pop_Den_km2, case_data$Pop_Den_km2[i]*(1-0.2), case_data$Pop_Den_km2[i]*(1+0.2)) &
  #                  between(control_data$Prosperity, case_data$Prosperity[i]*(1-0.2), case_data$Prosperity[i]*(1+0.2)) &
  #                  control_data$N_num == case_data$N_num[i])
  #   mat1 <- LTLA_mat[mat, "LTLA_ID"] %>% unlist() %>% unname()
  #   if (length(mat1) > 0) {
  #     n.mat <- length(mat1)
  #     sampler <- as.numeric(sample(mat1, n.mat, replace = F))
  #     case_data$index1[i] <- sampler[1]
  #     case_data$index2[i] <- sampler[2]
  #     if (n.mat>=3) {
  #       case_data$index3[i] <- sampler[3]
  #     }else{
  #       case_data$index3[i] <- NA
  #     }
  #   } else{
  #     case_data$index1[i] <- NA
  #   }
  # }
  
  ## Extar all LTLA name and convert it's name to c.LTLA_name (i.e. LTLA name in control ) for the next match
  con_name <- control_data %>% 
    mutate(c_LTLA.ID = LTLA_ID, c_LTLA.name = LTLA_name) %>% 
    dplyr::select(c_LTLA.ID, c_LTLA.name)
  
  compare_and_mark <- function(sampler, all_mat) {
    if (is.na(sampler)) {
      return(NA)
    } else {
      sampler <- as.numeric(unlist(strsplit(sampler, ",")))
      all_mat <- as.numeric(unlist(strsplit(all_mat, ",")))
      if (length(sampler) == length(all_mat)) {
        star <- str_flatten_comma(paste0(unlist(con_name[all_mat[all_mat%in%sampler],"c_LTLA.name"]),"*"))
        return(star)
      } else {
        star <- str_flatten_comma(paste0(unlist(con_name[all_mat[all_mat%in%sampler],"c_LTLA.name"]),"*"))
        no_star <- str_flatten_comma(unlist(con_name[all_mat[!(all_mat%in%sampler)],"c_LTLA.name"]))
        return(str_flatten_comma(c(star, no_star)))
      }
    }
  }
  
  
  matched_name <- data.frame(LTLA_name = case_data$LTLA_name, 
                             mated = map2_chr(case_data$sampled, case_data$all.mat, compare_and_mark),
                             N_num = case_data$N_num) %>% drop_na()
  
  ## Match control LTLA number and Control LTLA name
  matched <- suppressMessages(
    case_data %>% 
      filter(index1 != is.na(index1)) %>% 
      pivot_longer(contains("index"), names_to = "index_name", values_to = "index") %>% 
      group_by(LTLA_ID) %>% 
      mutate(subclass = cur_group_id(), # add a ID number of matched group, cur_group_id func is used to generate unique number of current group
             c_LTLA.ID = index, c_type = 0, k_LTLA.ID = LTLA_ID, k_LTLA.name = LTLA_name, k_type = type) %>% # c=control, k=case
      ungroup() %>%        # dob_child1  dob_child2 gender_child1 gender_child2
      left_join(., con_name) %>% 
      dplyr::select(subclass, k_LTLA.ID, k_LTLA.name, k_type, c_LTLA.ID, c_LTLA.name, c_type) %>% 
      drop_na() %>% 
      pivot_longer(-subclass, names_to = c("klass",".value"), names_sep = "_", values_to = c("LTLA_ID", "LTLA_name", "type")) %>% # 注意这里分隔符只能用下划线_如果用点.的话会出错
      group_by(subclass) %>% 
      distinct(.keep_all = T) %>% 
      dplyr::select(subclass, LTLA_ID = LTLA.ID, LTLA_name = LTLA.name, type)
  )
    
  
  # matched <- case_data %>% 
  #   filter(index1 != is.na(index1)) %>% #
  #   mutate(subclass = 1:length(LTLA_ID), c.type = 0, c.LTLA_ID = index1, # c=control, k=case
  #          k.LTLA_ID = LTLA_ID, k.LTLA_name = LTLA_name, k.type = type) %>% 
  #   left_join(., con_name) %>% 
  #   dplyr::select(subclass, k.LTLA_ID, k.LTLA_name, k.type, c.LTLA_ID, c.LTLA_name, c.type) %>% 
  #   as.data.frame() %>% 
  #   reshape(., direction = "long", 
  #           varying = list(LTLA_ID = c(2,5), name = c(3,6), type = c(4,7)),
  #           v.names = c("LTLA_ID", "LTLA_name", "type"),
  #           idvar = "subclass") %>% 
  #   dplyr::select(! time) %>% 
  #   arrange(subclass, desc(type)) 
  
  matched <- suppressMessages(left_join(matched, matched_name))
  
  m_name <- matched %>% 
    distinct(., subclass, .keep_all = T) %>% 
    transmute(matched = paste(subclass, LTLA_ID, LTLA_name, sep = "_")) %>%
    as.data.frame() %>% .$matched
  
  matched <- setNames(split(matched, matched$subclass), m_name)
  return(matched)
}



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
      control <- data[["LTLA_ID"]][-1] #提取对照的编号，这里可能要改成同时提取多个编号
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
  
  case_dat <- data %>% filter(LTLA_ID == case) %>% 
    dplyr::select(Date = date, LTLA_name, y = newCasesBySpecimenDate) %>% 
    arrange(Date)
  control_dat <- vector("list", length = leng)
  for (i in 1:leng) {
    control_dat[[i]] <- data %>% filter(LTLA_ID == control[i]) %>% dplyr::select(date, LTLA_name, newCasesBySpecimenDate) %>% # 如果是多个对照的话，是写在一个列表里还是直接合并成一个
      arrange(date)
  }
  
  control_dat <- as.data.frame(control_dat) # 如果是多个对照，还能不能转dataframe
  
  if (leng == 1) {
    casual_data <- cbind(case_dat, control_dat)
    if (all(casual_data[,1] == casual_data[,4])) { # all 代表全部比较 # Check whether each date is the same in the case group and the control group
      case_name <- casual_data[1,2]
      casual_data <- casual_data %>% dplyr::select(Date, y, contains("newCasesBy"))
      return(list(name = case_name,
                  data = arrange(casual_data, Date)))
    }else{
      warning("!!! Check case and control date !!!")
    }
  } else{
    get_date_col <- grep("\\bdate\\b", colnames(control_dat), ignore.case = TRUE) # 构建正则表达式用于提取所有包含date的列
    if (all(sapply(control_dat[get_date_col], identical, control_dat[,1]))) { # check case and control 
      casual_data <- cbind(case_dat, control_dat)
      if (all(casual_data[,1] == casual_data[,4])) { # check case and control
        case_name <- casual_data[1,2]
        casual_data <- casual_data %>% dplyr::select(Date, y, contains("newCasesBy")) # 超过两个对照之后，名称怎么写，都用newCasesBy似乎不行
        return(list(name = case_name,
                    data = arrange(casual_data, Date)))
      }else{
        warning("!!! Check control date !!!")
      }
    }else{
      warning("!!! Check each control date, they are not equal !!!")
    }
  }
}


do.Causal <- function(start_date = NULL,
                      intervention_date = NULL,
                      end_date = NULL,
                      data = data,
                      seed = 2723,
                      ahead = 0, # 
                      original = F, # show original report 
                      get.table = F,
                      raw.data = c("date", "series", F), # this option is used to select type of raw data output. "date" means data were aligning by date (the open date is't at same position). "series" means  data were aligning by open date.
                      show.fig = T,
                      save.fig = F,
                      path = NULL,
                      ...) {
  case_loca <- data[["name"]] 
  data <- data[["data"]]
  if (is.POSIXct(data[1,1]) | is.Date(data[1,1])) {
    start_date <- data[1,1]
    end_date <- data[length(data[,1]), 1]
    time.points <- seq.Date(from = as.Date(start_date), to = as.Date(end_date), by = 1)
    x.date <- as.Date(data[,1])
    data <- zoo(data[,-1], x.date)
  } else {
    time.points <- seq.Date(from = as.Date(start_date), 
                            to = as.Date(end_date), by = 1)
    data <- zoo(data, time.points)
  }
  
  pre.period <- c(as.Date(start_date), as.Date(intervention_date))
  post.period <- c(as.Date(intervention_date)+1, as.Date(end_date))
  timecheck <- between(time.points, as.Date(intervention_date)-ahead, as.Date(end_date))
  

  impact <- CausalImpact(data, pre.period, post.period, ...)
  #show(summary(impact))
  
  if (show.fig == T) {
    show(plot(impact))
  }
  
  if (save.fig == T) {
    ggsave(plot(impact), filename = path, width = 7, height = 5)
  }
  
  if (original == T) {
    res_list <- table
  }
  
  if (get.table == T) {
    table <- data.frame(abs_eff = round(impact[["summary"]][["AbsEffect"]][1], 2),
                        abs_lci = round(impact[["summary"]][["AbsEffect.lower"]][1], 2),
                        abs_uci = round(impact[["summary"]][["AbsEffect.upper"]][1], 2),
                        abs_sd = round(impact[["summary"]][["AbsEffect.sd"]][1], 2),
                        relative_eff = round(impact[["summary"]][["RelEffect"]][1]*100, 2),
                        rel_lci = round(impact[["summary"]][["RelEffect.lower"]][1]*100, 2),
                        rel_uci = round(impact[["summary"]][["RelEffect.upper"]][1]*100, 2),
                        rel_sd = round(impact[["summary"]][["RelEffect.sd"]][1]*100, 2),
                        int_date = intervention_date)
    
    res_list <- table
  } 
  
  if (raw.data == "date") {
    raw_mean <- as.data.frame(impact[["raw_mean"]]) %>% setNames(., time.points)
    raw_sample <- as.data.frame(impact[["raw_sample"]]) %>% setNames(., time.points)
    raw_mean <- raw_mean[,timecheck]
    raw_sample <- raw_sample[,timecheck]
    
    res_list <- list(raw_mean = raw_mean,
                     raw_sample = raw_sample)
  } else if(raw.data == "series"){
    raw_mean <- as.data.frame(impact[["raw_mean"]]) %>% setNames(., time.points)
    raw_sample <- as.data.frame(impact[["raw_sample"]]) %>% setNames(., time.points)
    raw_mean <- raw_mean[,timecheck]
    raw_sample <- raw_sample[,timecheck]
    
    series_name <- paste0("V", 1:length(raw_mean))
    raw_mean <- raw_mean %>% setNames(., series_name)
    raw_sample <- raw_sample %>% setNames(., series_name)
    
    res_list <- list(raw_mean = raw_mean,
                     raw_sample = raw_sample)
  }
  
  if (get.table == T & raw.data != F) {
    res_list <- list(table = table,
                     raw_mean = raw_mean,
                     raw_sample = raw_sample)
  }
    return(res_list)
} 

combine.dat <- function(data, 
                        length.dat, # this option is used to input the number of compared LTLA
                        type = c("table", "raw_mean", "raw_sample"),
                        select.length = NULL) { # this option is used to select a specific length of data e.g. 41(10 days before, 1 open and 30 days after opening) 
  if (type == "table") {
    table_list <- vector("list", length = length(length.dat))
    for (i in 1:length(length.dat)) {
      table_list[i] <- data[[i]][type]
    }
    result <- do.call(bind_rows, table_list)
    
    case_control_list <- vector("list", length = length(length.dat))
    for (i in 1:length(length.dat)) {
      case_control_list[[i]] <- suppressMessages(
        length.dat[[i]] %>% 
          drop_na() %>% 
          dplyr::select(LTLA_name, mated, N_num)
      )
    }
    
    case_control_name <- do.call(bind_rows, case_control_list)
    
    record <-names(length.dat)
    result <- cbind(cbind(record, result), case_control_name)

  } else {
    table_list <- vector("list", length = length(length.dat))
    for (i in 1:length(length.dat)) {
      table_list[i] <- data[[i]][type]
    }
    result <- do.call(bind_rows, table_list)
  }
  
  if (is.null(select.length)) {
    return(result)
  } else {
    result <- result[, 1:select.length]
    return(result)
  }
}

before.dat <- function(table, select.length) {
  before_open <- vector("list", length = length(table$record))
  for (i in 1:length(table$record)) {
    before_open[[i]] <- LTLA_gr %>% 
      filter(LTLA_name == table[i,"location"] & date >=  (as.Date(table[i,"int_date"])-10)) %>% # 选择从reopen开始前10天的每个LTLA的真实的数据
      dplyr::select(newCasesBySpecimenDate) %>% 
      t() %>% as.data.frame()
  }
  before_open <- do.call(bind_rows, before_open)
  before_open_meta <- apply(before_open, 2, FUN = function(x) quantile(x, c(0.5), na.rm = T))
  before_open_meta <- before_open_meta[1:select.length] # 选择开放前10天至第41天（即开放后第30天）的数据
  return(before_open_meta)
}

cum.pred <- function(y.samples, point.pred, y, post.period.begin) {
  
  # Compute posterior mean
  is.post.period <- seq_along(y) >= post.period.begin
  cum.pred.mean.pre <- cumsum.na.rm(as.vector(y)[!is.post.period])
  non.na.indices <- which(!is.na(cum.pred.mean.pre))
  assert_that(length(non.na.indices) > 0)
  last.non.na.index <- max(non.na.indices)
  cum.pred.mean.post <- cumsum(point.pred[is.post.period]) + cum.pred.mean.pre[last.non.na.index]
  cum.pred.mean <- c(cum.pred.mean.pre, cum.pred.mean.post)
  
  # Compute posterior interval
  cum.pred.lower.pre <- cum.pred.mean.pre
  cum.pred.upper.pre <- cum.pred.mean.pre
  y.samples.cum.post <- t(apply(y.samples[, is.post.period, drop = FALSE], 1, cumsum)) + cum.pred.mean.pre[last.non.na.index]
  
  if (sum(is.post.period) == 1) {
    y.samples.cum.post <- t(y.samples.cum.post)
  }
  
  cum.pred.lower.post <- as.numeric(t(apply(y.samples.cum.post, 2, FUN = function(x) quantile(x, c(0.025), na.rm = T))))
  cum.pred.upper.post <- as.numeric(t(apply(y.samples.cum.post, 2, FUN = function(x) quantile(x, c(0.975), na.rm = T))))
  cum.pred.lower <- c(cum.pred.lower.pre, cum.pred.lower.post)
  cum.pred.upper <- c(cum.pred.upper.pre, cum.pred.upper.post)
  
  # Put cumulative prediction together
  cum.pred <- data.frame(cum.pred = cum.pred.mean, 
                         cum.pred.lower, cum.pred.upper)
  
  cum.pred$cum.effect <- cumsum.na.rm(y) - cum.pred$cum.pred
  cum.pred$cum.effect.lower <- cumsum.na.rm(y) - cum.pred$cum.pred.upper
  cum.pred$cum.effect.upper <- cumsum.na.rm(y) - cum.pred$cum.pred.lower
  
  return(cum.pred %>% dplyr::select(contains("cum.effect")))
}

report.it <- function(data, length.dat, select.length = 41) { # This func. is used to get meta table. The length is the number of compared group. 
  raw_mean <- combine.dat(data = data, length.dat = length.dat, type = "raw_mean", select.length = select.length)
  mean_meta <- apply(raw_mean, 2, FUN = function(x) quantile(x, c(0.5), na.rm = T))
  raw_sample <- combine.dat(data = data, length.dat = length.dat, type = "raw_sample", select.length = select.length)
  sample_meta <- t(apply(raw_sample, 2, FUN = function(x) quantile(x, c(0.025, 0.975), na.rm = T)))
  meta_table <- data.frame(date = 1:select.length, mean_meta, sample_meta) %>% setNames(., c("date","median", "lci", "uci"))
  
  return(meta_table)
}

plot.it <- function(data, length.dat, report_table, select.length = 41, title, save = F, path = path, width = NULL, height = NULL) {
  
  before_open <- before.dat(report_table, select.length = select.length)
  meta_table <- report.it(data, length.dat, select.length = select.length)
  plot_table_original <- cbind(meta_table,before_open)
  
  f1 <- ggplot(plot_table_original, aes(x = date)) + 
    geom_vline(xintercept = 11, linewidth = 1.2, linetype = 3, colour = "#bc3b29") +
    geom_hline(yintercept = 0, linetype = 2, colour = "grey50") +
    geom_ribbon(aes(ymin = lci, ymax = uci), alpha = 0.1, colour = "#275066", fill = "#275066") +
    geom_line(aes(y = before_open), linewidth = 1.5, colour = "#ad002a", alpha = 0.7) +
    geom_line(aes(y = median), linewidth = 1.2, linetype = 2, colour = "#082243", alpha = 0.7) +
    scale_x_continuous(name = "", breaks = seq(1,select.length, by=2), 
                       labels = c(paste0("-",seq(1,10,by=2)), "Reopen", paste0("+",seq(2,select.length-11,by=2)))) + # c(paste0("B",seq(1,10,by=2)), "Reopen", paste0("A",seq(2,select.length-11,by=2)))
    scale_y_continuous(name = "Pooled log growth rates between\ncase LTLA and control LTLA(s)", limits = c(-1,1)) + #
    labs(tag = "A.") +
    theme_bw() +
    theme(axis.text = element_text(size = 14, colour = "black"),
          axis.title = element_text(size = 16, face = "bold", colour = "black"),
          plot.title = element_text(size = 18, face = "bold", colour = "black"),
          plot.margin = unit(c(0.5,1,0.5,1),"cm"),
          plot.tag = element_text(size = 18),
          plot.tag.position = c(0, 1)) 
  
  raw_mean <- combine.dat(data = data, length.dat = length.dat, type = "raw_mean", select.length = select.length)
  mean_meta <- apply(raw_mean, 2, FUN = function(x) quantile(x, c(0.5), na.rm = T))
  raw_sample <- combine.dat(data = data, length.dat = length.dat, type = "raw_sample", select.length = select.length)
  
  #point.pred.lower <- as.numeric(t(apply(raw_sample, 2, FUN = function(x) quantile(x, c(0.025), na.rm = T))))
  #point.pred.upper <- as.numeric(t(apply(raw_sample, 2, FUN = function(x) quantile(x, c(0.975), na.rm = T))))
  #point.pred <- data.frame(point.pred = mean_meta, point.pred.lower, point.pred.upper)
  
  #plot_table_point <- data.frame(date = 1:41, median = before_open-point.pred$point.pred, lci = before_open - point.pred$point.pred.lower, uci = before_open - point.pred$point.pred.upper)
  
  
  #f2 <- ggplot(plot_table_point, aes(x = date)) + 
  #  geom_vline(xintercept = 11, linewidth = 1.2, linetype = 3, colour = "#bc3b29") +
  #  geom_hline(yintercept = 0, linetype = 2, colour = "grey50") +
  #  geom_ribbon(aes(ymin = lci, ymax = uci), alpha = 0.1, colour = "#275066", fill = "#275066") +
  #  geom_line(aes(y = before_open), linewidth = 1.5, colour = "#ad002a", alpha = 0.7) +
  #  geom_line(aes(y = median), linewidth = 1.2, linetype = 2, colour = "#082243", alpha = 0.7) +
  #  scale_x_continuous(name = "", breaks = seq(1,41, by=2), labels = c(paste0("B",seq(1,10,by=2)), "Reopen", paste0("A",seq(2,30,by=2)))) +
  #  scale_y_continuous(name = "Relationship between reopen", limits = c(-1,1)) + 
  #  labs(tag = "A.") +
  #  theme_bw() +
  #  theme(axis.text = element_text(size = 14, colour = "black"),
  #        axis.title = element_text(size = 16, colour = "black"),
  #        plot.title = element_text(size = 18, colour = "black"),
  #        plot.margin = unit(c(0.5,1,0.5,1),"cm"),
  #        plot.tag = element_text(size = 18),
  #        plot.tag.position = c(0, 1)) 
  

  cum_table <- cum.pred(raw_sample, mean_meta, before_open, 12) # note here. The 12 means we will calculate cum effect from the next day of reopen. We also could calculate from the reopen day

  
  f2 <-  data.frame(date = 1:select.length, cum_table) %>% 
    setNames(., c("date","median", "lci", "uci")) %>% 
    ggplot(aes(x = date)) +
    geom_vline(xintercept = 11, linewidth = 1.2, linetype = 2, colour = "#bc3b29") +
    geom_hline(yintercept = 0, linetype = 2, colour = "grey50") +
    geom_ribbon(aes(ymin = lci, ymax = uci), alpha = 0.1, colour = "#275066", fill = "#275066") +
    geom_line(aes(y = median), linewidth = 2, colour = "#082243") +
    scale_x_continuous(name = "Date", breaks = seq(1,select.length, by=2), 
                       labels = c(paste0("-",seq(1,10,by=2)), "Reopen", paste0("+",seq(2,select.length-11,by=2)))) + # c(paste0("B",seq(1,10,by=2)), "Reopen", paste0("A",seq(2,select.length-11,by=2)))
    scale_y_continuous(name = "Cumulative difference in log growth rate\nbetween case LTLA and control LTLA(s)", breaks = -3:5,limits = c(-3,5), expand = c(0,0)) + 
    labs(tag = "B.") +
    theme_bw() +
    theme(axis.text = element_text(size = 14, colour = "black"),
          axis.title = element_text(size = 16, face = "bold", colour = "black"),
          plot.title = element_text(size = 18, face = "bold", colour = "black"),
          plot.margin = unit(c(0.5,1,0.5,1),"cm"),
          plot.tag = element_text(size = 18),
          plot.tag.position = c(0, 1)) 
  
  finalplot <- f1/f2 + 
    plot_annotation(title = title, 
                    theme = theme(plot.title = element_text(size = 20, face = "bold", colour = "black"), 
                                  plot.margin = unit(c(0.5,0.5,0.5,0.5),"cm")))
  
  if (save == T) {
    ggsave(filename = path, finalplot, width = width, height = height, units = "in")
  }
  plot(finalplot)
}



cal.cum.ci <- function(data, m.data, table, select.length = 41, tidy.out = F) {
  
  matched_tbl <- do.call(bind_rows, m.data) %>% drop_na() %>% arrange(N_num) %>% split(., .$N_num) %>% 
    map(., \(df) df$subclass)
  
  # 这一段算的是 raw_mean 和 raw_sample
  raw_mean_list <- list()
  raw_sample_list <- list()
  before_open_list <- list()
  for (i in 1:length(matched_tbl)) {
    index <- unlist(matched_tbl[i]) %>% unname()
    raw_mean_table_list <- map(index, ~ data[[.x]]$raw_mean)
    raw_sample_table_list <- map(index, ~ data[[.x]]$raw_sample)
    before_open_table_list <- map(index, function(.x){
      LTLA_gr %>% 
        filter(LTLA_name == table[.x,"location"] & date >=  (as.Date(table[.x,"int_date"])-10)) %>% 
        dplyr::select(newCasesBySpecimenDate) %>% 
        t() %>% as.data.frame()
    })
    
    #raw_mean
    raw_mean_all <- do.call(bind_rows, raw_mean_table_list)
    raw_mean_all <- raw_mean_all[, 1:select.length]
    raw_mean_list[[i]] <- raw_mean_all
    
    #raw_sample
    raw_sample_all <- do.call(bind_rows, raw_sample_table_list)
    raw_sample_all <- raw_sample_all[, 1:select.length]
    raw_sample_list[[i]] <- raw_sample_all
    
    #before_open
    before_open_all <- do.call(bind_rows, before_open_table_list)
    before_open_all <- before_open_all[, 1:select.length]
    before_open_list[[i]] <- before_open_all
  }
  
  mean_meta_list <- map(raw_mean_list, ~ map_dbl(.x, ~ quantile(.x, 0.5, na.rm = TRUE)))
  before_open_meta_list <- map(before_open_list, ~map_dbl(.x, ~ quantile(.x, 0.5, na.rm = TRUE)))
  
  cum_table <- pmap(list(raw_sample_list, mean_meta_list, before_open_meta_list, 12), cum.pred)
  
  before_open <- before.dat(table, select.length = select.length)
  raw_mean <- combine.dat(data = data, length.dat = m.data, type = "raw_mean", select.length = select.length)
  mean_meta <- apply(raw_mean, 2, FUN = function(x) quantile(x, c(0.5), na.rm = T))
  raw_sample <- combine.dat(data = data, length.dat = m.data, type = "raw_sample", select.length = select.length)
  cum_table_all <- cum.pred(raw_sample, mean_meta, before_open, 12) 

  if (tidy.out == T) {
    label <- c("day3", "day7", "day14")
    region_code <- table %>% left_join(., region_num, by = "N_num") %>% 
      dplyr::select(N_num, N_region) %>% arrange(N_num) %>% distinct(., .keep_all = T)
    
    cum_table_all <- cum_table_all %>% 
      slice(., c(12+3, 12+7, 12+14)) %>% 
      exp() %>% round(., digits = 2) %>% format(., nsmall = 2) %>% 
      mutate(cum.ci = paste0(cum.effect, " (", cum.effect.lower, ", ", cum.effect.upper, ")"),
             label = label, N_num = 0, N_region = "All region") %>% 
      dplyr::select(cum.ci, label, N_num, N_region) %>% 
      pivot_wider(names_from = "label", values_from = cum.ci) %>% 
      relocate(starts_with("N"))
    
    cum_table_tidy <- cum_table %>% 
      map(., ~format(round(exp(slice(.x, c(12+3, 12+7, 12+14))),digits = 2), nsmall = 2) %>% 
          mutate(cum.ci = paste0(cum.effect, " (", cum.effect.lower, ", ", cum.effect.upper, ")"),
                 label = label) %>% 
          dplyr::select(cum.ci, label) %>% 
          pivot_wider(names_from = "label", values_from = cum.ci)) %>% 
      do.call(bind_rows, .) %>% 
      bind_cols(., region_code) %>% 
      relocate(starts_with("N"))
    
    cum_table_tidy <- bind_rows(cum_table_all, cum_table_tidy)
    return(cum_table_tidy)
    
  } else{
    return(cum_table)
  }
}



# Auto correlation --------------------------------------------------------

get.decay.par <- function(sim.data) { # This function was used to get parameter of decay function. We don't need this any more
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

get.RR <- function(chain1 = NA,
                   chain2 = NA,
                   chain3 = NULL,
                   chain4 = NULL,
                   data = NA, 
                   unit = c("1", "sd"), # RR per 1 unit or per sd unit
                   ci = c(0.5, 0.025, 0.975),
                   coef.num = NULL, # how many coef need to extract
                   digits = 3) {
  if (is.null(chain3)) {
    beta.samples.combined <- rbind(chain1[["samples"]][["beta"]], chain2[["samples"]][["beta"]])
  } else{
    beta.samples.combined <- rbind(chain1[["samples"]][["beta"]], chain2[["samples"]][["beta"]], 
                                   chain3[["samples"]][["beta"]], chain4[["samples"]][["beta"]])
  }
  
  data <- data %>% as.data.frame()
  res_names <- rownames(chain1[["summary.results"]])
  res_matrix <- matrix(data = NA, nrow = coef.num, ncol = 3)
  
  if (unit == "1") {
    for (i in 2:(coef.num+1)) {
      res_matrix[i-1,] <- round(quantile(exp(1 * beta.samples.combined[ ,i]), ci),digits)
    }
  } else {
    for (i in 2:(coef.num+1)) {
      res_matrix[i-1,] <- round(quantile(exp(sd(data[,res_names[i]]) * beta.samples.combined[ ,i]), ci),digits)
    }
  }
  rownames(res_matrix) <- rownames(chain1[["summary.results"]])[2:(coef.num+1)]
  colnames(res_matrix) <- c(paste0(ci[1]*100,"%"), paste0(ci[2]*100,"%"), paste0(ci[3]*100,"%"))
  return(res_matrix)
}


get.risk <- function(chain1 = NA,
                     chain2 = NA,
                     chain3 = NULL,
                     chain4 = NULL,
                     data = NA, 
                     ci = c(0.5, 0.025, 0.975)) {
  if (is.null(chain3)) {
    fitted.samples.combined <- rbind(chain1[["samples"]][["fitted"]], chain2[["samples"]][["fitted"]])
  } else{
    fitted.samples.combined <- rbind(chain1[["samples"]][["fitted"]], chain2[["samples"]][["fitted"]], 
                                     chain3[["samples"]][["fitted"]], chain4[["samples"]][["fitted"]])
  }
  
  data <- data %>% as.data.frame()
  n.samples <- nrow(fitted.samples.combined)
  n.all <- ncol(fitted.samples.combined)
  ## compute the risk distribution
  risk.samples.combined <- fitted.samples.combined / matrix(rep(data$log_GR, nrow(fitted.samples.combined)), 
                                                            nrow=n.samples, ncol=n.all, byrow=TRUE) 
  
  #### Compute the areal unit average risk for each day
  risk.trends <- array(NA, c(n.samples, length(table(data$date))))
  for(i in 1:n.samples)
  {
    risk.trends[i, ] <- tapply(risk.samples.combined[i, ], data$date, mean)
  }
  
  #### Prepare data to plot the average risk trends
  time.trends <- as.data.frame(t(apply(risk.trends, 2, quantile, ci))) %>% 
    mutate(date=names(table(data$date)))
  colnames(time.trends)[1:3] <- c("Median","LCI", "UCI")
  
  return(time.trends)
}


## Calculate
cal.cum.bs <- function(rawdata, region_data, eff_data) {
  first_row <- rawdata %>% 
    left_join(region_data) %>% 
    dplyr::select(starts_with("LTLA"),  date, h_index) %>% 
    arrange(LTLA_ID) %>% 
    group_by(LTLA_ID) %>% 
    mutate(row_num = row_number()) %>% 
    filter(h_index > 0) %>% 
    slice(1) %>% 
    dplyr::select(-c(h_index,date))
  
  # 给eff_all加一个识别列，然后先操作HEIdec，之后在合并两个
  eff_name <- suppressMessages(
    rawdata %>% 
      dplyr::select(starts_with("LTLA"), date) %>% 
      bind_cols(eff_data))
  
  split_dat <- rawdata %>% 
    left_join(region_data) %>% 
    dplyr::select(starts_with("LTLA"), starts_with("N"), date, h_index) %>% 
    left_join(., first_row) %>% 
    arrange(LTLA_ID) %>% 
    group_by(LTLA_ID) %>% 
    filter(row_number() >= row_num &
             row_number() <= row_num+14) %>% 
    arrange(N_num) %>% 
    left_join(., eff_name) %>% 
    split(., .$N_num) %>% 
    map(., function(x) {
      ungroup(x) %>% 
        group_by(LTLA_ID) %>% 
        split(., .$LTLA_ID)
    })
  
  label <- c("day3", "day7", "day14")
  
  cl <- makeCluster(12)
  registerDoParallel(cl)
  table2_bs <- foreach (df = split_dat, # i in 1:12
                        .packages = c("tidyverse"),
                        .combine = rbind) %dopar% {
                          do.call(cbind, df) %>% 
                            #split_dat %>% 
                            # filter(N_num == i) %>% 
                            # group_by(LTLA_ID) %>% 
                            # split(., .$LTLA_ID) %>% 
                            # bind_cols() %>% 
                            dplyr::select(-c(contains("_"), contains("date"))) %>% # 
                            apply(., 1, FUN = function(x) quantile(x, c(0.5, 0.025, 0.975))) %>% 
                            t() %>% 
                            apply(., 2, cumsum) %>% as.data.frame() %>%
                            slice(., c(4,8,15)) %>% 
                            exp() %>% round(., digits = 2) %>% format(., nsmall = 2) %>% 
                            mutate(cum.bs = paste0(`50%`, " (", `2.5%`, ", ", `97.5%`, ")"), label = label) %>% 
                            dplyr::select(cum.bs, label) %>% 
                            pivot_wider(names_from = "label", values_from = cum.bs)
                        }
  stopCluster(cl)
  # closeAllConnections()
  table2_bs_tidy <- region_data %>% dplyr::select(contains("N_")) %>% distinct() %>% bind_cols(., table2_bs) %>% relocate(N_num)
  
  
  ## Calculate effect of HEI(s) reopen in all region
  data_block<- rawdata %>% 
    left_join(region_data) %>% 
    dplyr::select(starts_with("LTLA"), starts_with("N"), date, h_index) %>% 
    left_join(., first_row) %>% 
    arrange(LTLA_ID) %>% 
    group_by(LTLA_ID) %>% 
    filter(row_number() >= row_num &
             row_number() <= row_num+14) %>% 
    left_join(., eff_name) %>% 
    split(., .$LTLA_ID) %>% 
    map(., function(x) ungroup(x) %>% dplyr::select(-c(contains("_"), contains("date")))) %>% 
    split(., ceiling(seq_along(.)/12)) # 将数据分成12个块，使用下面的foreach提升计算速度，此处的12是指电脑的12个核
  
  ## 使用foreach提升合并的计算速度
  cl <- makeCluster(12)
  registerDoParallel(cl)
  combine_block <- foreach (block=data_block, .combine = cbind) %dopar% {do.call(cbind, block)}
  stopCluster(cl)
  
  
  table2_bs_all <- combine_block %>% 
    apply(., 1, FUN = function(x) quantile(x, c(0.5, 0.025, 0.975))) %>% 
    t() %>% 
    apply(., 2, cumsum) %>% as.data.frame() %>% 
    slice(., c(4, 8, 15)) %>% 
    exp() %>% round(., digits = 2) %>% format(., nsmall = 2) %>% 
    mutate(cum.bs = paste0(`50%`, " (", `2.5%`, ", ", `97.5%`, ")"),
           label = label) %>% 
    dplyr::select(cum.bs, label) %>% 
    pivot_wider(names_from = "label", values_from = cum.bs) %>% 
    mutate(N_num = 0, N_region = "All region") %>% 
    relocate(starts_with("N"))
  
  return(rbind(table2_bs_all, table2_bs_tidy))
}
