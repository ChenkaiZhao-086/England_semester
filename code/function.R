# get Rt value ------------------------------------------------------------
crawlOne <- function(
  location,
  prefix = "/Users/chenkaizhao/Documents/600_Project/620_England semester/data/raw/Rt_dat/LTLA_public/"
) {
  # https://raw.githubusercontent.com/ImperialCollegeLondon/covid19local/master/LTLA_public/
  webpage <- read_html(paste(prefix, location, sep = ""))
  body_nodes <- webpage %>%
    html_node("#reproduction-number") %>%
    html_children()
  raw.data <- fromJSON(html_text(body_nodes[4]))
  data.length <- (length(raw.data$x$data$x[[1]]) - 1) / 2
  res.data <- data.frame(
    x = raw.data$x$data$x[[1]][-(data.length * 2 + 1)],
    bound = rep(c("lower", "upper"), each = data.length),
    ci.90 = raw.data$x$data$y[[1]][-(data.length * 2 + 1)],
    ci.60 = raw.data$x$data$y[[2]][-(data.length * 2 + 1)],
    ci.30 = raw.data$x$data$y[[3]][-c(data.length * 2 + 2, data.length + 1)],
    x.upper = raw.data$x$data$x[[5]][1]
  )
  res.data <- res.data[res.data$x <= res.data$x.upper, ]
  res.data <- res.data %>%
    pivot_wider(names_from = bound, values_from = c(ci.90, ci.60, ci.30))
  res.data$Imperial <- location
  message(location)
  return(res.data)
}

### sample Rt
reSample5 <- function(A, B, C, D, E) {
  return(sample(size = 1, x = c(A, B, C, C, D, E)))
}

### calculate CI
CI.cal <- function(vector, digits = 2) {
  basic <- data.frame(
    date = vector[1, "date"],
    Imperial = vector[1, "Imperial"]
  )
  CI <- format(
    epi.conf(vector$sample.R, ctype = "mean.single"),
    digits = digits,
    nsmall = digits
  )
  result <- cbind(basic, CI)
  return(result)
}


# Save map ----------------------------------------------------------------
x11.save <- function(fig = fig, file = file, width = width, height = height) {
  plot(fig)
  # dev.copy2pdf(file = file, width = width, height = height, out.type = "pdf")
  dev.print(pdf, file = file, width = width, height = height)
  dev.off()
} # When change default graphic device from Quartz to Cairo, we can save plot by gg.save. It's seems we do not need this function anymore. By the way, open a separate Cairo window is much faster than the bottom right on mac

# For match LTLA ----------------------------------------------------------
# Calculate moving average OR rolling sum case number
case.cal <- function(
  start_date = "2020-06-01",
  end_date = "2020-12-01",
  type = c("MA", "RS", "week"),
  data
) {
  switch(
    type,
    "MA" = {
      dat <- data %>%
        group_by(LTLA_ID) %>%
        arrange(LTLA_ID, date) %>%
        mutate(
          ma = rollmean(newCasesBySpecimenDate, 7, align = "right", fill = NA),
          newCasesBySpecimenDate = ma
        ) %>%
        dplyr::select(
          areaCode,
          LTLA_ID,
          LTLA_name,
          date,
          newCasesBySpecimenDate
        ) %>%
        drop_na() %>%
        filter(date >= as.Date(start_date) & date <= as.Date(end_date)) %>%
        ungroup()

      return(dat)
    },
    "RS" = {
      dat <- data %>%
        group_by(LTLA_ID) %>%
        arrange(LTLA_ID, date) %>%
        mutate(
          rs = rollsum(newCasesBySpecimenDate, 7, align = "right", fill = NA),
          newCasesBySpecimenDate = rs
        ) %>%
        dplyr::select(
          areaCode,
          LTLA_ID,
          LTLA_name,
          date,
          newCasesBySpecimenDate
        ) %>%
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
        dplyr::select(
          areaCode,
          LTLA_ID,
          LTLA_name,
          date,
          newCasesBySpecimenDate
        ) %>%
        drop_na() %>%
        filter(date >= as.Date(start_date) & date <= as.Date(end_date)) %>%
        ungroup()
    }
  )
}


m.match <- function(m_mat, prop = 0.2, control_num = 3) {
  # control_num is the max number need to match

  ## Separate dataset to case dataset and control dataset
  m_mat <- m_mat %>%
    dplyr::select(LTLA_ID, LTLA_name, type, Pop_Den_km2, Prosperity, N_num)
  case_data <- m_mat %>%
    filter(type == 1) %>%
    mutate(all.mat = NA, sampled = NA, index1 = NA)
  control_data <- m_mat %>% filter(type == 0)

  for (i in 1:length(case_data$type)) {
    mated_line <- which(
      between(
        control_data$Pop_Den_km2,
        case_data$Pop_Den_km2[i] * (1 - 0.2),
        case_data$Pop_Den_km2[i] * (1 + 0.2)
      ) &
        between(
          control_data$Prosperity,
          case_data$Prosperity[i] * (1 - 0.2),
          case_data$Prosperity[i] * (1 + 0.2)
        ) &
        control_data$N_num == case_data$N_num[i]
    )
    if (length(mated_line) > 0) {
      n.mat <- length(mated_line)
      if (control_num == 1) {
        if (n.mat == 1) {
          case_data[i, "index1"] <- control_data[mated_line, "LTLA_ID"] %>%
            unlist() %>%
            unname()
          case_data[i, "all.mat"] <- as.character(mated_line) # 所有符合匹配规则的行号
          case_data[i, "sampled"] <- as.character(mated_line) # 在符合匹配规则的行中根据对照数量随机抽取到的行号
          next # 这个很重要，不加next的话，在只匹配一个的时候会再走一遍下面的 if (n.mat == 1)
        } else {
          sampler <- as.numeric(sample(mated_line, 1, replace = F))
          sampler_ID <- control_data[sampler, "LTLA_ID"] %>%
            unlist() %>%
            unname()
          case_data[i, "index1"] <- sampler_ID
          case_data[i, "all.mat"] <- str_flatten_comma(mated_line) # Label all the sample results
          case_data[i, "sampled"] <- as.character(sampler)
          next
        }
      } else if (is.na(control_num)) {
        sampler <- as.numeric(sample(mated_line, n.mat, replace = F))
        sampler_ID <- control_data[sampler, "LTLA_ID"] %>%
          unlist() %>%
          unname()
      } else if (is.na(control_num) == F & n.mat > control_num) {
        n.mat <- control_num
        sampler <- as.numeric(sample(mated_line, n.mat, replace = F))
        sampler_ID <- control_data[sampler, "LTLA_ID"] %>%
          unlist() %>%
          unname()
      } else if (is.na(control_num) == F & n.mat <= control_num) {
        if (n.mat == 1) {
          case_data[i, "index1"] <- control_data[mated_line, "LTLA_ID"] %>%
            unlist() %>%
            unname()
          case_data[i, "all.mat"] <- as.character(mated_line)
          case_data[i, "sampled"] <- as.character(mated_line)
          next
        } else {
          sampler <- as.numeric(sample(mated_line, n.mat, replace = F))
          sampler_ID <- control_data[sampler, "LTLA_ID"] %>%
            unlist() %>%
            unname()
        }
      }
      case_data[i, "all.mat"] <- str_flatten_comma(mated_line) # Label all the sample results
      case_data[i, "sampled"] <- str_flatten_comma(sampler) # Label the selected results
      for (j in 1:n.mat) {
        case_data[i, paste0("index", j)] <- sampler_ID[j]
      }
    } else {
      case_data[i, "index1"] <- NA
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
        star <- str_flatten_comma(paste0(
          unlist(con_name[all_mat[all_mat %in% sampler], "c_LTLA.name"]),
          "*"
        ))
        return(star)
      } else {
        star <- str_flatten_comma(paste0(
          unlist(con_name[all_mat[all_mat %in% sampler], "c_LTLA.name"]),
          "*"
        ))
        no_star <- str_flatten_comma(unlist(con_name[
          all_mat[!(all_mat %in% sampler)],
          "c_LTLA.name"
        ]))
        return(str_flatten_comma(c(star, no_star)))
      }
    }
  }

  matched_name <- data.frame(
    LTLA_name = case_data$LTLA_name,
    mated = map2_chr(case_data$sampled, case_data$all.mat, compare_and_mark),
    N_num = case_data$N_num
  ) %>%
    drop_na()

  ## Match control LTLA number and Control LTLA name
  matched <- suppressMessages(
    case_data %>%
      filter(index1 != is.na(index1)) %>%
      pivot_longer(
        contains("index"),
        names_to = "index_name",
        values_to = "index"
      ) %>%
      group_by(LTLA_ID) %>%
      mutate(
        subclass = cur_group_id(), # add a ID number of matched group, cur_group_id func is used to generate unique number of current group
        c_LTLA.ID = index,
        c_type = 0,
        k_LTLA.ID = LTLA_ID,
        k_LTLA.name = LTLA_name,
        k_type = type
      ) %>% # c=control, k=case
      ungroup() %>% # dob_child1  dob_child2 gender_child1 gender_child2
      left_join(., con_name) %>%
      dplyr::select(
        subclass,
        k_LTLA.ID,
        k_LTLA.name,
        k_type,
        c_LTLA.ID,
        c_LTLA.name,
        c_type
      ) %>%
      drop_na() %>%
      pivot_longer(
        -subclass,
        names_to = c("klass", ".value"),
        names_sep = "_",
        values_to = c("LTLA_ID", "LTLA_name", "type")
      ) %>% # 注意这里分隔符只能用下划线_如果用点.的话会出错
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
    as.data.frame() %>%
    .$matched

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
get.info <- function(
  data = data, # this parameter need to be a indexed list, like thisform: a[[1]]
  open_data = NULL,
  type = c("case", "control", "open")
) {
  if (is.numeric(data)) {
    open_list <- open_data[open_data$LTLA_ID == data, "HEI_opening"]
    open <- open_list %>%
      distinct() %>%
      arrange(HEI_opening) %>%
      as.data.frame()
    return(as.Date(open[1, 1]))
    print("As the data  is numeric, the returned value is the opening time")
  } else if (type == "case") {
    if (data[["type"]][1] == 1) {
      case <- data[["LTLA_ID"]][1]
      return(case)
    } else {
      stop("!!! Check the CASE NUMBER !!!")
    }
  } else if (type == "control") {
    if (data[["type"]][2] == 0) {
      control <- data[["LTLA_ID"]][-1] # 提取对照的编号，这里可能要改成同时提取多个编号
      return(control)
    } else {
      stop("!!! Check the CONTROL NUMBER !!!")
    }
  } else if (type == "open") {
    if (data[["type"]][1] == 1) {
      case <- data[["LTLA_ID"]][1]
      if (is.null(open_data)) {
        stop("!!! Inpute the open_data dataset !!!")
      }
    } else {
      stop("!!! Check the CASE NUMBER !!!")
    }
    open_list <- open_data[open_data$LTLA_ID == case, "HEI_opening"]
    open <- open_list %>%
      distinct() %>%
      arrange(HEI_opening) %>%
      as.data.frame()
    return(as.Date(open[1, 1]))
  }
}


prep.Causal <- function(
  data = data,
  case = case, # only one
  control = control # could be a vactor
) {
  leng <- length(control)

  case_dat <- data %>%
    filter(LTLA_ID == case) %>%
    dplyr::select(Date = date, LTLA_name, y = newCasesBySpecimenDate) %>%
    arrange(Date)
  control_dat <- vector("list", length = leng)
  for (i in 1:leng) {
    control_dat[[i]] <- data %>%
      filter(LTLA_ID == control[i]) %>%
      dplyr::select(date, LTLA_name, newCasesBySpecimenDate) %>% # 如果是多个对照的话，是写在一个列表里还是直接合并成一个
      arrange(date)
  }

  control_dat <- as.data.frame(control_dat) # 如果是多个对照，还能不能转dataframe

  if (leng == 1) {
    casual_data <- cbind(case_dat, control_dat)
    if (all(casual_data[, 1] == casual_data[, 4])) {
      # all 代表全部比较 # Check whether each date is the same in the case group and the control group
      case_name <- casual_data[1, 2]
      casual_data <- casual_data %>%
        dplyr::select(Date, y, contains("newCasesBy"))
      return(list(
        name = case_name,
        data = arrange(casual_data, Date)
      ))
    } else {
      warning("!!! Check case and control date !!!")
    }
  } else {
    get_date_col <- grep(
      "\\bdate\\b",
      colnames(control_dat),
      ignore.case = TRUE
    ) # 构建正则表达式用于提取所有包含date的列
    if (all(sapply(control_dat[get_date_col], identical, control_dat[, 1]))) {
      # check case and control
      casual_data <- cbind(case_dat, control_dat)
      if (all(casual_data[, 1] == casual_data[, 4])) {
        # check case and control
        case_name <- casual_data[1, 2]
        casual_data <- casual_data %>%
          dplyr::select(Date, y, contains("newCasesBy")) # 超过两个对照之后，名称怎么写，都用newCasesBy似乎不行
        return(list(
          name = case_name,
          data = arrange(casual_data, Date)
        ))
      } else {
        warning("!!! Check control date !!!")
      }
    } else {
      warning("!!! Check each control date, they are not equal !!!")
    }
  }
}


do.Causal <- function(
  start_date = NULL,
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
  ...
) {
  case_loca <- data[["name"]]
  data <- data[["data"]]
  if (is.POSIXct(data[1, 1]) | is.Date(data[1, 1])) {
    start_date <- data[1, 1]
    end_date <- data[length(data[, 1]), 1]
    time.points <- seq.Date(
      from = as.Date(start_date),
      to = as.Date(end_date),
      by = 1
    )
    x.date <- as.Date(data[, 1])
    data <- zoo(data[, -1], x.date)
  } else {
    time.points <- seq.Date(
      from = as.Date(start_date),
      to = as.Date(end_date),
      by = 1
    )
    data <- zoo(data, time.points)
  }

  pre.period <- c(as.Date(start_date), as.Date(intervention_date))
  post.period <- c(as.Date(intervention_date) + 1, as.Date(end_date))
  timecheck <- between(
    time.points,
    as.Date(intervention_date) - ahead,
    as.Date(end_date)
  )

  impact <- CausalImpact(data, pre.period, post.period, ...)
  # show(summary(impact))

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
    table <- data.frame(
      abs_eff = round(impact[["summary"]][["AbsEffect"]][2], 2),
      abs_lci = round(impact[["summary"]][["AbsEffect.lower"]][2], 2),
      abs_uci = round(impact[["summary"]][["AbsEffect.upper"]][2], 2),
      abs_sd = round(impact[["summary"]][["AbsEffect.sd"]][2], 2),
      relative_eff = round(impact[["summary"]][["RelEffect"]][1] * 100, 2),
      rel_lci = round(impact[["summary"]][["RelEffect.lower"]][1] * 100, 2),
      rel_uci = round(impact[["summary"]][["RelEffect.upper"]][1] * 100, 2),
      rel_sd = round(impact[["summary"]][["RelEffect.sd"]][1] * 100, 2),
      int_date = intervention_date
    )

    res_list <- table
  }

  if (raw.data == "date") {
    raw_mean <- as.data.frame(impact[["raw_mean"]]) %>% setNames(., time.points)
    raw_sample <- as.data.frame(impact[["raw_sample"]]) %>%
      setNames(., time.points)
    raw_mean <- raw_mean[, timecheck]
    raw_sample <- raw_sample[, timecheck]

    res_list <- list(
      raw_mean = raw_mean,
      raw_sample = raw_sample
    )
  } else if (raw.data == "series") {
    raw_mean <- as.data.frame(impact[["raw_mean"]]) %>% setNames(., time.points)
    raw_sample <- as.data.frame(impact[["raw_sample"]]) %>%
      setNames(., time.points)
    raw_mean <- raw_mean[, timecheck]
    raw_sample <- raw_sample[, timecheck]

    series_name <- paste0("V", 1:length(raw_mean))
    raw_mean <- raw_mean %>% setNames(., series_name)
    raw_sample <- raw_sample %>% setNames(., series_name)

    res_list <- list(
      raw_mean = raw_mean,
      raw_sample = raw_sample
    )
  }

  if (get.table == T & raw.data != F) {
    res_list <- list(
      table = table,
      raw_mean = raw_mean,
      raw_sample = raw_sample
    )
  }
  return(res_list)
}

combine.dat <- function(
  data,
  length.dat, # this option is used to input the number of compared LTLA
  type = c("table", "raw_mean", "raw_sample"),
  select.length = NULL
) {
  # this option is used to select a specific length of data e.g. 41(10 days before, 1 open and 30 days after opening)
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

    record <- names(length.dat)
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
      filter(
        LTLA_name == table[i, "location"] &
          date >= (as.Date(table[i, "int_date"]) - 10)
      ) %>% # 选择从reopen开始前10天的每个LTLA的真实的数据
      dplyr::select(newCasesBySpecimenDate) %>%
      t() %>%
      as.data.frame()
  }
  before_open <- do.call(bind_rows, before_open)
  before_open_meta <- apply(before_open, 2, FUN = function(x) {
    quantile(x, c(0.5), na.rm = T)
  })
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
  cum.pred.mean.post <- cumsum(point.pred[is.post.period]) +
    cum.pred.mean.pre[last.non.na.index]
  cum.pred.mean <- c(cum.pred.mean.pre, cum.pred.mean.post)

  # Compute posterior interval
  cum.pred.lower.pre <- cum.pred.mean.pre
  cum.pred.upper.pre <- cum.pred.mean.pre
  y.samples.cum.post <- t(apply(
    y.samples[, is.post.period, drop = FALSE],
    1,
    cumsum
  )) +
    cum.pred.mean.pre[last.non.na.index]

  if (sum(is.post.period) == 1) {
    y.samples.cum.post <- t(y.samples.cum.post)
  }

  cum.pred.lower.post <- as.numeric(t(apply(
    y.samples.cum.post,
    2,
    FUN = function(x) quantile(x, c(0.025), na.rm = T)
  )))
  cum.pred.upper.post <- as.numeric(t(apply(
    y.samples.cum.post,
    2,
    FUN = function(x) quantile(x, c(0.975), na.rm = T)
  )))
  cum.pred.lower <- c(cum.pred.lower.pre, cum.pred.lower.post)
  cum.pred.upper <- c(cum.pred.upper.pre, cum.pred.upper.post)

  # Put cumulative prediction together
  cum.pred <- data.frame(
    cum.pred = cum.pred.mean,
    cum.pred.lower,
    cum.pred.upper
  )

  cum.pred$cum.effect <- cumsum.na.rm(y) - cum.pred$cum.pred
  cum.pred$cum.effect.lower <- cumsum.na.rm(y) - cum.pred$cum.pred.upper
  cum.pred$cum.effect.upper <- cumsum.na.rm(y) - cum.pred$cum.pred.lower

  return(cum.pred %>% dplyr::select(contains("cum.effect")))
}

report.it <- function(data, length.dat, select.length = 41) {
  # This func. is used to get meta table. The length is the number of compared group.
  raw_mean <- combine.dat(
    data = data,
    length.dat = length.dat,
    type = "raw_mean",
    select.length = select.length
  )
  mean_meta <- apply(raw_mean, 2, FUN = function(x) {
    quantile(x, c(0.5), na.rm = T)
  })
  raw_sample <- combine.dat(
    data = data,
    length.dat = length.dat,
    type = "raw_sample",
    select.length = select.length
  )
  sample_meta <- t(apply(raw_sample, 2, FUN = function(x) {
    quantile(x, c(0.025, 0.975), na.rm = T)
  }))
  meta_table <- data.frame(date = 1:select.length, mean_meta, sample_meta) %>%
    setNames(., c("date", "median", "lci", "uci"))

  return(meta_table)
}

plot.it <- function(
  data,
  length.dat,
  report_table,
  select.length = 41,
  title,
  save = F,
  path = path,
  width = NULL,
  height = NULL
) {
  before_open <- before.dat(report_table, select.length = select.length)
  meta_table <- report.it(data, length.dat, select.length = select.length)
  plot_table_original <- cbind(meta_table, before_open)

  f1 <- ggplot(plot_table_original, aes(x = date)) +
    geom_vline(
      xintercept = 11,
      linewidth = 1.2,
      linetype = 2,
      colour = "#bc3b29"
    ) +
    geom_hline(yintercept = 0, linetype = 2, colour = "grey50") +
    geom_ribbon(
      aes(ymin = lci, ymax = uci),
      alpha = 0.1,
      colour = "#275066",
      fill = "#275066"
    ) +
    geom_line(
      aes(y = before_open),
      linewidth = 1.5,
      colour = "#ad002a",
      alpha = 0.7
    ) +
    geom_line(
      aes(y = median),
      linewidth = 1.2,
      linetype = 2,
      colour = "#082243",
      alpha = 0.7
    ) +
    scale_x_continuous(
      name = "",
      breaks = seq(1, select.length, by = 2),
      labels = c(
        paste0("-", rev(seq(2, 10, by = 2))),
        "Reopen",
        paste0("+", seq(2, select.length - 11, by = 2))
      )
    ) + # c(paste0("B",seq(1,10,by=2)), "Reopen", paste0("A",seq(2,select.length-11,by=2)))
    scale_y_continuous(
      name = "Pooled log growth rates between\ncase LTLA and control LTLA(s)",
      limits = c(-1, 1)
    ) + #
    labs(tag = "(1). ") +
    theme_bw() +
    theme(
      axis.text = element_text(size = 14, colour = "black"),
      axis.title = element_text(size = 16, face = "bold", colour = "black"),
      plot.title = element_text(size = 18, face = "bold", colour = "black"),
      plot.margin = unit(c(0.5, 1, 0.5, 1), "cm"),
      plot.tag = element_text(size = 18),
      plot.tag.position = c(0, 1)
    )

  raw_mean <- combine.dat(
    data = data,
    length.dat = length.dat,
    type = "raw_mean",
    select.length = select.length
  )
  mean_meta <- apply(raw_mean, 2, FUN = function(x) {
    quantile(x, c(0.5), na.rm = T)
  })
  raw_sample <- combine.dat(
    data = data,
    length.dat = length.dat,
    type = "raw_sample",
    select.length = select.length
  )

  # point.pred.lower <- as.numeric(t(apply(raw_sample, 2, FUN = function(x) quantile(x, c(0.025), na.rm = T))))
  # point.pred.upper <- as.numeric(t(apply(raw_sample, 2, FUN = function(x) quantile(x, c(0.975), na.rm = T))))
  # point.pred <- data.frame(point.pred = mean_meta, point.pred.lower, point.pred.upper)

  # plot_table_point <- data.frame(date = 1:41, median = before_open-point.pred$point.pred, lci = before_open - point.pred$point.pred.lower, uci = before_open - point.pred$point.pred.upper)

  # f2 <- ggplot(plot_table_point, aes(x = date)) +
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

  cum_table <- cum.pred(raw_sample, mean_meta, before_open, 12)
  # note here. The 12 means we will calculate cum effect from the next day of reopen. We also could calculate from the reopen day

  f2 <- data.frame(date = 1:select.length, cum_table) %>%
    setNames(., c("date", "median", "lci", "uci")) %>%
    ggplot(aes(x = date)) +
    geom_vline(
      xintercept = 11,
      linewidth = 1.2,
      linetype = 2,
      colour = "#bc3b29"
    ) +
    geom_hline(yintercept = 0, linetype = 2, colour = "grey50") +
    geom_ribbon(
      aes(ymin = lci, ymax = uci),
      alpha = 0.1,
      colour = "#275066",
      fill = "#275066"
    ) +
    geom_line(aes(y = median), linewidth = 2, colour = "#082243") +
    scale_x_continuous(
      name = "Date",
      breaks = seq(1, select.length, by = 2),
      labels = c(
        paste0("-", rev(seq(2, 10, by = 2))),
        "Reopen",
        paste0("+", seq(2, select.length - 11, by = 2))
      )
    ) + # c(paste0("B",seq(1,10,by=2)), "Reopen", paste0("A",seq(2,select.length-11,by=2)))
    scale_y_continuous(
      name = "Cumulative difference in log growth rate\nbetween case LTLA and control LTLA(s)",
      breaks = -3:5,
      limits = c(-3, 5),
      expand = c(0, 0)
    ) +
    labs(tag = "(2). ") +
    theme_bw() +
    theme(
      axis.text = element_text(size = 14, colour = "black"),
      axis.title = element_text(size = 16, face = "bold", colour = "black"),
      plot.title = element_text(size = 18, face = "bold", colour = "black"),
      plot.margin = unit(c(0.5, 1, 0.5, 1), "cm"),
      plot.tag = element_text(size = 18),
      plot.tag.position = c(0, 1)
    )

  finalplot <- f1 /
    f2 +
    plot_annotation(
      title = title,
      theme = theme(
        plot.title = element_text(size = 20, face = "bold", colour = "black"),
        plot.margin = unit(c(0.5, 0.5, 0.5, 0.5), "cm")
      )
    )

  if (save == T) {
    ggsave(
      filename = path,
      finalplot,
      width = width,
      height = height,
      units = "in"
    )
  }
  plot(finalplot)
}

cal.cum.ci.region <- function(
  data,
  m.data,
  table,
  # region_num, # 新增：N_num → N_region 映射表，与 cal.cum.ci 中的 region_num 一致
  select.length = 42
  # title = NULL,
  # save = FALSE,
  # path = NULL,
  # width = NULL,
  # height = NULL
) {
  # ── Step 1: 构建 subclass 索引（与 cal.cum.ci 完全一致）─────────────────────
  matched_tbl <- do.call(bind_rows, m.data) %>%
    drop_na() %>%
    arrange(N_num) %>%
    split(., .$N_num) %>%
    map(., \(df) df$subclass)

  N_num_values <- names(matched_tbl) # character 型，如 "1","2",...
  n_groups <- length(matched_tbl)

  # ── Step 2: 获取 N_num -> N_region 映射（与 cal.cum.ci tidy.out=T 一致）──────
  region_code <- table %>%
    left_join(region_num, by = "N_num") %>%
    dplyr::select(N_num, N_region) %>%
    arrange(N_num) %>%
    distinct(.keep_all = TRUE)

  # ── Step 3: 按 N_num 组提取数据并计算累积效应 ────────────────────────────────
  raw_mean_list <- vector("list", n_groups)
  raw_sample_list <- vector("list", n_groups)
  mean_meta_list <- vector("list", n_groups)
  before_open_meta_list <- vector("list", n_groups)

  for (i in seq_len(n_groups)) {
    index <- unlist(matched_tbl[i]) %>% unname()

    raw_mean_all <- map(index, ~ data[[.x]]$raw_mean) %>%
      do.call(bind_rows, .) %>%
      .[, 1:select.length]

    raw_sample_all <- map(index, ~ data[[.x]]$raw_sample) %>%
      do.call(bind_rows, .) %>%
      .[, 1:select.length]

    before_open_all <- map(index, function(.x) {
      LTLA_gr %>%
        filter(
          LTLA_name == table[.x, "location"] &
            date >= (as.Date(table[.x, "int_date"]) - 10)
        ) %>%
        dplyr::select(newCasesBySpecimenDate) %>%
        t() %>%
        as.data.frame()
    }) %>%
      do.call(bind_rows, .) %>%
      .[, 1:select.length]

    raw_mean_list[[i]] <- raw_mean_all
    raw_sample_list[[i]] <- raw_sample_all
    mean_meta_list[[i]] <- map_dbl(
      raw_mean_all,
      ~ quantile(.x, 0.5, na.rm = TRUE)
    )
    before_open_meta_list[[i]] <- map_dbl(
      before_open_all,
      ~ quantile(.x, 0.5, na.rm = TRUE)
    )
  }

  cum_table_list <- pmap(
    list(raw_sample_list, mean_meta_list, before_open_meta_list, 12),
    cum.pred
  )

  # ── Step 4: All region 累积效应（与 plot.it 完全一致）──────────────────────
  before_open_all_region <- before.dat(table, select.length = select.length)

  raw_mean_all_region <- combine.dat(
    data = data,
    length.dat = m.data,
    type = "raw_mean",
    select.length = select.length
  )
  mean_meta_all_region <- apply(raw_mean_all_region, 2, FUN = function(x) {
    quantile(x, 0.5, na.rm = TRUE)
  })

  raw_sample_all_region <- combine.dat(
    data = data,
    length.dat = m.data,
    type = "raw_sample",
    select.length = select.length
  )
  cum_table_all_region <- cum.pred(
    raw_sample_all_region,
    mean_meta_all_region,
    before_open_all_region,
    12
  )

  # ── Step 5: 构建含标识列的 raw_plot_data ────────────────────────────────────
  # 各 N_num 组附加标识列
  cum_table_labeled_list <- map2(
    cum_table_list,
    seq_len(n_groups),
    function(tbl, i) {
      n_val <- as.integer(N_num_values[i])
      n_region <- region_code %>%
        filter(N_num == n_val) %>%
        pull(N_region)
      n_region <- if (length(n_region) == 0) NA_character_ else n_region[1]

      tbl %>%
        mutate(
          date = seq_len(select.length),
          N_num = n_val,
          N_region = n_region
        ) %>%
        relocate(date, N_num, N_region)
    }
  )

  # All region 附加标识列
  cum_table_all_labeled <- cum_table_all_region %>%
    mutate(
      date = seq_len(select.length),
      N_num = 0L,
      N_region = "All region"
    ) %>%
    relocate(date, N_num, N_region)

  # 合并为完整原始数据框（All region 排首行）
  raw_plot_data <- bind_rows(cum_table_all_labeled, cum_table_labeled_list)

  # ── Step 6: 构建统一绘图信息列表（All region 排第一）────────────────────────
  group_info_list <- c(
    list(list(
      cum_table = cum_table_all_region,
      n_num = 0L,
      n_region = "All region",
      tag = "(All). "
    )),
    map(seq_len(n_groups), function(i) {
      n_val <- as.integer(N_num_values[i])
      n_region <- region_code %>%
        filter(N_num == n_val) %>%
        pull(N_region)
      n_region <- if (length(n_region) == 0) NA_character_ else n_region[1]
      list(
        cum_table = cum_table_list[[i]],
        n_num = n_val,
        n_region = n_region,
        tag = paste0("(", i, "). ")
      )
    })
  )

  # # ── Step 7: 逐组绘图，以 N_region 命名列表 ──────────────────────────────────
  # n_total <- length(group_info_list)
  # plot_list <- setNames(
  #   vector("list", n_total),
  #   sapply(group_info_list, `[[`, "n_region")
  # )

  # for (i in seq_len(n_total)) {
  #   grp <- group_info_list[[i]]

  #   plot_title <- if (!is.null(title)) {
  #     paste0(title, "  (", grp$n_region, ")")
  #   } else {
  #     grp$n_region
  #   }

  #   p <- data.frame(date = seq_len(select.length), grp$cum_table) %>%
  #     setNames(c("date", "median", "lci", "uci")) %>%
  #     ggplot(aes(x = date)) +
  #     geom_vline(
  #       xintercept = 11,
  #       linewidth = 1.2,
  #       linetype = 2,
  #       colour = "#bc3b29"
  #     ) +
  #     geom_hline(yintercept = 1, linetype = 2, colour = "grey50") +
  #     geom_ribbon(
  #       aes(ymin = exp(lci), ymax = exp(uci)),
  #       alpha = 0.1,
  #       colour = "#275066",
  #       fill = "#275066"
  #     ) +
  #     geom_line(aes(y = exp(median)), linewidth = 2, colour = "#082243") +
  #     scale_x_continuous(
  #       name = "Date",
  #       breaks = seq(1, select.length, by = 2),
  #       labels = c(
  #         paste0("-", rev(seq(2, 10, by = 2))),
  #         "Reopen",
  #         paste0("+", seq(2, select.length - 11, by = 2))
  #       )
  #     ) +
  #     scale_y_continuous(
  #       name = "Cumulative difference in log growth rate\nbetween case LTLA and control LTLA(s)",
  #       breaks = -3:5,
  #       expand = c(0, 0)
  #     ) +
  #     labs(title = plot_title, tag = grp$tag) +
  #     theme_bw() +
  #     theme(
  #       axis.text = element_text(size = 14, colour = "black"),
  #       axis.title = element_text(size = 16, face = "bold", colour = "black"),
  #       plot.title = element_text(size = 18, face = "bold", colour = "black"),
  #       plot.margin = unit(c(0.5, 1, 0.5, 1), "cm"),
  #       plot.tag = element_text(size = 18),
  #       plot.tag.position = c(0, 1)
  #     )

  #   plot_list[[i]] <- p

  #   if (isTRUE(save) && !is.null(path)) {
  #     # 将 N_region 名称中的特殊字符替换为下划线，确保文件名合法
  #     safe_name <- gsub("[^A-Za-z0-9_]", "_", grp$n_region)
  #     save_path <- sub("(\\.\\w+)$", paste0("_", safe_name, "\\1"), path)
  #     ggsave(
  #       filename = save_path,
  #       plot = p,
  #       width = width,
  #       height = height,
  #       units = "in"
  #     )
  #   }
  # }

  # for (p in plot_list) {
  #   plot(p)
  # }
  # invisible(list(
  #   plots = plot_list,
  #   raw_plot_data = raw_plot_data
  # ))
  return(raw_plot_data)
}


cal.cum.ci <- function(data, m.data, table, select.length = 42, tidy.out = F) {
  # ← ① 默认值 41 → 42
  matched_tbl <- do.call(bind_rows, m.data) %>%
    drop_na() %>%
    arrange(N_num) %>%
    split(., .$N_num) %>%
    map(., \(df) df$subclass)

  raw_mean_list <- list()
  raw_sample_list <- list()
  before_open_list <- list()
  for (i in 1:length(matched_tbl)) {
    index <- unlist(matched_tbl[i]) %>% unname()
    raw_mean_table_list <- map(index, ~ data[[.x]]$raw_mean)
    raw_sample_table_list <- map(index, ~ data[[.x]]$raw_sample)
    before_open_table_list <- map(index, function(.x) {
      LTLA_gr %>%
        filter(
          LTLA_name == table[.x, "location"] &
            date >= (as.Date(table[.x, "int_date"]) - 10)
        ) %>%
        dplyr::select(newCasesBySpecimenDate) %>%
        t() %>%
        as.data.frame()
    })

    raw_mean_all <- do.call(bind_rows, raw_mean_table_list)
    raw_mean_all <- raw_mean_all[, 1:select.length]
    raw_mean_list[[i]] <- raw_mean_all

    raw_sample_all <- do.call(bind_rows, raw_sample_table_list)
    raw_sample_all <- raw_sample_all[, 1:select.length]
    raw_sample_list[[i]] <- raw_sample_all

    before_open_all <- do.call(bind_rows, before_open_table_list)
    before_open_all <- before_open_all[, 1:select.length]
    before_open_list[[i]] <- before_open_all
  }

  mean_meta_list <- map(
    raw_mean_list,
    ~ map_dbl(.x, ~ quantile(.x, 0.5, na.rm = TRUE))
  )
  before_open_meta_list <- map(
    before_open_list,
    ~ map_dbl(.x, ~ quantile(.x, 0.5, na.rm = TRUE))
  )

  cum_table <- pmap(
    list(raw_sample_list, mean_meta_list, before_open_meta_list, 12),
    cum.pred
  )

  before_open <- before.dat(table, select.length = select.length)
  raw_mean <- combine.dat(
    data = data,
    length.dat = m.data,
    type = "raw_mean",
    select.length = select.length
  )
  mean_meta <- apply(raw_mean, 2, FUN = function(x) {
    quantile(x, c(0.5), na.rm = T)
  })
  raw_sample <- combine.dat(
    data = data,
    length.dat = m.data,
    type = "raw_sample",
    select.length = select.length
  )
  cum_table_all <- cum.pred(raw_sample, mean_meta, before_open, 12)

  if (tidy.out == T) {
    label <- c("day3", "day7", "day14", "day30") # ← ② 新增 "day30"
    region_code <- table %>%
      left_join(., region_num, by = "N_num") %>%
      dplyr::select(N_num, N_region) %>%
      arrange(N_num) %>%
      distinct(., .keep_all = T)

    cum_table_all <- cum_table_all %>%
      slice(., c(12 + 3, 12 + 7, 12 + 14, 12 + 30)) %>% # ← ③ 新增 12+30
      exp() %>%
      round(., digits = 2) %>%
      format(., nsmall = 2) %>%
      mutate(
        cum.ci = paste0(
          cum.effect,
          " (",
          cum.effect.lower,
          ", ",
          cum.effect.upper,
          ")"
        ),
        label = label,
        N_num = 0,
        N_region = "All region"
      ) %>%
      dplyr::select(cum.ci, label, N_num, N_region) %>%
      pivot_wider(names_from = "label", values_from = cum.ci) %>%
      relocate(starts_with("N"))

    cum_table_tidy <- cum_table %>%
      map(
        .,
        ~ format(
          round(
            exp(slice(.x, c(12 + 3, 12 + 7, 12 + 14, 12 + 30))),
            digits = 2
          ), # ← ④ 新增 12+30
          nsmall = 2
        ) %>%
          mutate(
            cum.ci = paste0(
              cum.effect,
              " (",
              cum.effect.lower,
              ", ",
              cum.effect.upper,
              ")"
            ),
            label = label
          ) %>%
          dplyr::select(cum.ci, label) %>%
          pivot_wider(names_from = "label", values_from = cum.ci)
      ) %>%
      do.call(bind_rows, .) %>%
      bind_cols(., region_code) %>%
      relocate(starts_with("N"))

    cum_table_tidy <- bind_rows(cum_table_all, cum_table_tidy)
    return(cum_table_tidy)
  } else {
    return(cum_table)
  }
}

cal.cum.ci.LTLA <- function(
  data,
  m.data,
  table,
  select.length = 42,
  tidy.out = FALSE
) {
  BASELINE <- 12L # 基线期行数
  ALL_DAYS <- c(day3 = 3L, day7 = 7L, day14 = 14L, day30 = 30L)
  # ── 1. 构建 subclass 级别的分组信息 ──────────────────────────────────────
  # 合并所有匹配结果，按 subclass 分组，
  # 取每组中 N_num 不为 NA 的行（即处理单元）的 LTLA_name 作为标签
  subclass_info <- do.call(bind_rows, m.data) %>%
    group_by(subclass) %>%
    summarize(
      subclass_name = LTLA_name[!is.na(N_num)][1], # 处理单元名称
      N_num = N_num[!is.na(N_num)][1], # 保留 N_num 供输出使用
      .groups = "drop"
    ) %>%
    arrange(subclass)

  sc_idx <- subclass_info$subclass # subclass 编号向量

  # ── 2. 工具函数 ──────────────────────────────────────────────────────────
  # trim_cols <- function(df) df[, seq_len(select.length), drop = FALSE]

  # robust trim_cols: coerce to data.frame and select up to available columns
  trim_cols <- function(df, len = select.length) {
    actual_len <- min(len, ncol(df)) # ← 核心修复
    df[, seq_len(actual_len), drop = FALSE]
  }
  col_median <- function(df) map_dbl(df, ~ quantile(.x, 0.5, na.rm = TRUE))

  # ── 3. 按 subclass 逐个提取数据并计算累积因果效应 ────────────────────────
  # 每个 subclass 对应一次干预，独立完成一次 cum.pred 计算
  cum_table <- map(sc_idx, function(sc) {
    raw_mean <- trim_cols(data[[sc]]$raw_mean)
    raw_sample <- trim_cols(data[[sc]]$raw_sample)
    before_open <- LTLA_gr %>%
      filter(
        LTLA_name == table[sc, "location"],
        date >= as.Date(table[sc, "int_date"]) - 10
      ) %>%
      dplyr::select(newCasesBySpecimenDate) %>%
      t() %>%
      as.data.frame() %>%
      trim_cols()

    cum.pred(raw_sample, col_median(raw_mean), col_median(before_open), 12)
  })

  # tidy.out = FALSE 时直接返回各 subclass 的完整时间序列列表
  if (!tidy.out) {
    return(cum_table)
  }

  # # ── 4. 全局合并计算（仅 tidy.out = TRUE 时执行）──────────────────────────
  before_open_g <- before.dat(table, select.length = select.length)
  raw_mean_g <- combine.dat(
    data = data,
    length.dat = m.data,
    type = "raw_mean",
    select.length = select.length
  )
  raw_sample_g <- combine.dat(
    data = data,
    length.dat = m.data,
    type = "raw_sample",
    select.length = select.length
  )
  mean_meta_g <- apply(raw_mean_g, 2, quantile, 0.5, na.rm = TRUE)
  cum_all <- cum.pred(raw_sample_g, mean_meta_g, before_open_g, 12)

  # ── 5. 整洁化输出 ────────────────────────────────────────────────────────
  label <- c("day3", "day7", "day14", "day30")
  slice_rows <- 12 + c(3, 7, 14, 30)

  # 将单个 cum.pred 结果格式化为一行宽格式
  make_tidy_row <- function(ct, n_num, n_region) {
    # 1. 计算该 subclass 实际可用的最大天数
    max_avail_day <- nrow(ct) - BASELINE

    # 2. 筛选出数据量足够的节点（e.g. 数据不足30天时只保留 day3/day7/day14）
    avail_days <- ALL_DAYS[ALL_DAYS <= max_avail_day]

    # 3. 若连3天都不够则跳过，返回 NULL
    if (length(avail_days) == 0) {
      warning(sprintf(
        "地区 '%s' 可用数据不足 %d 天，已跳过",
        n_region,
        min(ALL_DAYS)
      ))
      return(NULL)
    }

    # 4. 计算可用节点的效应
    effects <- ct %>%
      slice(BASELINE + avail_days) %>%
      exp() %>%
      round(digits = 2) %>%
      format(nsmall = 2) %>%
      mutate(
        cum.ci = paste0(
          cum.effect,
          " (",
          cum.effect.lower,
          ", ",
          cum.effect.upper,
          ")"
        ),
        label = names(avail_days)
      ) %>%
      dplyr::select(cum.ci, label) %>%
      pivot_wider(names_from = "label", values_from = "cum.ci")

    # 5. 补全缺失的天数列为 NA，保持输出列结构一致
    missing_cols <- setdiff(names(ALL_DAYS), names(effects))
    effects[missing_cols] <- NA_character_

    effects %>%
      mutate(N_num = n_num, N_region = n_region) %>%
      relocate(N_num, N_region, names(ALL_DAYS)) # 固定列顺序
  }

  # 全局汇总行（N_num = 0）
  global_row <- make_tidy_row(cum_all, n_num = 0, n_region = "All region")

  # 各 subclass 行：用处理单元的 LTLA_name 作为 N_region 标签
  per_sc_rows <- pmap_dfr(
    list(cum_table, subclass_info$N_num, subclass_info$subclass_name),
    make_tidy_row
  )

  bind_rows(global_row, per_sc_rows)
}


# Auto correlation --------------------------------------------------------

get.decay.par <- function(sim.data) {
  # This function was used to get parameter of decay function. We don't need this any more
  par <- nls(y ~ SSasymp(t, yf, y0, log_alpha), data = sim.data) %>%
    tidy() %>%
    dplyr::select(term, estimate) %>%
    spread(term, estimate) %>%
    mutate(alpha = exp(log_alpha))
  return(par)
}

gen.decay <- function(decay.period, decay.fun, incubation) {
  x <- 1:decay.period
  y <- eval(parse(text = decay.fun))[1:(decay.period - 2)]
  inc.time <- rep(0, incubation)
  time1 <- c(inc.time, 1, y, 0)
  return(time1)
}

get.index <- function(dat, decay) {
  h.indes_matrix <- cbind(
    expand.grid(
      h.index = dat$h_index,
      weight = decay
    ),
    expand.grid(
      h.index_c = c(1:length(dat$h_index)),
      weight_c = 1:length(decay)
    )
  )
  h.indes_matrix$value <- h.indes_matrix$h.index * h.indes_matrix$weight
  h.indes_matrix$day <- h.indes_matrix$h.index_c + h.indes_matrix$weight_c - 1

  h_dat <- dat %>%
    dplyr::select(!contains("h_index"))

  h_index_dat <- h.indes_matrix %>%
    group_by(day) %>%
    filter(day <= length(dat$h_index)) %>%
    summarise(h_index = sum(value))

  result <- cbind(h_dat, h_index_dat[, 2])
  return(result)
}

get.RR <- function(
  Dat,
  data = NA,
  unit = c("1", "sd", "student"), # RR per 1 unit or per sd unit
  digits = 3
) {
  Dat <- Dat[["summary.results"]]
  Num.row <- nrow(Dat)

  data <- data %>% as.data.frame()
  res_names <- rownames(Dat)
  res_matrix <- matrix(data = NA, nrow = (Num.row - 8), ncol = 3)

  if (unit == "1") {
    for (i in 2:(Num.row - 7)) {
      res_matrix[i - 1, 1] <- round(exp(10 * Dat[i, 1]), digits)
      res_matrix[i - 1, 2] <- round(exp(10 * Dat[i, 2]), digits)
      res_matrix[i - 1, 3] <- round(exp(10 * Dat[i, 3]), digits)
    }
  } else if (unit == "sd") {
    for (i in 2:(Num.row - 7)) {
      res_matrix[i - 1, 1] <- round(
        exp(sd(data[, res_names[i]]) * Dat[i, 1]),
        digits
      )
      res_matrix[i - 1, 2] <- round(
        exp(sd(data[, res_names[i]]) * Dat[i, 2]),
        digits
      )
      res_matrix[i - 1, 3] <- round(
        exp(sd(data[, res_names[i]]) * Dat[i, 3]),
        digits
      )
    }
  } else {
    # h index
    res_matrix[1, 1] <- round(exp(5000 / 40382 * Dat[2, 1]), digits)
    res_matrix[1, 2] <- round(exp(5000 / 40382 * Dat[2, 2]), digits)
    res_matrix[1, 3] <- round(exp(5000 / 40382 * Dat[2, 3]), digits)
    if (Num.row - 8 > 2) {
      # retail gr
      res_matrix[2, 1] <- round(exp(0.6 * Dat[3, 1]), digits)
      res_matrix[2, 2] <- round(exp(0.6 * Dat[3, 2]), digits)
      res_matrix[2, 3] <- round(exp(0.6 * Dat[3, 3]), digits)
      for (i in 4:(Num.row - 7)) {
        res_matrix[i - 1, 1] <- round(
          exp(sd(data[, res_names[i]]) * Dat[i, 1]),
          digits
        )
        res_matrix[i - 1, 2] <- round(
          exp(sd(data[, res_names[i]]) * Dat[i, 2]),
          digits
        )
        res_matrix[i - 1, 3] <- round(
          exp(sd(data[, res_names[i]]) * Dat[i, 3]),
          digits
        )
      }
    }
  }
  rownames(res_matrix) <- rownames(Dat)[2:(Num.row - 7)]
  colnames(res_matrix) <- c("50%", "2.5%", "97.5%")
  return(res_matrix)
}


get.risk <- function(
  chain1 = NA,
  chain2 = NA,
  chain3 = NULL,
  chain4 = NULL,
  data = NA,
  ci = c(0.5, 0.025, 0.975)
) {
  if (is.null(chain3)) {
    fitted.samples.combined <- rbind(
      chain1[["samples"]][["fitted"]],
      chain2[["samples"]][["fitted"]]
    )
  } else {
    fitted.samples.combined <- rbind(
      chain1[["samples"]][["fitted"]],
      chain2[["samples"]][["fitted"]],
      chain3[["samples"]][["fitted"]],
      chain4[["samples"]][["fitted"]]
    )
  }

  data <- data %>% as.data.frame()
  n.samples <- nrow(fitted.samples.combined)
  n.all <- ncol(fitted.samples.combined)
  ## compute the risk distribution
  risk.samples.combined <- fitted.samples.combined /
    matrix(
      rep(data$log_GR, nrow(fitted.samples.combined)),
      nrow = n.samples,
      ncol = n.all,
      byrow = TRUE
    )

  #### Compute the areal unit average risk for each day
  risk.trends <- array(NA, c(n.samples, length(table(data$date))))
  for (i in 1:n.samples) {
    risk.trends[i, ] <- tapply(risk.samples.combined[i, ], data$date, mean)
  }

  #### Prepare data to plot the average risk trends
  time.trends <- as.data.frame(t(apply(risk.trends, 2, quantile, ci))) %>%
    mutate(date = names(table(data$date)))
  colnames(time.trends)[1:3] <- c("Median", "LCI", "UCI")

  return(time.trends)
}

## Calculate
# cal.cum.bs <- function(rawdata, region_data, eff_data) {
#   # ── 准备公共数据 ────────────────────────────────────────────────────────────
#   first_row <- rawdata %>%
#     left_join(region_data) %>%
#     dplyr::select(starts_with("LTLA"), date, h_index) %>%
#     arrange(LTLA_ID) %>%
#     group_by(LTLA_ID) %>%
#     mutate(row_num = row_number()) %>%
#     filter(h_index > 0) %>%
#     slice(1) %>%
#     dplyr::select(-c(h_index, date))

#   eff_name <- suppressMessages(
#     rawdata %>%
#       dplyr::select(starts_with("LTLA"), date) %>%
#       bind_cols(eff_data)
#   )

#   split_dat <- rawdata %>%
#     left_join(region_data) %>%
#     dplyr::select(starts_with("LTLA"), starts_with("N"), date, h_index) %>%
#     left_join(., first_row) %>%
#     arrange(LTLA_ID) %>%
#     group_by(LTLA_ID) %>%
#     filter(
#       row_number() >= row_num &
#         row_number() <= row_num + 30
#     ) %>%
#     arrange(N_num) %>%
#     left_join(., eff_name) %>%
#     split(., .$N_num) %>%
#     map(., function(x) {
#       ungroup(x) %>%
#         group_by(LTLA_ID) %>%
#         split(., .$LTLA_ID)
#     })

#   label <- c("day3", "day7", "day14", "day30")

#   # ── 启动并行集群 ────────────────────────────────────────────────────────────
#   n_cores <- max(1, detectCores() - 1)
#   cl <- makeCluster(n_cores)
#   registerDoParallel(cl)
#   on.exit(stopCluster(cl), add = TRUE) # 无论函数正常退出还是报错，都自动释放集群

#   # ── Part 1: 按 N_num 组并行计算 ────────────────────────────────────────────
#   table2_bs_list <- foreach(
#     i = seq_along(split_dat),
#     .combine = rbind,
#     .packages = c("dplyr", "tidyr", "purrr"),
#     .export = c("label")
#   ) %dopar%
#     {
#       df <- split_dat[[i]]

#       # [FIX A] 过滤行数不足 31 的 LTLA，保证 cbind 时行数一致
#       row_counts <- sapply(df, nrow)
#       excluded <- names(row_counts)[row_counts < 31]
#       df <- df[row_counts >= 31]

#       # 若过滤后没有任何 LTLA，返回全 NA 占位行
#       if (length(df) == 0) {
#         return(
#           setNames(
#             as.data.frame(matrix(
#               NA_character_,
#               nrow = 1,
#               ncol = length(label)
#             )),
#             label
#           )
#         )
#       }

#       do.call(cbind, df) %>%
#         dplyr::select(-c(contains("_"), contains("date"))) %>%
#         apply(., 1, FUN = function(x) quantile(x, c(0.5, 0.025, 0.975))) %>%
#         t() %>%
#         apply(., 2, cumsum) %>%
#         as.data.frame() %>%
#         slice(., c(4, 8, 15, 31)) %>%
#         exp() %>%
#         round(., digits = 2) %>%
#         format(., nsmall = 2) %>%
#         mutate(
#           cum.bs = paste0(`50%`, " (", `2.5%`, ", ", `97.5%`, ")"),
#           label = label
#         ) %>%
#         dplyr::select(cum.bs, label) %>%
#         pivot_wider(names_from = "label", values_from = cum.bs)
#     }

#   table2_bs_tidy <- region_data %>%
#     dplyr::select(contains("N_")) %>%
#     distinct() %>%
#     bind_cols(., table2_bs_list) %>%
#     relocate(N_num)

#   # ── Part 2: All region 并行计算 ────────────────────────────────────────────

#   # [FIX A] 在构建 data_block 之前统一过滤行数不足 31 的 LTLA
#   ltla_list <- rawdata %>%
#     left_join(region_data) %>%
#     dplyr::select(starts_with("LTLA"), starts_with("N"), date, h_index) %>%
#     left_join(., first_row) %>%
#     arrange(LTLA_ID) %>%
#     group_by(LTLA_ID) %>%
#     filter(
#       row_number() >= row_num &
#         row_number() <= row_num + 30
#     ) %>%
#     left_join(., eff_name) %>%
#     split(., .$LTLA_ID)

#   all_row_counts <- sapply(ltla_list, nrow)
#   excluded_global <- names(all_row_counts)[all_row_counts < 31]

#   if (length(excluded_global) > 0) {
#     message(sprintf(
#       "[All region] 排除 %d 个数据不足的 LTLA: %s",
#       length(excluded_global),
#       paste(excluded_global, collapse = ", ")
#     ))
#   }

#   ltla_list <- ltla_list[all_row_counts >= 31]

#   data_block <- ltla_list %>%
#     map(., function(x) {
#       ungroup(x) %>% dplyr::select(-c(contains("_"), contains("date")))
#     }) %>%
#     split(., ceiling(seq_along(.) / 12))

#   # 并行合并每个 block
#   combine_list <- foreach(
#     j = seq_along(data_block),
#     .combine = "list",
#     .multicombine = TRUE,
#     .packages = c("dplyr")
#   ) %dopar%
#     {
#       do.call(cbind, data_block[[j]])
#     }

#   combine_block <- do.call(cbind, combine_list)

#   table2_bs_all <- combine_block %>%
#     apply(., 1, FUN = function(x) quantile(x, c(0.5, 0.025, 0.975))) %>%
#     t() %>%
#     apply(., 2, cumsum) %>%
#     as.data.frame() %>%
#     slice(., c(4, 8, 15, 31)) %>%
#     exp() %>%
#     round(., digits = 2) %>%
#     format(., nsmall = 2) %>%
#     mutate(
#       cum.bs = paste0(`50%`, " (", `2.5%`, ", ", `97.5%`, ")"),
#       label = label
#     ) %>%
#     dplyr::select(cum.bs, label) %>%
#     pivot_wider(names_from = "label", values_from = cum.bs) %>%
#     mutate(N_num = 0, N_region = "All region") %>%
#     relocate(starts_with("N"))

#   return(rbind(table2_bs_all, table2_bs_tidy))
# }

cal.cum.bs.LTLA <- function(rawdata, region_data, eff_data) {
  # ── 准备公共数据 ────────────────────────────────────────────────────────────
  first_row <- rawdata %>%
    left_join(region_data) %>%
    dplyr::select(starts_with("LTLA"), date, h_index) %>%
    arrange(LTLA_ID) %>%
    group_by(LTLA_ID) %>%
    mutate(row_num = row_number()) %>%
    filter(h_index > 0) %>%
    slice(1) %>%
    dplyr::select(-c(h_index, date))

  eff_name <- suppressMessages(
    rawdata %>%
      dplyr::select(starts_with("LTLA"), date) %>%
      bind_cols(eff_data)
  )

  # ── 直接按 LTLA_ID 分割，每个 LTLA 为独立单元 ─────────────────────────────
  split_dat <- rawdata %>%
    left_join(region_data) %>%
    dplyr::select(starts_with("LTLA"), starts_with("N"), date, h_index) %>%
    left_join(., first_row) %>%
    arrange(LTLA_ID) %>%
    group_by(LTLA_ID) %>%
    filter(
      row_number() >= row_num &
        row_number() <= row_num + 30
    ) %>%
    left_join(., eff_name) %>%
    split(., .$LTLA_ID) # ← 关键改动：直接按 LTLA_ID 拆分

  # ── 天数 → cumsum 行号映射 ─────────────────────────────────────────────────
  # 第1行 = day0(基线), 第4行 = day3, 第8行 = day7, 第15行 = day14, 第31行 = day30
  day_row_map <- c(day3 = 4L, day7 = 8L, day14 = 15L, day30 = 31L)
  all_labels <- names(day_row_map)

  # ── 启动并行集群 ────────────────────────────────────────────────────────────
  n_cores <- max(1, detectCores() - 1)
  cl <- makeCluster(n_cores)
  registerDoParallel(cl)
  on.exit(stopCluster(cl), add = TRUE)

  # ── 按 LTLA_ID 并行计算累积效应 ────────────────────────────────────────────
  table2_bs_list <- foreach(
    i = seq_along(split_dat),
    .combine = bind_rows,
    .packages = c("dplyr", "tidyr"),
    .export = c("day_row_map", "all_labels")
  ) %dopar%
    {
      df <- split_dat[[i]]
      ltla_id <- df$LTLA_ID[1]
      ltla_name <- df$LTLA_name[1]
      n_rows <- nrow(df)

      # ── 动态筛选可计算的天数（核心修复）──────────────────────────────────────
      # 例：nrow = 20，则 day30(需31行)被过滤，只保留 day3/day7/day14
      avail_rows <- day_row_map[day_row_map <= n_rows]

      if (length(avail_rows) == 0) {
        # 连 day3 都不够（< 4行），返回全 NA 占位行
        message(sprintf(
          "[跳过] LTLA_ID=%s (%s): 仅有 %d 行，不足以计算任何节点效应",
          ltla_id,
          ltla_name,
          n_rows
        ))
        result <- as.data.frame(
          matrix(NA_character_, nrow = 1, ncol = length(all_labels))
        )
        names(result) <- all_labels
        result$LTLA_ID <- as.character(ltla_id)
        result$LTLA_name <- ltla_name
        return(dplyr::relocate(result, LTLA_ID, LTLA_name))
      }

      # ── 提取 bootstrap 效应列（去除含"_"或"date"的元数据列）─────────────────
      numeric_data <- df %>%
        ungroup() %>%
        dplyr::select(-c(contains("_"), contains("date")))

      # ── 行方向分位数 → 累积和（列方向）──────────────────────────────────────
      cum_mat <- numeric_data %>%
        apply(1, FUN = function(x) quantile(x, c(0.5, 0.025, 0.975))) %>%
        t() %>%
        apply(2, cumsum) %>%
        as.data.frame()

      # ── 只取有数据的行，格式化为 "median (lower, upper)" ─────────────────────
      cum_sliced <- cum_mat %>%
        slice(unname(avail_rows)) %>%
        exp() %>%
        round(digits = 2) %>%
        format(nsmall = 2) %>%
        mutate(
          cum.bs = paste0(`50%`, " (", `2.5%`, ", ", `97.5%`, ")"),
          label = names(avail_rows)
        ) %>%
        dplyr::select(cum.bs, label) %>%
        pivot_wider(names_from = "label", values_from = cum.bs)

      # ── 不可计算的天数列补 NA，保证所有行输出结构一致 ────────────────────────
      missing_cols <- setdiff(all_labels, names(cum_sliced))
      cum_sliced[missing_cols] <- NA_character_

      cum_sliced %>%
        mutate(
          LTLA_ID = ltla_id,
          LTLA_name = ltla_name
        ) %>%
        dplyr::relocate(LTLA_ID, LTLA_name, all_of(all_labels))
    }

  return(table2_bs_list)
}

cal.cum.bs <- function(rawdata, region_data, eff_data) {
  # ── 准备公共数据 ────────────────────────────────────────────────────────────
  first_row <- rawdata %>%
    left_join(region_data) %>%
    dplyr::select(starts_with("LTLA"), date, h_index) %>%
    arrange(LTLA_ID) %>%
    group_by(LTLA_ID) %>%
    mutate(row_num = row_number()) %>%
    filter(h_index > 0) %>%
    slice(1) %>%
    dplyr::select(-c(h_index, date))

  eff_name <- suppressMessages(
    rawdata %>%
      dplyr::select(starts_with("LTLA"), date) %>%
      bind_cols(eff_data)
  )

  split_dat <- rawdata %>%
    left_join(region_data) %>%
    dplyr::select(starts_with("LTLA"), starts_with("N"), date, h_index) %>%
    left_join(., first_row) %>%
    arrange(LTLA_ID) %>%
    group_by(LTLA_ID) %>%
    filter(
      row_number() >= row_num &
        row_number() <= row_num + 30
    ) %>%
    arrange(N_num) %>%
    left_join(., eff_name) %>%
    split(., .$N_num) %>%
    map(., function(x) {
      ungroup(x) %>%
        group_by(LTLA_ID) %>%
        split(., .$LTLA_ID)
    })

  label <- c("day3", "day7", "day14", "day30")
  n_num_values <- names(split_dat) # character，如 "1","2",...，与 split() 排序一致

  # N_num → N_region 映射（用于组装 raw_plot_data 时查找 N_region）
  n_num_region_map <- region_data %>%
    dplyr::select(N_num, N_region) %>%
    distinct()

  # ── 启动并行集群 ────────────────────────────────────────────────────────────
  n_cores <- max(1, detectCores() - 1)
  cl <- makeCluster(n_cores)
  registerDoParallel(cl)
  on.exit(stopCluster(cl), add = TRUE)

  # ── Part 1: 按 N_num 组并行计算 ────────────────────────────────────────────
  # 关键改动：每次迭代返回 list(summary = ..., plot_data = ...)
  # 不指定 .combine，foreach 默认以 list 收集结果
  results_list <- foreach(
    i = seq_along(split_dat),
    .packages = c("dplyr", "tidyr", "purrr"),
    .export = c("label")
  ) %dopar%
    {
      df <- split_dat[[i]]

      # [FIX A] 过滤行数不足 31 的 LTLA
      row_counts <- sapply(df, nrow)
      df <- df[row_counts >= 31]

      # 过滤后无可用 LTLA：返回 NA 占位
      if (length(df) == 0) {
        return(list(
          summary = setNames(
            as.data.frame(matrix(
              NA_character_,
              nrow = 1,
              ncol = length(label)
            )),
            label
          ),
          plot_data = data.frame(
            date = seq_len(31),
            cum.effect = NA_real_,
            cum.effect.lower = NA_real_,
            cum.effect.upper = NA_real_
          )
        ))
      }

      # 计算完整 31 行累积时序（对数尺度，与 plot.it.by.Nnum 一致，不 exp()）
      cum_full <- do.call(cbind, df) %>%
        dplyr::select(-c(contains("_"), contains("date"))) %>%
        apply(., 1, FUN = function(x) quantile(x, c(0.5, 0.025, 0.975))) %>%
        t() %>%
        apply(., 2, cumsum) %>%
        as.data.frame() # 列名: "50%", "2.5%", "97.5%"

      # ① 汇总行（原有逻辑，4 个时间点，exp 后格式化）── 完全不变
      summary_row <- cum_full %>%
        slice(c(4, 8, 15, 31)) %>%
        exp() %>%
        round(digits = 2) %>%
        format(nsmall = 2) %>%
        mutate(
          cum.bs = paste0(`50%`, " (", `2.5%`, ", ", `97.5%`, ")"),
          label = label
        ) %>%
        dplyr::select(cum.bs, label) %>%
        pivot_wider(names_from = "label", values_from = cum.bs)

      # ② 绘图数据（全 31 行，对数尺度，列名与 raw_plot_data 一致）── 新增
      plot_data <- cum_full %>%
        setNames(c("cum.effect", "cum.effect.lower", "cum.effect.upper")) %>%
        mutate(date = row_number()) %>%
        relocate(date)

      list(summary = summary_row, plot_data = plot_data)
    }

  # 拆分：summary 路径（原逻辑）和 plot_data 路径（新增）
  table2_bs_list <- do.call(bind_rows, map(results_list, "summary"))
  plot_data_groups <- map(results_list, "plot_data")

  # 原有汇总表组装逻辑，完全不变
  table2_bs_tidy <- region_data %>%
    dplyr::select(contains("N_")) %>%
    distinct() %>%
    bind_cols(., table2_bs_list) %>%
    relocate(N_num)

  # ── Part 2: All region 并行计算 ────────────────────────────────────────────
  ltla_list <- rawdata %>%
    left_join(region_data) %>%
    dplyr::select(starts_with("LTLA"), starts_with("N"), date, h_index) %>%
    left_join(., first_row) %>%
    arrange(LTLA_ID) %>%
    group_by(LTLA_ID) %>%
    filter(
      row_number() >= row_num &
        row_number() <= row_num + 30
    ) %>%
    left_join(., eff_name) %>%
    split(., .$LTLA_ID)

  all_row_counts <- sapply(ltla_list, nrow)
  excluded_global <- names(all_row_counts)[all_row_counts < 31]

  if (length(excluded_global) > 0) {
    message(sprintf(
      "[All region] 排除 %d 个数据不足的 LTLA: %s",
      length(excluded_global),
      paste(excluded_global, collapse = ", ")
    ))
  }

  ltla_list <- ltla_list[all_row_counts >= 31]

  data_block <- ltla_list %>%
    map(function(x) {
      ungroup(x) %>% dplyr::select(-c(contains("_"), contains("date")))
    }) %>%
    split(., ceiling(seq_along(.) / 12))

  combine_list <- foreach(
    j = seq_along(data_block),
    .combine = "list",
    .multicombine = TRUE,
    .packages = c("dplyr")
  ) %dopar%
    {
      do.call(cbind, data_block[[j]])
    }

  combine_block <- do.call(cbind, combine_list)

  # All region 完整累积时序（对数尺度）── 原有管道拆分为两段
  cum_full_all <- combine_block %>%
    apply(., 1, FUN = function(x) quantile(x, c(0.5, 0.025, 0.975))) %>%
    t() %>%
    apply(., 2, cumsum) %>%
    as.data.frame() # 列名: "50%", "2.5%", "97.5%"

  # All region 汇总行（原有逻辑，完全不变）
  table2_bs_all <- cum_full_all %>%
    slice(c(4, 8, 15, 31)) %>%
    exp() %>%
    round(digits = 2) %>%
    format(nsmall = 2) %>%
    mutate(
      cum.bs = paste0(`50%`, " (", `2.5%`, ", ", `97.5%`, ")"),
      label = label
    ) %>%
    dplyr::select(cum.bs, label) %>%
    pivot_wider(names_from = "label", values_from = cum.bs) %>%
    mutate(N_num = 0, N_region = "All region") %>%
    relocate(starts_with("N"))

  summary_table <- rbind(table2_bs_all, table2_bs_tidy)

  # ── 组装 raw_plot_data（新增）──────────────────────────────────────────────
  # 各 N_num 组：附加 N_num / N_region 标识列
  plot_data_by_group <- map2(
    plot_data_groups,
    seq_along(plot_data_groups),
    function(pd, i) {
      n_val <- as.integer(n_num_values[i])
      n_reg <- n_num_region_map %>%
        filter(N_num == n_val) %>%
        pull(N_region)
      n_reg <- if (length(n_reg) == 0) NA_character_ else n_reg[1]

      pd %>%
        mutate(N_num = n_val, N_region = n_reg) %>%
        relocate(date, N_num, N_region)
    }
  )

  # All region：附加标识列（N_num = 0，与 plot.it.by.Nnum 一致）
  plot_data_all_region <- cum_full_all %>%
    setNames(c("cum.effect", "cum.effect.lower", "cum.effect.upper")) %>%
    mutate(
      date = row_number(),
      N_num = 0L,
      N_region = "All region"
    ) %>%
    relocate(date, N_num, N_region)

  # 合并（All region 排首，与 plot.it.by.Nnum 的 raw_plot_data 结构一致）
  raw_plot_data <- bind_rows(plot_data_all_region, plot_data_by_group)

  # ── 返回 ────────────────────────────────────────────────────────────────────
  return(list(
    summary_table = summary_table,
    raw_plot_data = raw_plot_data
  ))
}

# get.daily.risk.bs <- function(
#   rawdata,
#   eff_data,
#   ltla_col = "ltla_name",
#   date_col = "date",
#   Nnum_col = "N_num",
#   h_index_col = "h_index",
#   n_days = 31
# ) {
#   # ── 0. 标准化 eff_data 列名（若为无名矩阵则自动命名）──────────────────────────
#   eff_df <- as.data.frame(eff_data)
#   if (is.null(colnames(eff_df)) || all(colnames(eff_df) == "")) {
#     colnames(eff_df) <- paste0("S", seq_len(ncol(eff_df)))
#   }
#   sample_cols <- colnames(eff_df)

#   # ── 1. 将 LTLA/date 键列绑定到 eff_data，构建可供 left_join 的效应表 ──────────
#   #     与 cal.cum.bs 中 eff_name 的构建逻辑一致
#   eff_name <- dplyr::bind_cols(
#     rawdata %>% dplyr::select(dplyr::all_of(c(ltla_col, date_col))),
#     eff_df
#   )

#   # ── 2. 找到每个 LTLA 首个 h_index > 0 的内部行号（即开放日）────────────────────
#   first_row <- rawdata %>%
#     dplyr::group_by(dplyr::across(dplyr::all_of(ltla_col))) %>%
#     dplyr::mutate(.row_num = dplyr::row_number()) %>%
#     dplyr::filter(.data[[h_index_col]] > 0) %>%
#     dplyr::slice(1) %>%
#     dplyr::ungroup() %>%
#     dplyr::select(dplyr::all_of(ltla_col), .row_num_start = .row_num)

#   # ── 3. 以各 LTLA 自身开放日为第 1 行对齐，提取 n_days 行，并 join 效应数据 ──────
#   split_dat <- rawdata %>%
#     dplyr::group_by(dplyr::across(dplyr::all_of(ltla_col))) %>%
#     dplyr::mutate(.row_num = dplyr::row_number()) %>%
#     dplyr::ungroup() %>%
#     dplyr::left_join(first_row, by = ltla_col) %>%
#     dplyr::filter(
#       !is.na(.row_num_start),
#       .row_num >= .row_num_start,
#       .row_num < .row_num_start + n_days
#     ) %>%
#     dplyr::mutate(.time_index = .row_num - .row_num_start + 1L) %>%
#     dplyr::left_join(eff_name, by = c(ltla_col, date_col)) %>%
#     dplyr::select(
#       dplyr::all_of(c(ltla_col, Nnum_col)),
#       .time_index,
#       dplyr::all_of(sample_cols)
#     )

#   # ── 4. 核心辅助函数：对传入子集计算每日累积风险 ──────────────────────────────
#   #   与 cal.cum.bs 原理一致：
#   #   Step A：每个时间点 t，将组内全部 LTLA × 4000 后验样本展平，计算跨 LTLA 分位数
#   #   Step B：对三列分位数分别做逐日 cumsum（累积对数效应）
#   #   Step C：exp() 还原至比值/风险尺度
#   compute_cum_risk <- function(dat) {
#     time_idx <- dat$.time_index

#     # 提前转为矩阵，避免在循环中重复调用 dplyr，提升性能
#     samp_mat <- dat %>%
#       dplyr::select(dplyr::all_of(sample_cols)) %>%
#       as.matrix() # 维度：(n_LTLA × n_days) 行 × 4000 列

#     # Step A：逐时间点计算跨 LTLA × 样本的分位数
#     quant_mat <- do.call(
#       rbind,
#       lapply(seq_len(n_days), function(t) {
#         rows_t <- which(time_idx == t)

#         if (length(rows_t) == 0) {
#           return(setNames(
#             c(NA_real_, NA_real_, NA_real_),
#             c("median", "lower", "upper")
#           ))
#         }

#         # 将当前时间点所有 LTLA 的 4000 个样本展平为一个向量
#         vals <- as.vector(samp_mat[rows_t, , drop = FALSE])
#         quantile(vals, probs = c(0.5, 0.025, 0.975), na.rm = TRUE) %>%
#           setNames(c("median", "lower", "upper"))
#       })
#     )
#     # quant_mat 维度：n_days × 3

#     # Step B + C：列方向 cumsum → exp
#     cum_exp_mat <- exp(apply(quant_mat, 2, cumsum))

#     data.frame(
#       day = seq_len(n_days) - 1L, # day 0（开放当日）至 day 30
#       median = cum_exp_mat[, "median"],
#       lower = cum_exp_mat[, "lower"],
#       upper = cum_exp_mat[, "upper"]
#     )
#   }

#   # ── 5. 按 N_num 分组计算 ────────────────────────────────────────────────────
#   nnum_vals <- sort(unique(split_dat[[Nnum_col]]))

#   nnum_results <- lapply(nnum_vals, function(n) {
#     sub_dat <- dplyr::filter(split_dat, .data[[Nnum_col]] == n)
#     compute_cum_risk(sub_dat) %>%
#       dplyr::mutate(!!Nnum_col := as.character(n))
#   }) %>%
#     dplyr::bind_rows()

#   # ── 6. All region：全部 LTLA 不分组合并后计算 ────────────────────────────────
#   all_result <- compute_cum_risk(split_dat) %>%
#     dplyr::mutate(!!Nnum_col := "All")

#   # ── 7. 合并为 long format，按 N_num 和 day 排序后返回 ───────────────────────
#   final_result <- dplyr::bind_rows(nnum_results, all_result) %>%
#     dplyr::select(dplyr::all_of(Nnum_col), day, median, lower, upper) %>%
#     dplyr::arrange(dplyr::across(dplyr::all_of(Nnum_col)), day)

#   return(final_result)
# }
