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
# Calculate trailing means/sums or weekly totals from daily case counts.
# data contains dated LTLA counts; daily zeros are replaced by 0.5 before
# aggregation. Missing counts remain missing. Returns the dated count table.
case.cal <- function(
  start_date = "2020-06-01",
  end_date = "2020-12-01",
  type = c("MA", "RS", "week"),
  data
) {
  data <- data %>%
    mutate(
      newCasesBySpecimenDate = if_else(
        newCasesBySpecimenDate == 0,
        0.5,
        newCasesBySpecimenDate
      )
    )
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


# Fit BSTS for the supplied dated case/control series. Returns the requested report
# and/or raw state/predictive draws; raw outputs also carry their calendar dates.
# The reopening date ends the pre-period; post-period starts at day 1.
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
      raw_sample = raw_sample,
      dates = time.points[timecheck]
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
      raw_sample = raw_sample,
      dates = time.points[timecheck]
    )
  }

  if (get.table == T & raw.data != F) {
    res_list <- list(
      table = table,
      raw_mean = raw_mean,
      raw_sample = raw_sample,
      dates = time.points[timecheck]
    )
  }
  return(res_list)
}

# Fit one matched LTLA set with the existing BSTS settings.
# matched is one case/control match table; observations and openings are the
# dated outcome and HEI opening tables. Returns do.Causal report and dated draws.
# All inputs are explicit so workers need no enclosing analysis workspace.
fit.bsts.match <- function(matched, observations, openings) {
  do.Causal(
    data = prep.Causal(
      data = observations,
      case = get.info(data = matched, type = "case"),
      control = get.info(data = matched, type = "control")
    ),
    intervention_date = get.info(
      data = matched,
      open_data = openings,
      type = "open"
    ),
    ahead = 10,
    show.fig = FALSE,
    save.fig = FALSE,
    original = FALSE,
    get.table = TRUE,
    raw.data = "series",
    model.args = list(niter = 5000, nseasons = 7, season.duration = 1)
  )
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

# Historical optional arguments for the disabled plotting code below:
# region_num, # 新增：N_num → N_region 映射表，与 cal.cum.ci 中的 region_num 一致
# title = NULL,
# save = FALSE,
# path = NULL,
# width = NULL,
# height = NULL
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

# Build a daily influence kernel for offsets 0:decay.period after reopening.
# incubation is the offset with weight 1; decay.fun is evaluated at subsequent
# offsets x = 1, 2, ... . Returns zero before onset and has no support after the window.
gen.decay <- function(decay.period, decay.fun, incubation) {
  stopifnot(decay.period >= 0, incubation >= 0)
  weights <- numeric(decay.period + 1L)
  if (incubation <= decay.period) {
    weights[incubation + 1L] <- 1
    x <- seq_len(decay.period - incubation)
    if (length(x)) weights[incubation + 1L + x] <- eval(parse(text = decay.fun))
  }
  weights
}

# Apply a day-offset influence kernel to dated, same-day-summed opening pulses.
# dat is one LTLA frame with date and h_index; decay is the gen.decay vector.
# Returns the date-sorted frame with convolved h_index. Actual dates, rather than
# row offsets, ensure missing observations cannot shift the exposure window.
get.index <- function(dat, decay) {
  dat <- dat[order(dat$date), ]
  dates <- as.Date(dat$date)
  exposure <- numeric(nrow(dat))
  for (i in which(dat$h_index != 0)) {
    target <- match(dates[i] + seq_along(decay) - 1L, dates)
    keep <- !is.na(target)
    exposure[target[keep]] <- exposure[target[keep]] +
      dat$h_index[i] * decay[keep]
  }
  dat$h_index <- exposure
  dat
}


# Format the legacy fixed-effect table from CARBayesST model Dat.
# data supplies covariate SDs; unit selects the reporting scale and digits rounding.
# The student scale uses 5,000 proxy students / 40,485 at unit temporal weight,
# not measured returns. Returns the legacy rounded matrix; its centre exponentiates
# the coefficient mean (the historical column label is "50%").
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
    res_matrix[1, 1] <- round(exp(5000 / 40485 * Dat[2, 1]), digits)
    res_matrix[1, 2] <- round(exp(5000 / 40485 * Dat[2, 2]), digits)
    res_matrix[1, 3] <- round(exp(5000 / 40485 * Dat[2, 3]), digits)
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

# Write a named list of stage objects plus reproducibility metadata to an RDS file.
# dependencies are upstream file paths; hashes prevent mixing incompatible stages.
# Returns the output path invisibly; the previous file survives a failed write.
write.analysis <- function(objects, path, dependencies = character()) {
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  stopifnot(all(file.exists(dependencies)))
  code_files <- list.files("code", pattern = "\\.R$", full.names = TRUE)
  result <- list(
    version = "day1-D-v1",
    dependencies = tools::md5sum(dependencies),
    metadata = list(
      created = Sys.time(),
      session = sessionInfo(),
      code_md5 = tools::md5sum(code_files)
    ),
    objects = objects
  )
  temp <- tempfile(tmpdir = dirname(path))
  on.exit(unlink(temp), add = TRUE)
  saveRDS(result, temp)
  if (!file.rename(temp, path)) {
    stop("Could not replace result file: ", path)
  }
  invisible(path)
}

# Read a stage checkpoint and return its named object list.
# Rejects old formats or changed/missing upstream files; never starts a model.
read.analysis <- function(path) {
  result <- readRDS(path)
  if (!identical(result$version, "day1-D-v1")) {
    stop("Rebuild outdated checkpoint: ", path)
  }
  deps <- result$dependencies
  if (
    length(deps) &&
      (!all(file.exists(names(deps))) ||
        !identical(deps, tools::md5sum(names(deps))))
  ) {
    stop("Upstream inputs changed; rebuild checkpoint: ", path)
  }
  result$objects
}

# Fit a CAR model, save it immediately, and return a compact CARBayesST object.
# ... are ST.CARanova arguments; seed fixes this fit independently of other stages.
# input_file identifies the prepared checkpoint used for provenance validation.
# Parameter chains, diagnostics and fitted summaries are retained. Per-observation
# fitted draws are omitted unless england.keep_full_models requests a separate file.
fit.car <- function(
  result_name,
  seed,
  input_file = file.path(result_dir, "prepared.rds"),
  ...
) {
  set.seed(seed)
  args <- list(...)
  model <- do.call(ST.CARanova, args)
  settings <- args[setdiff(names(args), c("data", "W"))]
  settings$seed <- seed
  # Formula environments must not serialize the entire stage workspace.
  if (inherits(model$formula, "formula")) {
    environment(model$formula) <- baseenv()
  }
  if (inherits(settings$formula, "formula")) {
    environment(settings$formula) <- baseenv()
  }
  attr(model, "fit_settings") <- settings
  dependency <- input_file
  if (isTRUE(getOption("england.keep_full_models", FALSE))) {
    write.analysis(
      list(model = model),
      file.path(result_dir, paste0(result_name, "-full.rds")),
      dependency
    )
  }
  model$samples$fitted <- NULL
  write.analysis(
    list(model = model),
    file.path(result_dir, paste0(result_name, ".rds")),
    dependency
  )
  model
}

# Describe a linear covariate contrast without allocating observation-by-draw arrays.
# model is a CARBayesST fit; covariate names its beta column; baseline is the reference.
# Unnamed CARBayesST chains use the column order of the saved design matrix X.
# Returns only the covariate name, baseline and pooled coefficient samples.
car.effect <- function(model, covariate, baseline = 0) {
  chains <- model$samples$beta
  beta <- if (inherits(chains, "mcmc.list")) {
    do.call(rbind, chains)
  } else {
    as.matrix(chains)
  }
  if (is.null(colnames(beta))) {
    colnames(beta) <- colnames(model$X)
  }
  list(covariate = covariate, baseline = baseline, beta = beta[, covariate])
}

# Format a log-effect curve at days 3, 7, 14 and 30 as one wide RR row.
# curve contains date, cum.effect and lower/upper limits; unavailable days remain NA.
# Returns day3/day7/day14/day30 character columns, with no metadata columns.
effect.row <- function(curve) {
  days <- c(3L, 7L, 14L, 30L)
  cols <- c("cum.effect", "cum.effect.lower", "cum.effect.upper")
  values <- exp(as.matrix(curve[match(days, curve$date), cols, drop = FALSE]))
  labels <- ifelse(
    is.na(values[, 1]),
    NA_character_,
    sprintf("%.2f (%.2f, %.2f)", values[, 1], values[, 2], values[, 3])
  )
  as.data.frame(as.list(setNames(labels, paste0("day", days))))
}

# Align CAR covariates to actual reopening dates; return one small frame per LTLA.
# rawdata includes reopen_date and the requested covariate; region_data maps LTLA IDs.
# Day 0 is baseline, days 1:30 are follow-up. Missing dates end usable follow-up.
car.paths <- function(rawdata, region_data, effect) {
  dat <- rawdata %>%
    transmute(
      LTLA_ID,
      LTLA_name,
      date = as.Date(date),
      day = as.integer(as.Date(date) - as.Date(reopen_date)),
      exposure = .data[[effect$covariate]] - effect$baseline
    ) %>%
    filter(!is.na(day), day >= 1, day <= 30) %>%
    left_join(
      distinct(region_data, LTLA_ID, N_num, N_region),
      by = "LTLA_ID"
    ) %>%
    arrange(LTLA_ID, day)
  split(dat, dat$LTLA_ID)
}

# Compute one group's cumulative CAR curve without full effect matrices or workers.
# paths are car.paths frames; beta is a coefficient-draw vector. Returns days 0:30
# and n_ltla. At each horizon only complete LTLA trajectories contribute. The existing
# pooled daily quantiles followed by cumulative sums are deliberately retained.
car.curve <- function(paths, beta) {
  available <- vapply(
    paths,
    function(p) {
      ok <- match(1:30, p$day)
      sum(cumprod(!is.na(ok) & is.finite(p$exposure[ok])))
    },
    numeric(1)
  )
  curves <- vector("list", 31)
  previous <- integer()
  cumulative <- NULL
  for (day in 0:30) {
    eligible <- which(available >= day & available > 0)
    value <- rep(NA_real_, 3)
    if (day == 0) {
      value <- rep(0, 3)
    }
    if (day > 0 && length(eligible)) {
      # Reuse the daily quantiles until the complete-follow-up cohort changes.
      if (!identical(eligible, previous)) {
        daily <- vapply(
          seq_len(min(available[eligible])),
          function(d) {
            x <- vapply(
              paths[eligible],
              function(p) p$exposure[match(d, p$day)],
              numeric(1)
            )
            quantile(
              as.vector(outer(x, beta)),
              c(0.5, 0.025, 0.975),
              names = FALSE
            )
          },
          numeric(3)
        )
        cumulative <- daily
        if (ncol(daily) > 1) {
          for (d in 2:ncol(daily)) {
            cumulative[, d] <- cumulative[, d - 1] + daily[, d]
          }
        }
        previous <- eligible
      }
      value <- cumulative[, day]
    }
    curves[[day + 1L]] <- data.frame(
      date = day,
      cum.effect = value[1],
      cum.effect.lower = value[2],
      cum.effect.upper = value[3],
      n_ltla = length(eligible)
    )
  }
  bind_rows(curves)
}

# Return LTLA-specific RR summaries for compact CAR effects.
# Inputs are the model data, region lookup and car.effect descriptor; unavailable
# horizons are NA. Output columns are LTLA_ID, LTLA_name and day3/day7/day14/day30.
cal.cum.bs.LTLA <- function(rawdata, region_data, eff_data) {
  paths <- car.paths(rawdata, region_data, eff_data)
  bind_rows(lapply(paths, function(p) {
    bind_cols(
      p[1, c("LTLA_ID", "LTLA_name")],
      effect.row(car.curve(list(p), eff_data$beta))
    )
  }))
}

# Return region/global CAR summaries, plot curves and per-horizon denominators.
# eff_data is a car.effect descriptor (not a dense matrix). Region pooling is unchanged;
# date is days since actual reopening, and each horizon uses complete follow-up only.
cal.cum.bs <- function(rawdata, region_data, eff_data) {
  paths <- car.paths(rawdata, region_data, eff_data)
  regions <- region_data %>% distinct(N_num, N_region) %>% arrange(N_num)
  regions <- bind_rows(data.frame(N_num = 0, N_region = "All region"), regions)
  curves <- lapply(seq_len(nrow(regions)), function(i) {
    selected <- if (regions$N_num[i] == 0) {
      paths
    } else {
      Filter(
        function(p) !is.na(p$N_num[1]) && p$N_num[1] == regions$N_num[i],
        paths
      )
    }
    bind_cols(regions[rep(i, 31), ], car.curve(selected, eff_data$beta))
  })
  summary <- bind_rows(lapply(seq_along(curves), function(i) {
    bind_cols(regions[i, ], effect.row(curves[[i]]))
  }))
  plot_data <- bind_rows(curves) %>% relocate(date, N_num, N_region)
  list(
    summary_table = summary,
    raw_plot_data = plot_data,
    counts = dplyr::select(plot_data, N_num, N_region, date, n_ltla)
  )
}

# Build per-LTLA BSTS paths from do.Causal results, matched tables and observations.
# Results must carry calendar dates. Return only days 1:30, state draws, cumulative
# predictive draws and observed responses; no missing follow-up is imputed.
bsts.paths <- function(data, m.data, table) {
  lapply(seq_along(data), function(i) {
    case <- m.data[[i]] %>% filter(type == 1) %>% slice(1) %>% ungroup()
    days <- as.integer(as.Date(data[[i]]$dates) - as.Date(table$int_date[i]))
    columns <- which(days >= 1 & days <= 30)
    dates <- as.Date(data[[i]]$dates[columns])
    observed <- LTLA_gr %>% filter(LTLA_ID == case$LTLA_ID[1]) %>% arrange(date)
    y <- observed$newCasesBySpecimenDate[match(dates, as.Date(observed$date))]
    samples <- as.matrix(data[[i]]$raw_sample)[, columns, drop = FALSE]
    states <- as.matrix(data[[i]]$raw_mean)[, columns, drop = FALSE]
    # Limit to the consecutive, fully observed prefix of days 1:30.
    valid <- days[columns] == seq_along(columns) &
      is.finite(y) &
      colSums(!is.finite(samples)) == 0 &
      colSums(!is.finite(states)) == 0
    n <- sum(cumprod(valid))
    samples <- samples[, seq_len(n), drop = FALSE]
    cumulative <- samples
    if (n > 1) {
      for (d in 2:n) {
        cumulative[, d] <- cumulative[, d - 1] + samples[, d]
      }
    }
    list(
      LTLA_ID = case$LTLA_ID[1],
      LTLA_name = case$LTLA_name[1],
      N_num = case$N_num[1],
      observed = y[seq_len(n)],
      states = states[, seq_len(n), drop = FALSE],
      cumulative = cumulative,
      n = n
    )
  })
}

# Compute pooled BSTS cumulative effects for days 0:30 and report n_ltla.
# paths come from bsts.paths. At each horizon, pool only complete LTLA trajectories.
# Retains the existing medians of observations/states and pooled predictive intervals;
# this is not a newly defined posterior of a regional average effect.
bsts.curve <- function(paths) {
  available <- vapply(paths, function(p) p$n, numeric(1))
  curves <- vector("list", 31)
  previous <- integer()
  for (day in 0:30) {
    eligible <- which(available >= day & available > 0)
    value <- rep(NA_real_, 3)
    if (day == 0) {
      value <- rep(0, 3)
    }
    if (day > 0 && length(eligible)) {
      if (!identical(eligible, previous)) {
        selected <- paths[eligible]
        days <- seq_len(min(available[eligible]))
        observed <- cumsum(vapply(
          days,
          function(d) {
            median(vapply(selected, function(p) p$observed[d], numeric(1)))
          },
          numeric(1)
        ))
        predicted <- cumsum(vapply(
          days,
          function(d) {
            median(unlist(
              lapply(selected, function(p) p$states[, d]),
              use.names = FALSE
            ))
          },
          numeric(1)
        ))
        previous <- eligible
      }
      limits <- quantile(
        unlist(
          lapply(selected, function(p) p$cumulative[, day]),
          use.names = FALSE
        ),
        c(0.025, 0.975),
        names = FALSE
      )
      value <- c(
        observed[day] - predicted[day],
        observed[day] - limits[2],
        observed[day] - limits[1]
      )
    }
    curves[[day + 1L]] <- data.frame(
      date = day,
      cum.effect = value[1],
      cum.effect.lower = value[2],
      cum.effect.upper = value[3],
      n_ltla = length(eligible)
    )
  }
  bind_rows(curves)
}

# Return regional/global BSTS plot curves on calendar-aligned days 0:30.
# data/m.data/table are matching do.Causal, match and report outputs; select.length
# remains a compatibility argument (41 = ten pre-days + day 0 + thirty post-days).
cal.cum.ci.region <- function(data, m.data, table, select.length = 41) {
  paths <- bsts.paths(data, m.data, table)
  regions <- table %>%
    dplyr::select(N_num) %>%
    distinct() %>%
    left_join(region_num, by = "N_num") %>%
    arrange(N_num)
  regions <- bind_rows(data.frame(N_num = 0, N_region = "All region"), regions)
  bind_rows(lapply(seq_len(nrow(regions)), function(i) {
    selected <- if (regions$N_num[i] == 0) {
      paths
    } else {
      Filter(function(p) p$N_num == regions$N_num[i], paths)
    }
    bind_cols(regions[rep(i, 31), ], bsts.curve(selected))
  })) %>%
    relocate(date, N_num, N_region)
}

# Return BSTS regional RR summaries, or regional curves when tidy.out is FALSE.
# Inputs match cal.cum.ci.region; unavailable 3/7/14/30-day horizons remain NA.
cal.cum.ci <- function(
  data,
  m.data,
  table,
  select.length = 41,
  tidy.out = FALSE
) {
  curves <- cal.cum.ci.region(data, m.data, table, select.length)
  groups <- split(curves, curves$N_num)
  if (!tidy.out) {
    return(groups[names(groups) != "0"])
  }
  bind_rows(lapply(groups, function(p) {
    bind_cols(p[1, c("N_num", "N_region")], effect.row(p))
  }))
}

# Return global and LTLA-specific BSTS RR summaries (or curves when tidy.out is FALSE).
# Inputs match bsts.paths; the legacy N_region column carries the LTLA name.
# All incomplete horizons remain NA, including LTLAs with fewer than three post-days.
cal.cum.ci.LTLA <- function(
  data,
  m.data,
  table,
  select.length = 41,
  tidy.out = FALSE
) {
  # trim_cols <- function(df) df[, seq_len(select.length), drop = FALSE]
  paths <- bsts.paths(data, m.data, table)
  curves <- lapply(paths, function(p) bsts.curve(list(p)))
  if (!tidy.out) {
    return(curves)
  }
  global <- bind_cols(
    data.frame(N_num = 0, N_region = "All region"),
    effect.row(bsts.curve(paths))
  )
  per_ltla <- bind_rows(lapply(seq_along(paths), function(i) {
    bind_cols(
      data.frame(N_num = paths[[i]]$N_num, N_region = paths[[i]]$LTLA_name),
      effect.row(curves[[i]])
    )
  }))
  bind_rows(global, per_ltla)
}

# Keep whole areas with finite outcomes/covariates on the full study calendar.
# Input: dated panel and numeric column names. Output: date-major balanced panel.
complete.panel <- function(data, variables) {
  ids <- data %>%
    summarise(
      ok = n_distinct(date) == 188L &&
        n() == 188L &&
        all(is.finite(as.matrix(pick(all_of(variables))))),
      .by = LTLA_ID
    ) %>%
    filter(ok)
  data %>% filter(LTLA_ID %in% ids$LTLA_ID) %>% arrange(date, LTLA_ID)
}

# Check whether a saved model still matches its recorded input files.
# Input: checkpoint path. Output: TRUE for a reusable model, otherwise FALSE.
# Used only in explicit fitting stages; summaries never refit stale models.
analysis.cached <- function(path) {
  if (!file.exists(path)) {
    return(FALSE)
  }
  tryCatch(
    {
      read.analysis(path)
      TRUE
    },
    error = function(e) {
      message("Refitting checkpoint: ", path, " (", conditionMessage(e), ")")
      FALSE
    }
  )
}

# Fit or reuse one named CAR specification on its prepared balanced panel.
# Inputs: a one-row spec with model/outcome/hall/mobility/calendar columns,
# date-major data, named spatial matrix, seed, prepared path and core count.
# All covariates must be observed. The stage result_dir must be dirname(input_file).
# Saves the usual compact stage checkpoint; returns NULL invisibly.
fit.car.specification <- function(spec, data, W, seed, input_file, n.cores) {
  path <- file.path(dirname(input_file), paste0("car-", spec$model, ".rds"))
  if (analysis.cached(path)) {
    return(invisible(NULL))
  }
  areas <- data$LTLA_name[data$date == min(data$date)]
  formula <- reformulate(
    c(
      spec$hall,
      spec$mobility[spec$mobility != ""],
      "Pop_Den_km2",
      "Prosperity",
      "week",
      spec$calendar
    ),
    response = spec$outcome
  )
  message("CAR ", spec$model, ": ", nrow(data), " rows")
  model <- fit.car(
    result_name = paste0("car-", spec$model),
    seed = seed,
    input_file = input_file,
    formula = formula,
    family = "gaussian",
    data = data,
    W = W[areas, areas],
    burnin = 10000,
    n.sample = 110000,
    thin = 100,
    n.chains = 4,
    n.cores = n.cores,
    interaction = TRUE,
    verbose = FALSE
  )
  rm(model)
  gc()
  invisible(NULL)
}
