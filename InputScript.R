# Install necessary packages if not already installed
required_packages <- c("httr", "rvest", "stringr", "urltools", "openssl", "whois", "lubridate")

for (pkg in required_packages) {
  if (!require(pkg, character.only = TRUE)) {
    install.packages(pkg, dependencies = TRUE)
    library(pkg, character.only = TRUE)
  }
}

# Load necessary libraries
library(httr)
library(rvest)
library(stringr)
library(urltools)
library(openssl)
library(whois)
library(lubridate)

url_having_ip <- function(url) {
  return(0)
}

url_length <- function(url) {
  length <- nchar(url)
  if (length < 54) {
    return(-1)
  } else if (length <= 75) {
    return(0)
  } else {
    return(1)
  }
}

url_short <- function(url) {
  return(0)
}

having_at_symbol <- function(url) {
  if (str_detect(url, "@")) {
    return(1)
  } else {
    return(-1)
  }
}

doubleSlash <- function(url) {
  return(0)
}

prefix_suffix <- function(url) {
  parsed_url <- url_parse(url)
  if (str_detect(parsed_url$domain, "-")) {
    return(1)
  } else {
    return(-1)
  }
}

sub_domain <- function(url) {
  parsed_url <- url_parse(url)
  subdomains <- str_split(parsed_url$subdomain, "\\.")[[1]]
  if (length(subdomains) == 0) {
    return(-1)
  } else if (length(subdomains) == 1) {
    return(0)
  } else {
    return(1)
  }
}

SSLfinal_State <- function(url) {
  tryCatch({
    if (str_detect(url, "^https")) {
      usehttps <- 1
    } else {
      usehttps <- 0
    }
    parsed_url <- url_parse(url)
    host_name <- paste(parsed_url$domain, parsed_url$suffix, sep = ".")
    cert <- openssl::download_ssl_cert(host_name)
    certificate_Auth <- cert$issuer[[1]]$commonName
    trusted_Auth <- c('Comodo', 'Symantec', 'GoDaddy', 'GlobalSign', 'DigiCert', 'StartCom', 'Entrust', 'Verizon', 'Trustwave', 'Unizeto', 'Buypass', 'QuoVadis', 'Deutsche Telekom', 'Network Solutions', 'SwissSign', 'IdenTrust', 'Secom', 'TWCA', 'GeoTrust', 'Thawte', 'Doster', 'VeriSign')
    startingDate <- as.Date(cert$not_before)
    endingDate <- as.Date(cert$not_after)
    Age_of_certificate <- as.numeric(difftime(endingDate, startingDate, units = "days")) / 365
    
    if (usehttps == 1 && certificate_Auth %in% trusted_Auth && Age_of_certificate >= 1) {
      return(-1)
    } else if (usehttps == 1 && !certificate_Auth %in% trusted_Auth) {
      return(0)
    } else {
      return(1)
    }
  }, error = function(e) {
    return(1)
  })
}

domain_registration <- function(url) {
  tryCatch({
    w <- whois::whois(url)
    updated <- w$updated_date
    exp <- w$expiration_date
    length <- as.numeric(difftime(exp, updated, units = "days"))
    if (length <= 365) {
      return(1)
    } else {
      return(-1)
    }
  }, error = function(e) {
    return(0)
  })
}

favicon <- function(url) {
  return(0)
}

port <- function(url) {
  return(0)
}

https_token <- function(url) {
  parsed_url <- url_parse(url)
  host <- paste(parsed_url$subdomain, parsed_url$domain, parsed_url$suffix, sep = ".")
  if (str_detect(host, "https")) {
    return(1)
  } else {
    return(-1)
  }
}

request_url <- function(url) {
  tryCatch({
    parsed_url <- url_parse(url)
    websiteDomain <- parsed_url$domain
    webpage <- read_html(url)
    imgs <- html_nodes(webpage, "img")
    total <- length(imgs)
    linked_to_same <- sum(sapply(html_attr(imgs, "src"), function(src) {
      parsed_img <- url_parse(src)
      imgDomain <- parsed_img$domain
      return(ifelse(websiteDomain == imgDomain || imgDomain == "", 1, 0))
    }))
    vids <- html_nodes(webpage, "video")
    total <- total + length(vids)
    linked_to_same <- linked_to_same + sum(sapply(html_attr(vids, "src"), function(src) {
      parsed_vid <- url_parse(src)
      vidDomain <- parsed_vid$domain
      return(ifelse(websiteDomain == vidDomain || vidDomain == "", 1, 0))
    }))
    linked_outside <- total - linked_to_same
    if (total != 0) {
      avg <- linked_outside / total
    } else {
      avg <- 0
    }
    
    if (avg < 0.22) {
      return(-1)
    } else if (avg <= 0.61) {
      return(0)
    } else {
      return(1)
    }
  }, error = function(e) {
    return(0)
  })
}

url_of_anchor <- function(url) {
  tryCatch({
    parsed_url <- url_parse(url)
    websiteDomain <- parsed_url$domain
    webpage <- read_html(url)
    anchors <- html_nodes(webpage, "a")
    total <- length(anchors)
    linked_to_same <- sum(sapply(html_attr(anchors, "href"), function(href) {
      parsed_anchor <- url_parse(href)
      anchorDomain <- parsed_anchor$domain
      return(ifelse(websiteDomain == anchorDomain || anchorDomain == "", 1, 0))
    }))
    linked_outside <- total - linked_to_same
    if (total != 0) {
      avg <- linked_outside / total
    } else {
      avg <- 0
    }
    
    if (avg < 0.31) {
      return(-1)
    } else if (avg <= 0.67) {
      return(0)
    } else {
      return(1)
    }
  }, error = function(e) {
    return(0)
  })
}

Links_in_tags <- function(url) {
  tryCatch({
    webpage <- read_html(url)
    no_of_meta <- length(html_nodes(webpage, "meta"))
    no_of_link <- length(html_nodes(webpage, "link"))
    no_of_script <- length(html_nodes(webpage, "script"))
    anchors <- length(html_nodes(webpage, "a"))
    total <- no_of_meta + no_of_link + no_of_script + anchors
    tags <- no_of_meta + no_of_link + no_of_script
    if (total != 0) {
      avg <- tags / total
    } else {
      avg <- 0
    }
    
    if (avg < 0.25) {
      return(-1)
    } else if (avg <= 0.81) {
      return(0)
    } else {
      return(1)
    }
  }, error = function(e) {
    return(0)
  })
}

sfh <- function(url) {
  return(0)
}

email_submit <- function(url) {
  tryCatch({
    webpage <- read_html(url)
    if (length(html_nodes(webpage, xpath = '//*[@href^="mailto:"]')) > 0) {
      return(1)
    } else {
      return(-1)
    }
  }, error = function(e) {
    return(0)
  })
}

abnormal_url <- function(url) {
  return(0)
}

redirect <- function(url) {
  return(0)
}

on_mouseover <- function(url) {
  return(0)
}

rightClick <- function(url) {
  return(0)
}

popup <- function(url) {
  return(0)
}

iframe <- function(url) {
  return(0)
}

age_of_domain <- function(url) {
  tryCatch({
    w <- whois::whois(url)
    start_date <- w$creation_date
    current_date <- Sys.Date()
    age <- as.numeric(difftime(current_date, start_date, units = "days"))
    if (age >= 180) {
      return(-1)
    } else {
      return(1)
    }
  }, error = function(e) {
    return(0)
  })
}

dns <- function(url) {
  return(0)
}

web_traffic <- function(url) {
  return(0)
}

page_rank <- function(url) {
  return(0)
}

google_index <- function(url) {
  return(0)
}

links_pointing <- function(url) {
  return(0)
}

statistical <- function(url) {
  return(0)
}

process <- function(url) {
  check <- data.frame(
    url_having_ip(url),
    url_length(url),
    url_short(url),
    having_at_symbol(url),
    doubleSlash(url),
    prefix_suffix(url),
    sub_domain(url),
    SSLfinal_State(url),
    domain_registration(url),
    favicon(url),
    port(url),
    https_token(url),
    request_url(url),
    url_of_anchor(url),
    Links_in_tags(url),
    sfh(url),
    email_submit(url),
    abnormal_url(url),
    redirect(url),
    on_mouseover(url),
    rightClick(url),
    popup(url),
    iframe(url),
    age_of_domain(url),
    dns(url),
    web_traffic(url),
    page_rank(url),
    google_index(url),
    links_pointing(url),
    statistical(url)
  )
  
  print(check)
  return(check)
}

# Example usage
process("C:\\Users\\cntej\\Downloads\\phising\\phising\\data.csv")
