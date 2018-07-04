library(hgutils)
startup()
load_package_collection("processing")
files = list.files("R","\\.[Rr]$", recursive = TRUE, full.names = TRUE)
source_code = lapply(files, function(x) paste0(readLines(x), collapse = "\n")) %>% set_names(str_match(files, "/(.*?\\.R)$")[,-1])

functions = lapply(names(source_code), function(file_name) {
  code_pattern = "((?:#'.*?\n)+)((?:(?!#').*\n)*(?!#').*\\})"
  file_content = source_code[file_name][[1]]
  if(str_detect(file_content, code_pattern)) {
    str_match_all(file_content, code_pattern)[[1]][,-1] %>%
    {cbind("function_name" = str_match(.[,2], "((?:[[:alpha:]]|\\.(?![0-9]))[[:alnum:]_\\.]*)[ ]*=[ ]*function")[,-1],
           "file_name" = file_name, "documentation"=.[,1], "source"=.[,2])} %>%
    apply(1, as.list)
  } else list()
}) %>% set_names(names(source_code))

single_list = unlist(functions, recursive=FALSE) %>% set_names(., sapply(., function(x) x$function_name))
#set spaces after comma's
for (fname in names(source_code)) {
  A = str_replace_all(source_code[[fname]],",(?! )",", ") %>% #put space behind comma
      str_replace_all("#(?![' ])","# ") %>% #put space behind hastag
      str_replace_all("=(?![ =])","= ") %>% #put space behind equals sign
      str_replace_all("(?<![ \\[=])="," =") #put space before equals sign !!!!!!!!!!!!!!!\link[= survfit]

  before = str_match_all(A, "(\\([^\\)\\(]*? = [^\\(\\)]*?\\))")[[1]][,2]
  after = str_replace_all(before, " = ", "=")
  B = A %>% str_replace_all(fixed(before), fixed(after))

      str_split("\n")
  writeClipboard(B[[1]])

  parsed=getParseData(parse(text=single_list$load_packages))
  used_funcs = parsed$text[parsed$token %in% c("SYMBOL_FUNCTION_CALL","SPECIAL")] %>% unique %>% setdiff(names(single_list)) %>% sort
  lsf.str(pos=which(search()=="package:hgutils"))
}

files = list.files("tools/exercise/","\\.[Rr]$", recursive = TRUE, full.names = TRUE)
source_code = lapply(files, function(x) paste0(readLines(x), collapse = "\n")) %>% set_names(str_match(files, ".*/(.*?\\.R)$")[,-1])
sum(sapply(A, function(x) str_count(x,",(?! )")))
source_code %>% lapply(. %>% {str_split(source_code[[1]],"\n") %>%
                sapply(. %>% str_replace_all("(\\G(?:(?!#).)*?),(?! )","\\1, ")) %>%
                sapply(. %>% str_replace_all("(\\G(?:(?!#).)*?) (?= )","\\1")) %>%
                paste0(collapse = "\n")}) %>% .[[1]] %>% cat

# %>% as.data.frame(stringsAsFactors=FALSE) %>% set_names("line")
line = A$line[7]
B=str_replace_all(A[[1]],"(\\G(?:(?!#).)*?)(,(?! ))","\\1, ")
regmatches(line, gregexpr("\\G(?:(?!#).)*?(,(?! ))",line,perl = TRUE))
#hadley styles
#check if imports occur in code
# check if code occurs in imports

fnames="tools/exercise/ex.R"
fix_comments=FALSE
source_code = lapply(fnames, function(x) readLines(x)) %>% set_names(str_match(fnames,"([^/]*\\.R)$")[,-1])

n_total = sapply(source_code, function(file) sapply(file, function(line) ifelse(fix_comments,
                                                                                str_count(line, ","),
                                                                                str_count(line, "(\\G(?:(?!#).)*?),")))) %>% sum
n_to_fix = sapply(source_code, function(file) sapply(file, function(line) ifelse(fix_comments,
                                                                                 str_count(line, ",(?! )"),
                                                                                 str_count(line, "(\\G(?:(?!#).)*?),(?! )")))) %>% sum


source_code %>% lapply(. %>% {str_split(source_code[[1]],"\n") %>%
    sapply(. %>% str_replace_all("(\\G(?:(?!#).)*?),(?! )","\\1, ")) %>% #add space after comma
    sapply(. %>% str_replace_all("(\\G(?:(?!#).)*?) (?= )","\\1")) %>% #remove space before comma
    sapply(. %>% str_replace_all("(\\G(?:(?!#).)*?)=(?! )","\\1= ")) %>% #add space after equals sign
    sapply(. %>% str_replace_all("(\\G(?:(?!#).)*?)(?<! )=","\\1 =")) %>% #add space before equals sign
    sapply(. %>% str_replace_all("(\\G(?:(?!#).)*?)\\(=","\\1=")) %>% #remove spaces when between parentheses
    paste0(collapse = "\n")}) %>% .[[1]] %>% cat

