#' Patient flowchart
#'
#' @description Creates a patient flowchart which visualizes exclusions and updates the dataset.
#'
#' @param dataset The dataset, must be a data.frame.
#' @param node_text The text of the starting node, must be a string which can be interpreted by sprintf.
#' @param stratum An optional stratum, must be variable in dataset.
#' @param flowchart The flowchart object.
#' @param exclusion_criterium A boolean statement which is used to select patients to be discarded from the dataset.
#' @param reason An optional string to specify why patients were excluded. Defaults to the exclusion criterium.
#' @param excluded_text The text of the exclusion node, must be a string which can be interpreted by \code{\link[base]{sprintf}}.
#'
#' @note When excluding patients, the flowchart is updated 'behind the scenes' and is not returned.
#' @return A flowchart (when creating the flowchart), or updated dataset (when excluding patients).
#' @export
#'
#' @examples \dontrun{
#' dataset = survival::lung; dataset$sex = factor(dataset$sex,labels=c("male","female"))
#' flowchart = inclusion_flowchart(dataset)
#' dataset = exclude_patients(flowchart, dataset, status==1) #exclude all patients who did not die
#' dataset = exclude_patients(flowchart, dataset, time<100) #exclude patients with a short follow-up
#' flowchart #print diagram
#' }
inclusion_flowchart = function(dataset, node_text="%s eligable patients", stratum=NULL) {
  if(class(dataset) != "data.frame") {
    stop(sprintf("Argument 'dataset' must be a data.frame but is of type %s.", frmt(class(dataset))))
  }

  N = if(!is.null(stratum)) {
    if(!stratum %in% names(dataset) || length(stratum) > 1) {
      stop(sprintf("Argument 'stratum' must contain a single variable in 'dataset', but %s could not be found.", frmt(stratum)))
    }

    strat = table(dataset[[stratum]])
    paste0(names(strat), " (n=",strat,")",collapse = ", ")
  } else {
    paste0("N = ",nrow(dataset))
  }

  node = list(sprintf(node_text, N)); attr(node,"type") = "patient_flowchart_node"
  flowchart = list(node); class(flowchart) = "patient_flowchart"
  attr(flowchart,"stratum") = stratum
  flowchart
}

#' @rdname inclusion_flowchart
#' @export
exclude_patients = function(flowchart, dataset, exclusion_criterium, reason=deparse(substitute(exclusion_criterium)), node_text="%s eligable patients",
                            excluded_text="%s excluded") {
  if(!"patient_flowchart" %in% class(flowchart)) {
    stop(sprintf("Argument 'flowchart' must be a patient_flowchart but is of type %s.", frmt(class(flowchart))))
  }

  fc = flowchart
  stratum = attr(fc,"stratum")
  ind = eval(substitute(exclusion_criterium), dataset)
  N = if(!is.null(stratum)) {
    if(!stratum %in% names(dataset) || length(stratum) > 1) {
      stop(sprintf("Argument 'stratum' must contain a single variable in 'dataset', but %s could not be found.", frmt(stratum)))
    }

    strat = list(table(dataset[ind,stratum]), table(dataset[!ind,stratum]))
    list(paste0(names(strat[[1]]), " (n=",strat[[1]],")",collapse = ", "),
         paste0(names(strat[[2]]), " (n=",strat[[2]],")",collapse = ", "))
  } else {
    list(paste0("N = ",nrow(dataset[ind,])), paste0("N = ",nrow(dataset[!ind,])))
  }

  node1 = list(sprintf(paste0(excluded_text," [",reason,"]"), N[[1]])); attr(node1,"type") = "patient_flowchart_exclude"
  node2 = list(sprintf(node_text, N[[2]])); attr(node2,"type") = "patient_flowchart_node"

  fc[[length(fc)+1]] = node1
  fc[[length(fc)+1]] = node2
  class(fc) = "patient_flowchart"
  attr(fc,"stratum") = stratum

  eval.parent(substitute(flowchart<-fc))
  dataset[!ind,]
}


#' Print the patient inclusion flowchart
#'
#' @inheritParams base::print
#' @param length Length of the arrows (to the right)
#'
#' @export
#' @return NULL
print.patient_flowchart = function(x, length=7, ...) {
  cat(as.character.patient_flowchart(x))
}

#' Text representation of patient inclusion flowchart
#'
#' @inheritParams base::as.character
#' @param length Length of the arrows (to the right)
#'
#' @export
#' @return NULL
as.character.patient_flowchart = function(x, length=7, ...) {
  res = ""
  for(i in 1:length(x)) {
    element = x[[i]]

    if (attr(element,"type") == "patient_flowchart_node") {
      res = paste0(res, str_wrap(element[[1]], width=80),"\n")
    } else {
      txt = str_wrap(element[[1]], width=80-length, indent=0, exdent = 0) %>% str_split("\n") %>% .[[1]]
      middle = floor(length(txt)/2)+1

      acc = paste0(paste0(" |",paste0(rep(" ", length), collapse = ""), collapse = ""),
                   txt[seq(from=1,length.out = middle-1)],collapse = "\n")
      acc %<>% paste0("\n |",paste0(rep("-",length-2),collapse = ""),"> ",txt[middle],collapse = "")
      acc %<>% paste0("\n", paste0(" |",paste0(rep(" ",length),collapse = ""),
                                   txt[seq(from=middle+1,length.out = length(txt)-middle)], collapse = "\n"), collapse = "")

      res = paste0(res, acc,"\n V\n")
    }
  }
  res
}

