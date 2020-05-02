create_features_for_data <- function(data, column_names, pack=1) {
  operands_list <- create_default_operands_list(pack)
  return(create_features(data, operands_list, column_names))
}


create_features <- function(data, operands_list, column_names){
  for (stack_of_operands in operands_list) {
    expressions <- create_expression("", column_names, stack_of_operands)
    for (expr in expressions) {
      data[[expr]] <- eval(parse(text=expr))
    }
  }
  return(substitute_names(data))
}

create_expression <- function(previous_expression, column_names, stack_of_operands) {
  expressions <- c()
  if (length(stack_of_operands)==1) {
    return(paste0(previous_expression, stack_of_operands[1]))
  }
  else {
    actual_expression <- paste0(previous_expression, stack_of_operands[1], "data$")
    for (i in 1:length(column_names)) {
      expressions <- c(expressions, create_expression(paste0(actual_expression, column_names[i]), column_names[-i], stack_of_operands[-1]))
    }
    return(expressions)
  }
}

create_default_operands_list <- function(pack=1) {
  operands_list <- list()
  op1 <- c("*", "/")
  op2 <- c("+", "-")
  op3 <- c(op1, op2)
  if (pack==1) {
    operands_list <- list(c("sqrt(", ")"))
    for (i in op3) {
      operands_list <- add_operands(operands_list, c("", i, ""))
      operands_list <- add_operands(operands_list, c("(1/", paste0(")",i,"(1/"), ")"))
      operands_list <- add_operands(operands_list, c("(", paste0(")",i,"(1/"), ")"))
      operands_list <- add_operands(operands_list, c("sqrt(", paste0(")",i,"sqrt("), ")"))
      operands_list <- add_operands(operands_list, c("sqrt(", paste0(")",i,"("), ")"))
      operands_list <- add_operands(operands_list, c("sqrt(", paste0(")",i,"(1/"), ")"))
    }
  }
  if (pack==2) {
    for (i in op2) {
      for (j in op1) {
        operands_list <- add_operands(operands_list, c("(", i, paste0(")",j,"("), op2[1], ")"))
      }
    }
  }
  if (pack==3) {
    for (i in op2) {
      for (j in op1) {
        operands_list <- add_operands(operands_list, c("(", i, paste0(")",j,"("), op2[2], ")"))
      }
    }
  }
  if (pack==4) {
    for (i in op3) {
      for (j in op3) {
        operands_list <- add_operands(operands_list, c("(", paste0(")",i,"("), paste0(")",j,"("), ")"))
        operands_list <- add_operands(operands_list, c("(1/", paste0(")",i,"(1/"), paste0(")",j,"(1/"), ")"))
      }
    }
  }
  if (pack==5) {
    for (i in op3) {
      for (j in op3) {
        operands_list <- add_operands(operands_list, c("(1/", paste0(")",i,"(1/"), paste0(")",j,"("), ")"))
        operands_list <- add_operands(operands_list, c("(1/", paste0(")",i,"("), paste0(")",j,"("), ")"))
      }
    }
  }
  if (pack==6) {
    for (i in op3) {
      for (j in op3) {
        operands_list <- add_operands(operands_list, c("(1/", paste0(")",i,"("), paste0(")",j,"(1/"), ")"))
        operands_list <- add_operands(operands_list, c("sqrt(", paste0(")",i,"(1/"), paste0(")",j,"("), ")"))
      }
    }
  }
  if (pack==7) {
    for (i in op3) {
      for (j in op3) {
        operands_list <- add_operands(operands_list, c("(1/", paste0(")",i,"sqrt("), paste0(")",j,"(1/"), ")"))
        operands_list <- add_operands(operands_list, c("sqrt(", paste0(")",i,"("), paste0(")",j,"(1/"), ")"))
      }
    }
  }
  return(operands_list)
}

add_operands <- function(operand_list, operand) {
  return(c(operand_list, list(operand)))
}

# functions usage:
# see how pack=i operands list look like
#operands_list <- create_default_operands_list(pack=2)

# add columns from operands list pack=i and see some information
#data_new <- create_features_for_data(data, pack=5)
#ncol(data_new)
#nrow(data_new)
#data_new[1:20, 1:120]


substitute_names <- function(data) {
  colnames(data) <- stri_replace_all_fixed(colnames(data), "data$", "")
  colnames(data) <- stri_replace_all_fixed(colnames(data), "/", "_over_")
  colnames(data) <- stri_replace_all_fixed(colnames(data), "+", "_plus_")
  colnames(data) <- stri_replace_all_fixed(colnames(data), "-", "_minus_")
  colnames(data) <- stri_replace_all_fixed(colnames(data), "*", "_times_")
  colnames(data) <- stri_replace_all_fixed(colnames(data), "(", "_lp_")
  colnames(data) <- stri_replace_all_fixed(colnames(data), ")", "_rp_")
  colnames(data) <- stri_replace_all_regex(colnames(data), "^_", "")
  colnames(data) <- stri_replace_all_regex(colnames(data), "_$", "")
  data
}