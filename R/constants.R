CLASS_MATRIX = "polyMatrix"
CLASS_MARRAY = "polyMarray"
CLASS_MBOARD = "polyMbroad"
CLASS_MCELSS = "polyMcells"
CLASS_MDLIST = "polyMdlist"
CLASS_LIST = "list"
ALL_CLASSES = list(CLASS_MARRAY, CLASS_MBOARD, CLASS_MCELSS, CLASS_MDLIST)

OPERATION_TYPE_TOTAL = "total"
OPERAIION_TYPE_COLUMN = "col"
OPERATION_TYPE_ROW = "row"
OPERAIION_TYPES = list(OPERATION_TYPE_TOTAL, OPERAIION_TYPE_COLUMN, OPERATION_TYPE_ROW)


check_class <- function(class_name) {
  if (all(class_name != ALL_CLASSES)) {
    stop("Unexcpected class name")
  }
}
