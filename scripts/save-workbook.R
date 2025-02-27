# FUNCTION TO MAKE SPREADSHEET OF DIFFERENT TABS --------------------------
save_as_spreadsheet_multiformat <- function(number_of_tabs,
                                            tab1_data,
                                            tab1_name,
                                            tab2_data,
                                            tab2_name,
                                            tab3_data,
                                            tab3_name,
                                            tab4_data,
                                            tab4_name,
                                            tab5_data,
                                            tab5_name,
                                            tab6_data,
                                            tab6_name,
                                            tab7_data,
                                            tab7_name,
                                            tab8_data,
                                            tab8_name,
                                            xlsx_path,
                                            alternative_xlsx_path = 0, # optional
                                            number_cols_1,
                                            percent_cols_1,
                                            currency_cols_1 = 0,
                                            number_cols_2,
                                            percent_cols_2 = 0,
                                            currency_cols_2 = 0,
                                            number_cols_3,
                                            percent_cols_3 = 0,
                                            currency_cols_3 = 0,
                                            number_cols_4,
                                            percent_cols_4 = 0,
                                            currency_cols_4 = 0,
                                            number_cols_5,
                                            percent_cols_5 = 0,
                                            currency_cols_5 = 0,
                                            number_cols_6,
                                            percent_cols_6 = 0,
                                            currency_cols_6 = 0,
                                            number_cols_7,
                                            percent_cols_7 = 0,
                                            currency_cols_7 = 0,
                                            number_cols_8,
                                            percent_cols_8 = 0,
                                            currency_cols_8 = 0,
                                            colwidths = 20,
                                            number_decimal = FALSE,
                                            percent_decimal = FALSE) {
  
  # create work book
  wb <- createWorkbook("workbook")
  
  # formatting
  #style for header
  headerStyle <- createStyle(
    fontSize = 10, fontColour = "#1E234D", halign = "center", valign = "center", textDecoration = "bold", fontName = "Libre Franklin",
    fgFill = "#61BDAA", border = "TopBottomLeftRight", borderColour = "#1E234D", wrapText = TRUE
  )
  
  ## style for body
  bodyStyle <- createStyle(border = "LeftRight", borderColour = "#1E234D", fontSize = 10, fontColour = "#1E234D", fontName = "Libre Franklin")
  if(percent_decimal) {
    percent_style <- createStyle(border = "LeftRight", borderColour = "#1E234D", numFmt = "0.0%", fontSize = 10, fontColour = "#1E234D", fontName = "Libre Franklin")
  } else {
    percent_style <- createStyle(border = "LeftRight", borderColour = "#1E234D", numFmt = "0%", fontSize = 10, fontColour = "#1E234D", fontName = "Libre Franklin")
  }
  
  currency_style <- createStyle(border = "LeftRight", borderColour = "#1E234D", numFmt = "£0,0", fontSize = 10, fontColour = "#1E234D", fontName = "Libre Franklin")
  if(number_decimal) {
    number_style <- createStyle(border = "LeftRight", borderColour = "#1E234D", numFmt = "#,##0.0", fontSize = 10, fontColour = "#1E234D", fontName = "Libre Franklin")  
  } else {
    number_style <- createStyle(border = "LeftRight", borderColour = "#1E234D", numFmt = "COMMA", fontSize = 10, fontColour = "#1E234D", fontName = "Libre Franklin")  
  }
  
  
  # style for last row
  lastRowStyle  <- createStyle(fontSize = 10, fontColour = "#1E234D", fontName = "Libre Franklin", border = "BottomLeftRight", borderColour = "#1E234D")
  if(percent_decimal) {
    percent_style_last <- createStyle(border = "BottomLeftRight", borderColour = "#1E234D", numFmt = "0.0%", fontSize = 10, fontColour = "#1E234D", fontName = "Libre Franklin")  
  } else {
    percent_style_last <- createStyle(border = "BottomLeftRight", borderColour = "#1E234D", numFmt = "0%", fontSize = 10, fontColour = "#1E234D", fontName = "Libre Franklin")
  }
  
  currency_style_last <- createStyle(border = "BottomLeftRight", borderColour = "#1E234D", numFmt = "£0,0", fontSize = 10, fontColour = "#1E234D", fontName = "Libre Franklin")
  if(number_decimal) {
    number_style_last <- createStyle(border = "BottomLeftRight", borderColour = "#1E234D", numFmt = "#,##0.0", fontSize = 10, fontColour = "#1E234D", fontName = "Libre Franklin")
  } else {
    number_style_last <- createStyle(border = "BottomLeftRight", borderColour = "#1E234D", numFmt = "COMMA", fontSize = 10, fontColour = "#1E234D", fontName = "Libre Franklin")
  }
  
  # FIRST TAB
  ncols = ncol(tab1_data)
  nrows = nrow(tab1_data) + 1
  
  # write data
  addWorksheet(wb, tab1_name, gridLines = TRUE)
  writeData(wb, sheet = 1, startCol = 1, startRow = 1, tab1_data, rowNames = FALSE)
  
  # set column widths - first 'n' are auto, remaining are manual widths as defined in main function parameters
  setColWidths(wb, sheet = 1, cols = 1:ncols, widths = colwidths)
  
  # add filter
  addFilter(wb, sheet = 1, rows = 1, cols = 1:ncols)
  
  # add styles
  addStyle(wb, sheet = 1, headerStyle, rows = 1, cols = 1:ncols)
  addStyle(wb, sheet = 1, bodyStyle, rows = 2:nrows, cols = 1:ncols, gridExpand = TRUE)
  addStyle(wb, sheet = 1, lastRowStyle, rows = nrows, cols = 1:ncols, gridExpand = TRUE)
  
  # add styles for number and percent
  if(percent_cols_1[1] != 0) {
    addStyle(wb, sheet = 1, percent_style, rows = 2:nrows, cols = percent_cols_1, gridExpand = TRUE)
  }
  if(number_cols_1[1] != 0) {
    addStyle(wb, sheet = 1, number_style, rows = 2:nrows, cols = number_cols_1, gridExpand = TRUE)
  }
  if(currency_cols_1[1] != 0) {
    addStyle(wb, sheet = 1, currency_style, rows = 2:nrows, cols = currency_cols_1, gridExpand = TRUE)  
  }
  
  # add styles for number and percent - last row.
  if(percent_cols_1[1] != 0) {
    addStyle(wb, sheet = 1, percent_style_last, rows = nrows, cols = percent_cols_1, gridExpand = TRUE)
  }
  if(number_cols_1[1] != 0) {
    addStyle(wb, sheet = 1, number_style_last, rows = nrows, cols = number_cols_1, gridExpand = TRUE)
  }
  if(currency_cols_1[1] != 0) {
    addStyle(wb, sheet = 1, currency_style_last, rows = nrows, cols = currency_cols_1, gridExpand = TRUE)  
  }
  
  # SECOND TAB
  if(number_of_tabs > 1) {
    
    ncols = ncol(tab2_data)
    nrows = nrow(tab2_data) + 1
    
    # write data
    addWorksheet(wb, tab2_name, gridLines = TRUE)
    writeData(wb, sheet = 2, startCol = 1, startRow = 1, tab2_data, rowNames = FALSE)
    
    # set column widths - first 'n' are auto, remaining are manual widths as defined in main function parameters
    setColWidths(wb, sheet = 2, cols = 1:ncols, widths = colwidths)
    
    # add filter
    addFilter(wb, sheet = 2, rows = 1, cols = 1:ncols)
    
    # tab 2: formatting
    # add styles
    addStyle(wb, sheet = 2, headerStyle, rows = 1, cols = 1:ncols)
    addStyle(wb, sheet = 2, bodyStyle, rows = 2:nrows, cols = 1:ncols, gridExpand = TRUE)
    addStyle(wb, sheet = 2, lastRowStyle, rows = nrows, cols = 1:ncols, gridExpand = TRUE)
    
    # add styles for number and percent
    if(percent_cols_2[1] != 0) {
      addStyle(wb, sheet = 2, percent_style, rows = 2:nrows, cols = percent_cols_2, gridExpand = TRUE)
    }
    if(number_cols_2[1] != 0) {
      addStyle(wb, sheet = 2, number_style, rows = 2:nrows, cols = number_cols_2, gridExpand = TRUE)
    }
    if(currency_cols_2[1] != 0) {
      addStyle(wb, sheet = 2, currency_style, rows = 2:nrows, cols = currency_cols_2, gridExpand = TRUE)  
    }
    # add styles for number and percent - last row.
    if(percent_cols_2[1] != 0) {
      addStyle(wb, sheet = 2, percent_style_last, rows = nrows, cols = percent_cols_2, gridExpand = TRUE)
    }
    if(number_cols_2[1] != 0) {
      addStyle(wb, sheet = 2, number_style_last, rows = nrows, cols = number_cols_2, gridExpand = TRUE)
    }
    if(currency_cols_2[1] != 0) {
      addStyle(wb, sheet = 2, currency_style_last, rows = nrows, cols = currency_cols_2, gridExpand = TRUE)  
    }
    
  }
  
  # THIRD TAB
  if(number_of_tabs > 2) {
    
    ncols = ncol(tab3_data)
    nrows = nrow(tab3_data) + 1
    
    # write data
    addWorksheet(wb, tab3_name, gridLines = TRUE)
    writeData(wb, sheet = 3, startCol = 1, startRow = 1, tab3_data, rowNames = FALSE)
    
    # set column widths - first 'n' are auto, remaining are manual widths as defined in main function parameters
    setColWidths(wb, sheet = 3, cols = 1:ncols, widths = colwidths)
    
    # add filter
    addFilter(wb, sheet = 3, rows = 1, cols = 1:ncols)
    
    # tab 3: formatting
    # add styles
    addStyle(wb, sheet = 3, headerStyle, rows = 1, cols = 1:ncols)
    addStyle(wb, sheet = 3, bodyStyle, rows = 2:nrows, cols = 1:ncols, gridExpand = TRUE)
    addStyle(wb, sheet = 3, lastRowStyle, rows = nrows, cols = 1:ncols, gridExpand = TRUE)
    
    # add styles for number and percent
    if(percent_cols_3[1] != 0) {
      addStyle(wb, sheet = 3, percent_style, rows = 2:nrows, cols = percent_cols_3, gridExpand = TRUE)
    }
    if(number_cols_3[1] != 0) {
      addStyle(wb, sheet = 3, number_style, rows = 2:nrows, cols = number_cols_3, gridExpand = TRUE)
    }
    if(currency_cols_3[1] != 0) {
      addStyle(wb, sheet = 3, currency_style, rows = 2:nrows, cols = currency_cols_3, gridExpand = TRUE)  
    }
    # last row
    if(percent_cols_3[1] != 0) {
      addStyle(wb, sheet = 3, percent_style_last, rows = nrows, cols = percent_cols_3, gridExpand = TRUE)
    }
    if(number_cols_3[1] != 0) {
      addStyle(wb, sheet = 3, number_style_last, rows = nrows, cols = number_cols_3, gridExpand = TRUE)
    }
    if(currency_cols_3[1] != 0) {
      addStyle(wb, sheet = 3, currency_style_last, rows = nrows, cols = currency_cols_3, gridExpand = TRUE)  
    }
  }
  
  # FOURTH TAB
  if(number_of_tabs > 3) {
    
    ncols = ncol(tab4_data)
    nrows = nrow(tab4_data) + 1
    
    # write data
    addWorksheet(wb, tab4_name, gridLines = TRUE)
    writeData(wb, sheet = 4, startCol = 1, startRow = 1, tab4_data, rowNames = FALSE)
    
    # set column widths - first 'n' are auto, remaining are manual widths as defined in main function parameters
    setColWidths(wb, sheet = 4, cols = 1:ncols, widths = colwidths)
    
    # add filter
    addFilter(wb, sheet = 4, rows = 1, cols = 1:ncols)
    
    # tab 4: formatting
    # add styles
    addStyle(wb, sheet = 4, headerStyle, rows = 1, cols = 1:ncols)
    addStyle(wb, sheet = 4, bodyStyle, rows = 2:nrows, cols = 1:ncols, gridExpand = TRUE)
    addStyle(wb, sheet = 4, lastRowStyle, rows = nrows, cols = 1:ncols, gridExpand = TRUE)
    
    # add styles for number and percent
    if(percent_cols_4[1] != 0) {
      addStyle(wb, sheet = 4, percent_style, rows = 2:nrows, cols = percent_cols_4, gridExpand = TRUE)
    }
    if(number_cols_4[1] != 0) {
      addStyle(wb, sheet = 4, number_style, rows = 2:nrows, cols = number_cols_4, gridExpand = TRUE)
    }
    if(currency_cols_4[1] != 0) {
      addStyle(wb, sheet = 4, currency_style, rows = 2:nrows, cols = currency_cols_4, gridExpand = TRUE)  
    }
    # last row
    if(percent_cols_4[1] != 0) {
      addStyle(wb, sheet = 4, percent_style_last, rows = nrows, cols = percent_cols_4, gridExpand = TRUE)
    }
    if(number_cols_4[1] != 0) {
      addStyle(wb, sheet = 4, number_style_last, rows = nrows, cols = number_cols_4, gridExpand = TRUE)
    }
    if(currency_cols_4[1] != 0) {
      addStyle(wb, sheet = 4, currency_style_last, rows = nrows, cols = currency_cols_4, gridExpand = TRUE)  
    }
  }
  
  # FIFTH TAB
  if(number_of_tabs > 4) {
    
    ncols = ncol(tab5_data)
    nrows = nrow(tab5_data) + 1
    
    # write data
    addWorksheet(wb, tab5_name, gridLines = TRUE)
    writeData(wb, sheet = 5, startCol = 1, startRow = 1, tab5_data, rowNames = FALSE)
    
    # set column widths - first 'n' are auto, remaining are manual widths as defined in main function parameters
    setColWidths(wb, sheet = 5, cols = 1:ncols, widths = colwidths)
    
    # add filter
    addFilter(wb, sheet = 5, rows = 1, cols = 1:ncols)
    
    # tab 5: formatting
    # add styles
    addStyle(wb, sheet = 5, headerStyle, rows = 1, cols = 1:ncols)
    addStyle(wb, sheet = 5, bodyStyle, rows = 2:nrows, cols = 1:ncols, gridExpand = TRUE)
    addStyle(wb, sheet = 5, lastRowStyle, rows = nrows, cols = 1:ncols, gridExpand = TRUE)
    
    # add styles for number and percent
    if(percent_cols_5[1] != 0) {
      addStyle(wb, sheet = 5, percent_style, rows = 2:nrows, cols = percent_cols_5, gridExpand = TRUE)
    }
    if(number_cols_5[1] != 0) {
      addStyle(wb, sheet = 5, number_style, rows = 2:nrows, cols = number_cols_5, gridExpand = TRUE)
    }
    if(currency_cols_5[1] != 0) {
      addStyle(wb, sheet = 5, currency_style, rows = 2:nrows, cols = currency_cols_5, gridExpand = TRUE)  
    }
    # last row
    if(percent_cols_5[1] != 0) {
      addStyle(wb, sheet = 5, percent_style_last, rows = nrows, cols = percent_cols_5, gridExpand = TRUE)
    }
    if(number_cols_5[1] != 0) {
      addStyle(wb, sheet = 5, number_style_last, rows = nrows, cols = number_cols_5, gridExpand = TRUE)
    }
    if(currency_cols_5[1] != 0) {
      addStyle(wb, sheet = 5, currency_style_last, rows = nrows, cols = currency_cols_5, gridExpand = TRUE)  
    }
  }
  
  # SIXTH TAB
  if(number_of_tabs > 5) {
    
    ncols = ncol(tab6_data)
    nrows = nrow(tab6_data) + 1
    
    # write data
    addWorksheet(wb, tab6_name, gridLines = TRUE)
    writeData(wb, sheet = 6, startCol = 1, startRow = 1, tab6_data, rowNames = FALSE)
    
    # set column widths - first 'n' are auto, remaining are manual widths as defined in main function parameters
    setColWidths(wb, sheet = 6, cols = 1:ncols, widths = colwidths)
    
    # add filter
    addFilter(wb, sheet = 6, rows = 1, cols = 1:ncols)
    
    # tab 6: formatting
    # add styles
    addStyle(wb, sheet = 6, headerStyle, rows = 1, cols = 1:ncols)
    addStyle(wb, sheet = 6, bodyStyle, rows = 2:nrows, cols = 1:ncols, gridExpand = TRUE)
    addStyle(wb, sheet = 6, lastRowStyle, rows = nrows, cols = 1:ncols, gridExpand = TRUE)
    
    # add styles for number and percent
    if(percent_cols_6[1] != 0) {
      addStyle(wb, sheet = 6, percent_style, rows = 2:nrows, cols = percent_cols_6, gridExpand = TRUE)
    }
    if(number_cols_6[1] != 0) {
      addStyle(wb, sheet = 6, number_style, rows = 2:nrows, cols = number_cols_6, gridExpand = TRUE)
    }
    if(currency_cols_6[1] != 0) {
      addStyle(wb, sheet = 6, currency_style, rows = 2:nrows, cols = currency_cols_6, gridExpand = TRUE)  
    }
    # last row
    if(percent_cols_6[1] != 0) {
      addStyle(wb, sheet = 6, percent_style_last, rows = nrows, cols = percent_cols_6, gridExpand = TRUE)
    }
    if(number_cols_6[1] != 0) {
      addStyle(wb, sheet = 6, number_style_last, rows = nrows, cols = number_cols_6, gridExpand = TRUE)
    }
    if(currency_cols_6[1] != 0) {
      addStyle(wb, sheet = 6, currency_style_last, rows = nrows, cols = currency_cols_6, gridExpand = TRUE)  
    }
  }
  
  # SEVENTH TAB
  if(number_of_tabs > 6) {
    
    ncols = ncol(tab7_data)
    nrows = nrow(tab7_data) + 1
    
    # write data
    addWorksheet(wb, tab7_name, gridLines = TRUE)
    writeData(wb, sheet = 7, startCol = 1, startRow = 1, tab7_data, rowNames = FALSE)
    
    # set column widths - first 'n' are auto, remaining are manual widths as defined in main function parameters
    setColWidths(wb, sheet = 7, cols = 1:ncols, widths = colwidths)
    
    # add filter
    addFilter(wb, sheet = 7, rows = 1, cols = 1:ncols)
    
    # tab 6: formatting
    # add styles
    addStyle(wb, sheet = 7, headerStyle, rows = 1, cols = 1:ncols)
    addStyle(wb, sheet = 7, bodyStyle, rows = 2:nrows, cols = 1:ncols, gridExpand = TRUE)
    addStyle(wb, sheet = 7, lastRowStyle, rows = nrows, cols = 1:ncols, gridExpand = TRUE)
    
    # add styles for number and percent
    if(percent_cols_7[1] != 0) {
      addStyle(wb, sheet = 7, percent_style, rows = 2:nrows, cols = percent_cols_7, gridExpand = TRUE)
    }
    if(number_cols_7[1] != 0) {
      addStyle(wb, sheet = 7, number_style, rows = 2:nrows, cols = number_cols_7, gridExpand = TRUE)
    }
    if(currency_cols_7[1] != 0) {
      addStyle(wb, sheet = 7, currency_style, rows = 2:nrows, cols = currency_cols_7, gridExpand = TRUE)  
    }
    # last row
    if(percent_cols_7[1] != 0) {
      addStyle(wb, sheet = 7, percent_style_last, rows = nrows, cols = percent_cols_7, gridExpand = TRUE)
    }
    if(number_cols_7[1] != 0) {
      addStyle(wb, sheet = 7, number_style_last, rows = nrows, cols = number_cols_7, gridExpand = TRUE)
    }
    if(currency_cols_7[1] != 0) {
      addStyle(wb, sheet = 7, currency_style_last, rows = nrows, cols = currency_cols_7, gridExpand = TRUE)  
    }
  }
  
  # EIGHTH TAB
  if(number_of_tabs > 7) {
    
    ncols = ncol(tab8_data)
    nrows = nrow(tab8_data) + 1
    
    # write data
    addWorksheet(wb, tab8_name, gridLines = TRUE)
    writeData(wb, sheet = 8, startCol = 1, startRow = 1, tab8_data, rowNames = FALSE)
    
    # set column widths - first 'n' are auto, remaining are manual widths as defined in main function parameters
    setColWidths(wb, sheet = 8, cols = 1:ncols, widths = colwidths)
    
    # add filter
    addFilter(wb, sheet = 8, rows = 1, cols = 1:ncols)
    
    # tab 6: formatting
    # add styles
    addStyle(wb, sheet = 8, headerStyle, rows = 1, cols = 1:ncols)
    addStyle(wb, sheet = 8, bodyStyle, rows = 2:nrows, cols = 1:ncols, gridExpand = TRUE)
    addStyle(wb, sheet = 8, lastRowStyle, rows = nrows, cols = 1:ncols, gridExpand = TRUE)
    
    # add styles for number and percent
    if(percent_cols_8[1] != 0) {
      addStyle(wb, sheet = 8, percent_style, rows = 2:nrows, cols = percent_cols_8, gridExpand = TRUE)
    }
    if(number_cols_8[1] != 0) {
      addStyle(wb, sheet = 8, number_style, rows = 2:nrows, cols = number_cols_8, gridExpand = TRUE)
    }
    if(currency_cols_8[1] != 0) {
      addStyle(wb, sheet = 8, currency_style, rows = 2:nrows, cols = currency_cols_8, gridExpand = TRUE)  
    }
    # last row
    if(percent_cols_8[1] != 0) {
      addStyle(wb, sheet = 8, percent_style_last, rows = nrows, cols = percent_cols_8, gridExpand = TRUE)
    }
    if(number_cols_8[1] != 0) {
      addStyle(wb, sheet = 8, number_style_last, rows = nrows, cols = number_cols_8, gridExpand = TRUE)
    }
    if(currency_cols_8[1] != 0) {
      addStyle(wb, sheet = 8, currency_style_last, rows = nrows, cols = currency_cols_8, gridExpand = TRUE)  
    }
  }
  
  # save all sheets
  saveWorkbook(wb, xlsx_path, overwrite = TRUE)
  # optional save if want to save in a different location
  if(is.character(alternative_xlsx_path)) {
    saveWorkbook(wb, alternative_xlsx_path, overwrite = TRUE)
  }
  
}