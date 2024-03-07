#Mapping of variables




# Define the first and second row vectors
first_row <- c("figut (CYP3A) Use fabs (or a value of 1 if fabs is not available) assuming 100% inhibition",   "fgut (CYP3A)", "fm", "fm,CYP", "MW", "Dose", "t", "fu,plasma",
               "Ki,u", "ka (needed for Iinlet and for Eq. 3 & Eq. 5 )",
               "fabs (needed for Iinlet and for Eq. 3 & Eq. 5 )",
               "Igut Eq 7", "fgut (needed only if Iinlet is used)",
               "F", "Iinlet", "CL or CL/F (if using CL/F, use F= 1 in Col O)",
               "Elim rate const - k (needed if Imax is used)",
               "Imax (assumes instantaneous absorption)", "Isys", "KI,u",
               "kinact", "[I]u","AUC ratio (Rev)  Eq 2; figut = fabs",
               "Gut contribution to AUC ratio (Rev) Eq3",
               "AUC ratio (Rev)   Eqs 2 & 3",
               "AUC ratio (TDI)   Eq 4; figut = fabs",
               "AUC ratio (TDI) liver",
               "Gut contribution to AUC ratio (TDI) Eq 5",
               "AUC ratio (TDI)  Eqs 4 & 5"
               )

second_row <- c("","", "(1-fe)", "", "", "(mg)", "(h)", "", "(µM)", "(min-1)",
                "", "(µM)", "", "(µM)", "(mL/min)", "", "(min-1)", "(µM)",
                "(µM)", "(µM)", "(min-1)", "(µM)","","","","","","","")

values_vector <- c("selected_row_substr$fabs",
                   "selected_row_substr$fgut",
                   "selected_row_substr$`fm (1-fe)`",
                   "selected_row_substr$fmCYP3A4",
                   "selected_row_inhibtr$MW",
                   "selected_row_inhibtr$`Dose (mg)`",
                   "selected_row_inhibtr$`τ (h)`",
                   "selected_row_inhibtr$fu",
                   "selected_row_inhibtr$`Ki,u (µM)`",
                   "selected_row_inhibtr$`ka (min-1)`",
                   "selected_row_inhibtr$fabs",
                   "iguteq7",
                   "selected_row_inhibtr$fgut",
                   "selected_row_inhibtr$F",
                   "linlet_val",
                   "selected_row_inhibtr$`CL/F (mL/min)`",
                   "selected_row_inhibtr$F",
                   "lmax_val",
                   "lsys_val",
                   "selected_row_inhibtr$`Ki,u (µM)`",
                   "selected_row_inhibtr$`Kinact (min-1)`",
                   "Iu_val",
                   "auc_ratio1",
                   "gutcontribution",
                   "auc_ratio2",
                   "auc_ratio3",
                   "auc_ratio4support1",
                   "auc_ratio4support2",
                   "auc_ratio4")

# Combine first_row and second_row using underscore as a join character
combined_row <- paste(first_row, second_row, sep = "_")

# Create the dataframe
df <- data.frame("Excel names" = combined_row, "variable names" = values_vector)

df$ids <- sapply(df$Excel.names, function(x) {
  len <- sample(6:20, 1)
  paste0(substr(gsub("[^[:alnum:]]", "", x), 1, len))
})

unique(df$ids)

write.csv(df, "varmap.csv")
