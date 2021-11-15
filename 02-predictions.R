# Predict ----

# The fobization is not applied if the predicted CIF rate is below 1, if
# applying it would increase the bilateral asymmetry, or if the importer does
# not report CIF.

# Here "not in kilograms" refers to the cases when not both parties report
# weights in kilograms

# Kilowatt/hour units are labelled as "no fobization" and are not converted
# nor corrected

# rm(sfit)
#
# dexp <- map_df(1994:2004, impute_exp)
#
# dexp %>%
#   group_by(reported_by) %>%
#   count()
#
# dexp %>%
#   group_by(reported_value) %>%
#   count()
