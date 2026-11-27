## R CMD check results

0 errors | 0 warnings | 1 note

* This is a new release.
* Using the check() function shows possible problems under NOTE.
* These are two global variables, I) metric and II) value in the function summarise_data()
* This function has parallel-processing and it seems the check() function does not recognize the output generated from this part correctly.
* The functions works as intended and provides no errors. Neither am I understanding of how to resolve this NOTE from check().