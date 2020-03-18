# agmip_post_processing
Module that generates AgMIP reporting template from GCAM output (basex) database(s)

This fileset generates the AgMIP reporting template that was used for the GCAM 2020 submission for the main AgMIP study as well as a supplementary diet study. It queries user-specified databases and/or scenarios with a provided batch query file, and processes the data to the final format used in AgMIP global econoimic subgroup studies. It is set up to query data either from a local database, or from PIC using a connection.