CREATE TABLE Florida(         /* Creates the empty table
"policyID" INTEGER,           /* Fill in each of the variable names and types
"statecode" CHAR,
"county" CHAR,
"eq_site_limit" INTEGER,
"hu_site_limit" INTEGER,
"fl_site_limit" INTEGER,
"fr_site_limit" INTEGER, 
"tiv_2011" INTEGER,
"tiv_2012" DOUBLE,
"eq_site_deductible" DOUBLE,
"hu_site_deductible" DOUBLE,
"fl_site_deductible" DOUBLE,
"fr_site_deductible" DOUBLE,
"point_latitude" DOUBLE,
"point_longitude" DOUBLE,
"line" CHAR,
"construction" CHAR,
"point_granularity" INTEGER);
.mode csv                                  /* Tells sql that we want a CSV
.import FL_insurance_sample.csv            /* Import our CSV into the SQL file
SELECT * FROM Florida LIMIT 10;            /* Select our first ten rows and print them out
SELECT DISTINCT county FROM Florida;       /* Print out the unique values of the county variable
SELECT AVG(tiv_2012-tiv_2011) FROM Florida; /* Solve for the average of the two variables as on the PS3 instructions 
SELECT construction, COUNT(*) FROM Florida GROUP BY construction; /* Create the 1-way frequency table
