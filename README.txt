These are the data and code to accompany Pisor & Gurven (in press) When to diversity, and with whom? Choosing partners among out-group strangers in lowland Bolivia (http://dx.doi.org/10.1016/j.evolhumbehav.2017.09.003).

NOTE: These data were collected with living people who do not wish to be identified. Please respect their wishes and do not attempt to identify any of the participants in this sample.

Data are provided in Partner choice dataset_31-Jul-17.csv, and your guide to the meaning of each variable can be found in Data dictionary_21-Jul-17.csv. 

All code provided relies on the R statistical program, as well as its implementation of the Stan program. Data must be processed using the Data restructure 31-Jul-17.r file before Stan models can be run.

The main model featured in the paper (Table 1) appears in Minimal model_29-Jul-17.r. The other models can be matched to tables and figures as follows:

Figures 3 and 4: Minimal model_29-Jul-17.r.
Table S1: Data restructure_31-Jul-17.r.
Table S3: Minimal model_TVandCar_31-Jul-17.r.
Table S4: Prev sig model_27-Jul-17.r.
Table S5a: All controls model_Age_29-Jul-17.r.
Table S5b: All controls model_AgeDiff_29-Jul-17.r.
Table S6a: Consensus model_29-Jul-17_Good.r.
Table S6b: Consensus model_27-Jul-17_Trust.r.
Table S7: Data restructure_31-Jul-17.r.
Table S8: Data restructure_31-Jul-17.r.
Table S9: Anonymity only model_26-Jul-17.r.
Table S10: Data restructure_31-Jul-17.r.
Table S11a: Tsimane model_31-Jul-17.r.
Table S11b: Moseten model_28-Jul-17.r.
Table S11c: Intercultural model_27-Jul-17.r.

NOTE: These models feature 100 imputed data sets. Accordingly, these models were run on a UNIX server, and the parallelization we used cannot be handled by (most) personal computers. If you are not running this code on a server, omit the parallel processing code to avoid crashing your computer.
