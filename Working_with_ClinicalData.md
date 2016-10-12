# Working with Clincal Data


In this assignment you will gain experience extracting and trasforming clinical data from the state it is stored in into datasets for later statistical analysis. You will practice using common time-saving tools in the `R` programming language that are ideally suited to these tasks. 

You will use the [MIMIC III database](https://mimic.physionet.org/mimictables/patients/) as a sandbox to create a dataset describing patients in the intensive care unit whose respiratory function rapidly deteriorated while under [mechanical ventilation](https://en.wikipedia.org/wiki/Mechanical_ventilation). P/F ratio referes to a ratio of Pao2 (arterial oxygen partial pressure) and Fio2 (fractional inspired oxygen), which is how much oxygen is reaching the patient's blood relative to how much is being artificially supplied by the mechanical ventillator. All of the data you need for this assignment is here, or can be found in the files section of Canvas.

Please edit this document directly using either Jupyter Notebook or R markdown in R Studio and answer each of these questions in-line. Jupyter and R markdown are useful tools for reproducible research that you will use over and over again in your later work. They are worth taking the short amount of time necessary to learn them. Turn in a single `.pdf` document showing all of your code and output for the entire assignment, with each question clearly demarcated. You will be graded on your answers to each question, with partial credit given for correct code. Submit your completed assignment through Canvas.

## 0. Getting Ready

a) The first thing we need to do is load all of the packages we will use for this assignment. Please load the packages `dplyr`, `tidyr`, `lubridate`, `stringr`, and `ggplot2`. Also, please run the command `Sys.setenv(TZ='UTC')`. Without it, your date-time data will misbehave. 

## 1. Building a Cohort Based on Inclusion Criteria

### Loading Data

----
##### 1.1 (5 pts)

The first part of any patient-level study is identify the patients who are relevant to the study, and at what point during their records they became elligible. Typically, this is done with a set of "inclusion critera", which, if met, qualify the patient for inclusion. In our study, we will consider the following inclusion criteria based on [this study](http://ieeexplore.ieee.org/stamp/stamp.jsp?arnumber=4649796):
 
- Patients who were on mechanical ventilation
- and have a P/F ratio > 250 for first 12 hours of ventilation
- After at least the initial 12 hour period, P/F ratio drops below 300 and remains below 300 for > 1 day

The process of identifying patients who meet certain clinical criteria is sometimes refered to as _electronic phenotyping_.

It seems that to do this, we will need to know who was on _mechanical ventilaion_ and what their _P/F ratios_ were over time. We need to find if and where those are recorded in the database by seeing if there is an internal code or ID assigned to them. Using the MIMIC docomumentation, determine what table(s) in MIMIC you would query to find out if and where mechanical ventillation or P/F ratios would be recorded. 

----
##### 1.2 (5 pts)

If you were to search that table using the measurement label and find counts of the measurements (from other tables) of the relevant results, you would see that the duration of mechanical ventilation is not specified and P/F measurements are not directly recorded. However, there may be a way to create this data from what we do have. If we're lucky, other researchers have already done this work for us, most likely using SQL. 

Search the web for _SQL_ that uses _MIMIC_ data to compute _mechanical ventilation duration_ and for _SQL_ to compute the _Pao2 Fao2 ratio_. Report the URLs of the sources you find. 

----
##### 1.3 (5 pts)

Looks like we're in luck this time, but it's often the case that nobody has been gracious enough to do this and you need to find clinical experts and database managers to work with in order to extract quantities of interest. The course team has done the work of running the SQL we found against the full MIMIC database and exporting the results to two csv files, called `pf.csv` and `vent.csv`. We'll use those tables to find our patients. 

Load those two tables into R dataframes and use `head()` and to examine each of them. The tables are moderately sized, so don't worry if it takes a minute or two.

Using `dplyr` commands (perhaps with a little help from the `lubridate` package), make a dataframe of ICU stays where the patient had a stable start: i.e. they did not have any P/F ratios under 250 for the first 12 hours of their ventilation. Use `p_charttime` as the time of the P/F ratio measurement (given what you know about Pao2 and Fio2 measurements, think about why that makes sense). Exclude ICU stays without P/F ratio measurements in the first 12 hours of ventilation. Our code to do this is a chain of 7 `dplyr` commands. The result should be a dataframe with a single column. Use `head()` to examine it. Perhaps something has gone wrong...

Identify the problem. Perhaps use `str()` to see what is going on in the intermediate results. Which variables are not represented correctly?

-----
##### 1.4 (5 pts)

Solve the problem using `as.POSIXct` and one or two other functions. Try your code again from above to find the ICU stays where the patient had a stable start. The result should be a dataframe with a single column and about 4630 rows.

How many ICU stays are there in your final list?

----
### Cohort Building

-----
##### 1.5 (10 pts)

Now we're going to do a bit of tricky table manipulation to find those stable-start ICU stays (what we did above) for which the patients additionally had a longer-than 3-hour period during which their P/F ratios remained under 300. There are a few different steps and it can seem complicated if you're not used to thinking about data in terms of tables, so we'll go through it incrementally. First we will find all the PF measurements that are under 300 and happened 12 hours or more after the beginning of ventillation, but before the end of ventillation. Then we will build 3-hour long windows, starting with the measurements identified, and using a join with the original measurements we will see if any measurements in the window go above 300. If they do not, then that patient's stay in the ICU satisfies the inclusion criteria.

The first step is to find the times of all of the PF measurements that are under 300 and happened 12 hours or more after the beginning of ventilation, but before the end of ventilation. Use `dplyr` commands to create this dataframe. It should have the columns `icustay_id` and `p_charttime`.

How many rows does your table have?

-----
##### 1.6 (20 pts)

Using a self-join, we will build the shortest possible time windows that begin and end with two PF values under 300 and which are longer than 3 hours. Using a join with the original measurements we will see if any measurements in the windows go above 300, and remove those windows that do.

Implement this using dplyr commands. The result should be a dataframe with three columns: `icustay_id`, `window_begin`, and `window_end`. The last two columns should be datetimes indicating the periods (windows) during that ICU stay during which there were no PF measurements made above 300.

How many rows does the result have?

----
##### 1.7 (20 pts)

Let's visualize what we've done so far. This should help you identify any errors that you might of made. It's often difficult to keep track of all the data manipulation, even for expert researchers! That's why it's good practice to plot things every once in a while as a sanity check.

Use `ggplot` and `facet_wrap` to show the entire PF ratio trajectories of the first 9 ICU stays in your windows dataframe. Plot the datetime on the x-axis and the PF value on the y-axis. Use color to indicate PF measurements that are within or at the borders of your windows. Include a horizontal line on each plot at PF=300, and a green vertical line on each plot indicating the start of ventilation time plus 12 hours (the last example here http://docs.ggplot2.org/0.9.3.1/geom_vline.html and this question http://stackoverflow.com/questions/5388832/how-to-get-a-vertical-geom-vline-to-an-x-axis-of-class-date will come in handy). Your code may take a minute or two to run. *(20 pts)*

-----
##### 1.8 (10 pts)

We're at the last step. Now we know what patients seemed normal to start with, but later experienced a sustained drop in their PF ratios. If we're going to predict a treatment or outcome or see what treatments worked well for this kind of people, we need to establish the point in time at which the clinician would have concluded that the patient was experiencing the condition, starting the decision process of how to respond. If we don't do this, then we might accidentally use data from after the decision was made to try to predict the decision! In our case, let's say that time the end of the first window for each patient. We'll call that time the "index time" for each ICU stay.

Use `dplyr`, the results from above, and the file `icustays.csv` to create a dataframe containing three columns: `subject_id`, and `index_time`. Some patients (subjects) may have had multiple qualifying ICU stays, so only use the first of them.

How many ICU stays are there in the final cohort?

## 2. Building a Patient-Feature Matrix for this Cohort

Now that we know what patients are relevant to our question, we can gather up their data. In case you're still having trouble with part I, you can simply load the `cohort.csv` file at this point and use that in your analysis instead of the result you produced.

### Diagnoses

-----
##### 2.1 (3 pts)

Let's first deal with diagnoses. Load `diagnoses_icd.csv`. We would like to find the diagnoses that occured before the index time for each patient, but it looks like there is no time recorded in this table. 

What table in MIMIC would you use to find the times of each diagnoses? Use the online documentation to find out. *(3 pts)*

-----
##### 2.2 (4 pts)

This table is contained in `mystery.csv`. Load it into R and use it in conjunction with the diagnoses and cohort tables to filter the diagnoses for each patient that occured before the index date. Use the `dischtime` column as the time of diagnosis (think about why this makes sense, given that diagnoses are recored for billing). The final result should have the columns `subject_id`, `diagnosis_time`, `diagnosis`, and `index_time`.

How many rows are in the result?

-----
##### 2.3 (4 pts)
What are the top 10 most common diagnosis codes (by number of patients who have had them) in this data? Google the top 3 codes and think about whether or not you think they make sense for this cohort. This is another kind of sanity check

-----
##### 2.4 (4 pts)
Make a plot of the number of codes that are present in N number of patients. The x-axis should be the number of patients, and the y-axis should be the number of codes that are present in that number of patients.  In one sentence, dscribe the meaning of the plot in your own words. *(4 pts)*


-----
##### 2.5 (2 pts)
As you observed from the plot you created above, rare diagnoses can result in a sparse feature space. One way to manage this is to group rare features into broader categories that are more common in the data. *Information content (IC)* is a measure of specificity based on the frequency of occurrence of features that can be used in combination with a *concept hierarchy* to identify broader categories of features. 

The IC of a term that occurs in a set of documents is calculated as 

$log_2 \left( \frac{frequency(term)}{count(document)} \right)$

We have adapted this equation to calculate the IC of ICD9 codes and their parent concepts from the SNOMED CT concept hierarchy, based on their occurrence in Stanford EHRs:

$log_2 \left( \frac{frequency(ICD9 concept)}{count(patients)} \right)$

In this next step, you will use ICs we have calculated in combination with SNOMED CT's concept hierarchy to aggregate ICD9 codes into their parent categories within a specific IC range.

First, load the following three files in R: `icd9_cui.csv`, `child_parent_cui.csv` and `cui_ic.csv` and examine them with `head()`. `icd9_cui.csv` contains ICD9 codes and their corresponding UMLS concept unique identifier (CUI). `child_parent_cui.csv` contains the transitive closure for each ICD9 CUI - that is, the set of all parent concepts for each ICD9 CUI (including itself). `cui_ic.csv` contains the IC values for ICD9 SNOMED CT CUIs.

Join `icd9_cui.csv` and `child_parent_cui.csv` to get all the parent concepts for each ICD9 code. How many parent concepts does ICD9 code 401.9 have?


-----
##### 2.6 (2 pts)

What is the range (min and max) of ICs observed in the data? What are the 10 most general CUIs?

-----
##### 2.7 (3 pts)

For each ICD9 code, find its parent CUIs with an IC between 4 and 8, and keep only the most specific parent CUI. The result should be a table with the rows `icd9`, `cui`, `parent_cui` (the most specific parent cui with an IC between 4 and 8) and `ic` (which should be between 4 and 8).


-----
##### 2.8 (3 pts)
Use the resulting data frame to replace diagnoses in the dx_cohort with their parent CUI that is in the desired IC range. How many diagnosis features do you have after aggregation by IC? If something seems off, check how the data is coded in MIMIC relative to how it is coded in the snomed files... 

-----
##### 2.9 (10 pts)
Now we have our list of diagnoses features and the times they occured for each patient. All that is left to do is to create the patient-feature matrix. Using `tidyr` and `dplyr`, make a patient-feature matrix where each row is a single patient and each column is a diagnosis code, time binned by whether or not it occured in the 6 months most recent to the index time. There should be two columns for every diagnosis- one that indicates how many times the patient had the code in her record in the past six months, and one that indicates how many times that patient had the code in her record before that time. The way to implement this is using a `mutate` to create a time bin indicator and then grouping on that before summarizing and spreading. Use `unite` before spreading to create a unique name for each feature based on its diagnosis code and time bin.

Note that the ICU stay is the first time many patients have been seen at this hospital, so most patients may have few or no prior recorded diagnoses.

What are the dimensions of your resultant dataframe?

----
### Notes

-----
##### 2.10 (7 pts)
Now let's add features from notes. To do so, we'll have to process some text.

The `noteevents` table in MIMIC is too large and unweildy to load into R, so we've extracted the rows from that table that you will need by loading the cohort table into the database and using SQL. Write the SQL that you would use to get every row of the notes table that is for a patient in the cohort and that was written before the index date.

-----
##### 2.11 (3 pts)
The result is in the file `notes.csv`. Load it into R and examine it with `head()`.

UMLS terminologies provide concept hierarchies, as well as sets of terms for individual concepts. For example, there are more than 50 terms in UMLS terminologies for the concept 'myocardial infarction'!

In this step, you will use the SNOMED CT hierarchy and UMLS term sets to construct a dictionary of terms for respiratory system disorders, and then search for those terms in MIMIC III notes.

First, load `snomed_ct_isaclosure.csv` and `snomed_ct_str_cui.csv` in R, and examine them with `head()`. `snomed_ct_isaclosure.csv` contains the child-parent CUI relationships for all of SNOMED CT. `snomed_ct_str_cui.csv` contains the terms (each with a unique term identifier, tid) for each SNOMED CT CUI. These are large files, so loading them may take a minute.

Join `snomedct_isa_closure` with `snomedct_cui_string` to find all terms for each CUI (including the terms associated with its children).

-----
##### 2.12 (4 pts)

Find the CUI for respiratory system disorders in SNOMED CT, and construct a dictionary of all terms (a set of terms) corresponding to  respiratory disorders that have 20 characters or fewer. How many terms are in the dictionary?

-----
##### 2.13 (7 pts)
With the dictionary in hand, use `str_detect` from `stringr` and `sapply` from base R to add columns to your notes dataframe indicating whether or not text in that note matches one of the first fifty terms in your dictionary (limited for computational purposes). Your answer should have the columns `note_id` (the same as `row_id` in `notes.csv`), `subject_id`, `chartdate`, and as many more columns as there are terms in the dictionary (50).

What are the dimensions of the resulting dataframe?

-----
##### 2.14 (7 pts)

Now use `snomed_ct_concept_string` to convert terms back to their concepts and construct a dataframe of `subject_id`, `chartdate` and `concept`.

-----
##### 2.15 (7 pts)

As with the diagnoses, we must transform this data into a patient-feature matrix. Use `dplyr` and `tidyr` to transform this table of concept mentions into a patient-feature matrix where each row is a patient and each column is the presence or absence of a concept. Do not do any time-binning. Each concept should have only one column. Instead of counts, use a binary indicator to indicate that the concept was present in the patient's notes.

What are the dimensions of the resulting table?

----
### Vitals

-----
##### 2.16 (4 pts)

Finally, let's try to engineer some features from vital sign measurements.

We will focus on heart rate measurements. In what table would you find heart rate measurements in MIMIC? Use the online documentation.

-----
##### 2.17 (5 pts)

As with the notes, we've done the work of extracting the relevant rows of the relevant table for you. Load the table `heart_rates.csv` to proceed.

Let's do a sanity check. Make sure none of the measurements in this file were actually recorded after the index time or each patient. How many incorrect rows are there? If there are any, filter them out.

-----
##### 2.18 (10 pts)

One feature of interest might be the latest value of the heart rate before the index time. Use `dplyr` to make a dataframe with three columns: `subject_id`, `latest_heart_rate`, and `charttime`. 

What is the average value of the latest recorded heart rate in this cohort? Make a histogram or density plot to convince yourself that the data are consistent with what you might expect heart rate measurements to be in this cohort. *(10 pts)*

-----
##### 2.19 (5 pts)

The latest recorded heart rate might not be a useful feature to use if the latest recording is not near the index time. Make a density plot of the time difference between the latest heart rate recording and the index time.


-----
##### 2.20 (7 pts)
Some patients might have many heart rate recordings, and only using the last one might not be the best idea- it's possible the latest measurement is an outlier. Let's try to leverage all the heart rate measurements we have by creating a time-weighted average heart rate. Use the formula $w = e^{(-|\Delta t| - 1)}$ to calculate the weights of each measurement, where $\Delta t$ is the time difference between the measurement time and the index time in hours. Calculate the weighted average with the formula $\bar{x}_w = \sum(x_i w_i)/\sum(w_i)$. The result should be a dataframe with two columns: `subject_id` and `time_wt_avg`. Beware of measurements that were noted but have no recorded value.

What is the average time-weighted average heart rate across all patients? 

-----
##### 2.21 (4 pts)
Again let's do a sanity check to see if what we've done makes sense. We should expect that the time-weighted average heart rate and the latest recorded heart rate should be similar.

Make a scatterplot of the latest recorded heart rate (x-axis) and the time-weighted average heart rate (y-axis) of each patient.

----
### Joining the Features

-----
##### 2.22 (15 pts)
Our final patient-feature matrix will simply be the amalgamation of the different feature matrices we've created. Use an outer join to combine the columns of the feature matrices from diagnoses, notes, and heart rate measurements. Not all patients have diagnoses or note features, so fill in any NA values with 0 to indicate that there were no diagnoses or notes counted. Use `names` to look at all the features and make sure everything seems ok.

How many total features are there? What is the correlation between the number of unspecified hypertension diagnoses in the past six months and the latest measured heart rate? *(15 pts)*

----
That's it! We've gone through the major steps of transforming different kinds of data stored in a longitudinal database into a patient-feature matrix that we can use for association tests and prediction tasks. Along the way we hope you have gained practice in how to effectively use the `dplyr` and `tidyr` packages to manipulate data and the `ggplot2` package to make visual diagnostics. You are well on your way to being able to perform a clinical informatics study.
