# WCST index calculation

## What We Provide?

**We provide these indices‘ calculation:**

* TC  -Total Number Correct
* PR  -Perseverative Responses
* PE  -Perseverative Errors
* NPE -Nonperseverative Errors
* CLR -Conceptual Level Responses
* CAT -Number of Categories Completed
* FMS -Failure to Maintain Set

## What's The Input Data's Format?

We need a .csv file or a dataframe like this:

| subid | number_of_rule | correct_card | color_rule | shape_rule | number_rule | button_pressed | category_completed |
| ----- | -------------- | ------------ | ---------- | ---------- | ----------- | -------------- | ------------------ |
| 001   | 1              | 1            | 1          | 0          | 0           | 1              | 0                  |
| 001   | 1              | 0            | 0          | 2          | 3           | 0              | 0                  |
| 001   | 1              | 3            | 3          | 0          | 1           | 3              | 0                  |
| 001   | 1              | 0            | 0          | 3          | 0           | 0              | 0                  |
| 001   | 1              | 1            | 1          | 1          | 3           | 1              | 0                  |
| 001   | 1              | 2            | 2          | 2          | 0           | 2              | 0                  |
| 001   | 1              | 3            | 3          | 0          | 3           | 3              | 0                  |
| 001   | 1              | 0            | 0          | 3          | 2           | 2              | 0                  |
| 001   | 1              | 1            | 1          | 2          | 3           | 2              | 0                  |

The input frame should contain these columns:

* 'subid':              subject's id
* 'number_of_rule':     the present correct dimension(1/2/3 namely color/shape/number) that should be obey
* 'correct_card':       the correct card(0/1/2/3 namely the top four fixed cards from left to right) that should be chosen if obey the present correct dimension
* 'color_rule':         one of the four cards(0/1/2/3 ~) that would be chosen if obey the color dimension
* 'shape_rule':         one of the four cards(0/1/2/3 ~) that would be chosen if obey the shape dimension
* 'number_rule':        one of the four cards(0/1/2/3 ~) that would be chosen if obey the number dimension
* 'button_pressed':     one of the four cards(0/1/2/3 ~) actually be chosen
* 'category_completed': total categories completed(0/1/2/3/4/5/6-)

## What's The Output Data's Format?

We will get a "wcst_short.csv" file in the "persistance" directory

The output frame look like this:

|      | subid | TC   | PR   | PE   | NPE  | CLR  | CAT  | FMS  | p_TC     | p_PR     | p_PE     | p_NPE    | p_CLR | p_CAT    | p_FMS |
| ---- | ----- | ---- | ---- | ---- | ---- | ---- | ---- | ---- | -------- | -------- | -------- | -------- | ----- | -------- | ----- |
| 1    | 001   | 13   | 17   | 5    | 12   | 0    | 4    | 0    | 0.203125 | 0.265625 | 0.078125 | 0.1875   | 0     | 0.0625   | 0     |
| 2    | 002   | 16   | 9    | 3    | 13   | 0    | 1    | 0    | 0.25     | 0.140625 | 0.046875 | 0.203125 | 0     | 0.015625 | 0     |
| 3    | 003   | 17   | 13   | 2    | 12   | 0    | 5    | 0    | 0.265625 | 0.203125 | 0.03125  | 0.1875   | 0     | 0.078125 | 0     |
| 4    | 004   | 15   | 14   | 3    | 12   | 0    | 5    | 0    | 0.234375 | 0.21875  | 0.046875 | 0.1875   | 0     | 0.078125 | 0     |
| 5    | 005   | 16   | 14   | 3    | 12   | 0    | 5    | 0    | 0.25     | 0.21875  | 0.046875 | 0.1875   | 0     | 0.078125 | 0     |

Clearly, the output frame contains these columns:

* TC  -Total Number Correct
* PR  -Perseverative Responses
* PE  -Perseverative Errors
* NPE -Nonperseverative Errors
* CLR -Conceptual Level Responses
* CAT -Number of Categories Completed
* FMS -Failure to Maintain Set

What's more, we also provided the percent-type of these indices with "p_" prefix means "percent"

* p_TC -Total Number Correct of the total trials
* ...

## How to Read The R Script?

**The .R script is divided into four parts:**

1. STEP1: data preperation

   > by default, we read the "test_wcst_long.csv" file as a  table from the same directory of the .R script.
   >
   > you can also replace it with another table or file, but remember to remain the same format.
   >
   > the format is described above.

2. STEP2: necessary mediate indices calculation

   > we use this part to calculate mediate indices in wcst_long.csv
   >
   > these indices are used for the next step to calculate our target indices.
   >
   > you can also add your own functions to calculate your own mediate indices.

3. STEP3: indices calculation functions and descriptive statistics

   > the functions to calculate our target indices and save to .csv file.
   >
   > you can also add your own functions to calculate your own indices.

4. STEP4: split-half functions and estimates

   > this part is shut off, if you deannotate these part, it may cost most of the CPU source and may cost much time to complete.

