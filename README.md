# READ ME: 
#Coursera Course Project for Getting and Obtaining Data#


## Project Summary ##
The script run_analysis.R downloads, imports, merges, and summarizes Smartphone-obtained motion measurements from the study below.

#### Citation  ####
Davide Anguita, Alessandro Ghio, Luca Oneto, Xavier Parra and Jorge L. Reyes-Ortiz. Human Activity Recognition on Smartphones using a Multiclass Hardware-Friendly Support Vector Machine. International Workshop of Ambient Assisted Living (IWAAL 2012). Vitoria-Gasteiz, Spain. Dec 2012

#### Dataset website ####
http://archive.ics.uci.edu/ml/datasets/Human+Activity+Recognition+Using+Smartphones#

#### Study details  ####
Thirty volunteer subjects (ages 19-48) performed a series of six activities (walking, walking upstairs, walking downstairs, sitting, standing, and laying) while wearing a smartphone (Samsung Galaxy S II) on the waist.

Using the phone's embedded accelerometer and gyroscope, Anguita et al. captured 3-axial linear acceleration and 3-axial angular velocity at a constant rate of 50Hz.  The sensor signals (accelerometer and gyroscope) were pre-processed by applying noise filters and then sampled in fixed-width sliding windows of 2.56 sec and 50% overlap (128 readings/window). The sensor acceleration signal, which has gravitational and body motion components, was separated using a Butterworth low-pass filter into body acceleration and gravity. The gravitational force is assumed to have only low frequency components, therefore a filter with 0.3 Hz cutoff frequency was used. From each window, a vector of features was obtained by calculating variables from the time and frequency domain. 

The publicly-available processed dataset possesses 561 features, including time and frequency domain measurements (x, y, and z coordinate and magnitude Euclidean norm) for body acceleration, gravity acceleration, body acceleration jerk signals, and body gyroscope signals and jerk signals. These measurements were made for multiple repetitions of each of the six activities by each subject. Features were normalized and bounded within [-1,1].


#### R script summary:  ####
The purpose of R script run_analysis.R is to reformat the processed data described above into a single dataset possessing only mean and standard deviation calculations from among the features provided.  The mean of each data column is calculated for each activity for each patient, so the resulting dataset possesses mean data from the 66 selected "mean()" and std()" features across 180 rows (30 subjects x 6 activities).




## run_analysis.R Details  ##

The script downloads the processed data from the above website and imports it for analysis.  Subject data had previously been randomly partitioned into two sets, where 70% of the volunteers was selected for generating the training data and 30% the test data. Here, these datasets are merged, so measurements from all 30 subjects can be analyzed together.

Input files include:  

- subject_train.txt
- subject_test.txt
- X_train.txt
- X_test.txt
- y_train.txt
- y_test.txt
- features.txt
- activity_labels.txt

"Subject" files possess a single column of subject id numbers that correspond to the rows of data in the "X" files.  The "y" files contain a single column of numeric activity codes that reflect the activity performed for the corresponding row in the "X" files.  The "activity_labels.txt" file containing the key to which of the 6 activities correspond to the activity codes used in the "y" files.  The "features.txt" file containing the header labels for each of the columns of data in the "X" files. 

First, the script merges all of these files into a single dataframe with 3 header columns + 561 columns of numeric data and 10299 rows.

Next, the script using grep to select only the 66 features that represent mean (ie "mean()") and standard deviation (ie "std()") calculations.  (For example, "tBodyAcc-mean()-X" was one of the selected features). 

Activity labels were re-formatted from uppercase to lowercase and underscores were removed for use as factors in the script.

Feature names from the "features.txt" file were reformatted to remove parentheses and hyphens to improve readability.  (For example: "tBodyAcc-mean()-X" was converted to "tBodyAcc.mean.X").  These are column headers are not factor variables so the dots in the names were used for increased readability.  


Lastly, the script transforms the merged dataframe using the melt and dcast functions within the reshape2 package to calculate the mean of each data column for each activity for each patient.   The output "tidy" dataset (text file below) possesses mean data from the 66 selected "mean()" and std()" features across 180 rows (30 subjects x 6 activities).

Output file:  

- tidy\_data\_smartphone\_measurements\_by\_subject\_by_activity.txt

Supporting documentation:

- READ_ME.md

- codebook.md

codebook was generated using:
http://www.tablesgenerator.com/markdown_tables#

Last updated May 23, 2014