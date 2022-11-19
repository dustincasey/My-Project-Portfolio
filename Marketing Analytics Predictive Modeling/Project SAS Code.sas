proc import out=work.TrainData datafile="Z:\Students\100133984\MKTG 130\Final Project\Project Data - DCasey.xlsx" dbms=xlsx replace;
			Sheet='Data';
run;
proc import out=work.ValidationData datafile="Z:\Students\100133984\MKTG 130\Final Project\Project Data - DCasey.xlsx" dbms=xlsx replace;
			Sheet='Validation';
run;
data work.TrainData;
set work.TrainData;
	if Gross_Revenue = 0 then Purchase = 0;
   		else Purchase=1;
run;
data work.RegTrainData;
set work.TrainData(where=(Purchase=1));
run;
data work.TestData;
set work.TrainData(where=(RandomID <= 10000));
run;
data work.TrainData;
set work.TrainData(where=(RandomID > 10000));
run;

ods graphics on;
proc logistic data= work.TrainData descending;
	class job marital education default housing loan contact mon poutcome;
	model Purchase = age job marital education default housing loan contact 
	mon days previous poutcome emp_var_rate cons_price_idx cons_conf_idx euribor3m nr_employed / selection=stepwise ctable outroc=work.ROCData;
	store work.LogisticModel;
run;

proc glmselect data=Work.RegTrainData plots=all ;
	class job marital education default housing loan contact mon poutcome;
	model Gross_Revenue = age job marital education default housing loan contact 
	mon days previous poutcome emp_var_rate cons_price_idx cons_conf_idx euribor3m nr_employed / selection=stepwise;
	store work.RegressionModel;
run;
ods graphics off;

/*Testing model on test set*/
proc plm source=RegressionModel;
	score data=work.TestData out=work.TestScored predicted = PredictedRevenue; 
run;

proc plm source=LogisticModel;
	score data=work.TestScored out=work.TestScored predicted = ProbabilityOfPurchase / ilink; 
run;
data work.TestScored;
set work.TestScored;
	label ProbabilityOfPurchase = "Probability of Purchase";
	label PredictedRevenue = "Predicted Revenue";
	if ProbabilityOfPurchase > .0875 then PredictedPurchase = 1;
   		else PredictedPurchase = 0;
	label PredictedPurchase = "Predicted Purchase";
	if PredictedPurchase = 1 then PredictedProfit = (PredictedRevenue-5);
		else PredictedProfit = 0;
	if PredictedPurchase = 1 and Purchase = 0 then PredictedProfit = -5;
	label PredictedProfit = "Predicted Profit";
run;
proc means data=TestScored sum mean;
var PredictedProfit;
class PredictedPurchase;
run;

/* Applying model to Validation Data Set*/
proc plm source=RegressionModel;
	score data=work.ValidationData out=work.ValidationScored predicted = PredictedRevenue; 
run;

proc plm source=LogisticModel;
	score data=work.ValidationScored out=work.ValidationScored predicted = ProbabilityOfPurchase / ilink; 
run;

data work.ValidationScored;
set work.ValidationScored;
	label ProbabilityOfPurchase = "Probability of Purchase";
	label PredictedRevenue = "Predicted Revenue";
	if ProbabilityOfPurchase > .0875 then PredictedPurchase = 1;
   		else PredictedPurchase = 0;
	label PredictedPurchase = "Predicted Purchase";
	if PredictedPurchase = 1 then PredictedProfit = (PredictedRevenue-5);
	if PredictedPurchase = 0 then PredictedProfit = 0;
	label PredictedProfit = "Predicted Profit";
run;

proc means data=ValidationScored sum mean;
var PredictedProfit;
class PredictedPurchase;
run;

/*Tried out tree for logistic regression, but found more success with the proc logistic method.
ods graphics on;

proc hpsplit data=TrainData seed=1000 plots=all;
class Purchase job marital education default housing loan contact mon poutcome;
model Purchase (event='1') = age job marital education default housing loan contact 
mon days previous poutcome emp_var_rate cons_price_idx cons_conf_idx euribor3m nr_employed;
grow entropy;
prune costcomplexity;
ID RandomID;
output out=LogTreeOut;

run;

ods graphics off; 

data TestTreeScored;
set TestData end=eof;
keep RandomNumber Actual Predicted Prob;
%include "Z:\Students\100133984\MKTG 130\Final Project\BuiltTreeCode.sas";
Actual = Purchase;
Prob = P_Purchase1;
Predicted = (Prob >= 0.1);
run;

data LogValidateScored;
set ValidationData end=eof;
keep ID Predicted Prob;
%include "Z:\Students\100133984\MKTG 130\Final Project\BuiltTreeCode.sas";
Prob = P_Purchase1;
Predicted = (Prob >= 0.1);
run;
*/
