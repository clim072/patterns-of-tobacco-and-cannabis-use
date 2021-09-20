
LIBNAME perm '/folders/myfolders/datasets/';


PROC PRINT DATA = perm.ndshs2019a (OBS = 100);
VAR hh_id strata ASGS3 ASGS4;
RUN;

PROC PRINT DATA = temp (OBS = 500);
VAR hh_id strata ASGS3 ASGS4 area;
RUN;

PROC UNIVARIATE DATA = temp;
	WHERE hhincome NOT IN (-1,-2,17);
	VAR hhincome;
RUN;

PROC UNIVARIATE DATA = temp;
	WHERE persincome NOT IN (-1,-2,16);
	VAR persincome;
RUN;



DATA temp (KEEP = 	weight_7 strata hh_id males
					sex dem5 b1 AgeGroup1460p ageg
					PersIncome persincomeall area
					MaritalStatus HHIncome hhincomeall education
					cob employmentall australia educationall
					L1a L1b L1c L2n L4 L5 L7 L7a L7b L8a L8b 
					L9_05 L9_06 L10 L2 L3 L12_01--L12_05 L14 L15
					joint bong vape edible 
					mar_lt mar_12m mar_30d freq cigarette ryo
					cigarillos cigar waterpipe pipe currentsmoker
					Diabetes Heart_Disease Hypertension Asthma
					Cancer Chronic depression anxiety k10rc
					RcntEcst RcntAmph RcntCoca RcntHall
					RcntInha RcntHero RcntKeta RcntGHB
					RcntInje RcntSCan RcntEPS RcntTran
					RcntSter RcntDone RcntOpioids alcohol);
	SET perm.ndshs2019a;
	
	*Clean Marijuana variables;
	IF L1c = 1 THEN mar_lt = 1; ELSE IF L1c = 2 THEN mar_lt = 0;
	IF L3 = 1 THEN mar_12m = 1; ELSE IF L3 = 2 THEN mar_12m = 0;
	IF L5 = 1 THEN mar_30d = 1; ELSE IF L5 = 2 THEN mar_30d = 0;
	
	IF mar_lt = 1 AND mar_12m = 0 AND mar_30d =	. THEN mar_30d = 0;
	IF mar_lt = 0 AND mar_12m = . AND mar_30d = . THEN DO;
		mar_12m = 0;
		mar_30d = 0;
	END;
	IF mar_lt = 1 AND mar_12m = 1 AND mar_30d =	. THEN mar_30d = 0;	
	IF mar_lt = 1 AND mar_12m =	. AND mar_30d = . THEN DO;
		mar_12m = 0;
		mar_30d = 0;
	END;
	IF mar_lt = . AND mar_12m =	. AND mar_30d =	0 THEN DO;
		mar_lt = 0;
		mar_12m = 0;
	END;		
	IF mar_lt = . AND mar_12m =	0 AND mar_30d =	. THEN DO;
		mar_lt = 0;
		mar_30d = 0;
	END;		
	
	IF mar_lt = . AND mar_12m = . THEN DO;
		mar_lt = 0;
		mar_12m = 0;
	END;		
	
	*Route of Administration;
	IF mar_12m = 0 AND L12_01 IN (.,-2,1,2) THEN L12_01 =2;
	IF mar_12m = 1 AND L12_01 IN (-2) THEN L12_01 = .;
	
	IF mar_12m = 0 AND L12_02 IN (.,-2,1,2) THEN L12_02 =2;
	IF mar_12m = 1 AND L12_02 IN (-2) THEN L12_02 = .;
	
	IF mar_12m = 0 AND L12_03 IN (.,-2,1,2) THEN L12_03 =2;
	IF mar_12m = 1 AND L12_03 IN (-2) THEN L12_03 = .;
	
	IF mar_12m = 0 AND L12_04 IN (.,-2,1,2) THEN L12_04 =2;
	IF mar_12m = 1 AND L12_04 IN (-2) THEN L12_04 = .;
	
	IF L12_01 = 1 THEN joint = 1; ELSE IF L12_01 = -2 THEN joint = 999; ELSE IF L12_01 = . THEN joint = 999;ELSE joint = 0;
	IF L12_02 = 1 THEN bong = 1; ELSE IF L12_02 = -2 THEN bong = 999; ELSE IF L12_02 = . THEN bong = 999; ELSE bong = 0;
	IF L12_03 = 1 THEN vape = 1; ELSE IF L12_03 = -2 THEN vape = 999; ELSE IF L12_03 = . THEN vape = 999; ELSE vape = 0;
	IF L12_04 = 1 THEN edible = 1; ELSE IF L12_04 = -2 THEN edible = 999; ELSE IF L12_04 = . THEN edible = 999; ELSE edible = 0;
	/*IF L12_05 = 1 THEN couse = 1; ELSE IF L12_05 = -2 THEN couse = 999; ELSE IF L12_05 = . THEN couse = 999;  ELSE couse = 0;
	*IF L12_01 = 1 OR L12_02 = 1 THEN smoke = 1; ELSE IF L12_01 = . OR L12_02 = . THEN smoke = 999; ELSE IF L12_01 = -2 OR L12_02 = -2 THEN smoke = 999; ELSE smoke = 0;*/
		
	*Frequency of use; 
	IF mar_lt = 1 AND mar_12m = 0 AND L7 = 4 THEN mar_12m = 1;
	IF L7 IN (1,2) THEN freq =1 ; 
	ELSE IF L7 IN (3,4) THEN freq = 2; 
	ELSE IF L7 IN (5) THEN freq = 3 ;
	ELSE IF L7 IN (-2) THEN freq = 999;
	ELSE IF L7 = . THEN freq = 999;
	
	IF L14 = -2 THEN L14 =.;
	IF L15 = -2 THEN L15 =.;
	
	*Tobacco;
	*If all questions are missing and not answered;
	IF 	D3 IN (.,-2) AND D6 IN (.,-2) AND D9 IN (.,-2) AND D12 IN (.,-2) AND D13 IN (.,-2) AND 
		D14_01 IN (.,-2) AND D14_02 IN (.,-2) AND D14_03 IN (.,-2) AND D14_04 IN (.,-2) THEN DO;
		D3 = 0;
		D6 = 0;
		D9 = 0;
		D12 = 0;
		D13 = 0;
		D14_01 = 0;
		D14_02 = 0;
		D14_03 = 0;
		D14_04 = 0;
	END;
		
	IF 	D6 = 3 THEN D6 = 0; * 3= no, never smoked daily;
	*Roa = 4: not at all;
	IF 	D12 IN (4) THEN D12 = 0;
	IF  D13 IN (4) THEN D13 = 0;
	IF  D14_01 IN (4) THEN D14_01 = 0;
	IF  D14_02 IN (4) THEN D14_02 = 0;
	IF  D14_03 IN (4) THEN D14_03 = 0;
	IF	D14_04 IN (4) THEN D14_04 = 0;
	*Roa = -2: not answered;
	IF 	D12 IN (-2) THEN D12 = .;
	IF  D13 IN (-2) THEN D13 = .;
	IF  D14_01 IN (-2) THEN D14_01 = .;
	IF  D14_02 IN (-2) THEN D14_02 = .;
	IF  D14_03 IN (-2) THEN D14_03 = .;
	IF	D14_04 IN (-2) THEN D14_04 = .;
		
	IF d6 = 1 THEN d3 = 1; *1 = yes, i smoke daily;
	IF d6 = -2 THEN d6 = .;
	IF d9 = -2 THEN d9 = .;
	
	*Definition of current smoker;
	IF D9 IN (1,2,3) OR (D6 = 1 AND D9 = .) THEN currentsmoker = 1; ELSE currentsmoker = 0;
	
	IF currentsmoker = 0 AND D12 >0 THEN currentsmoker = 1;
	IF currentsmoker = 0 AND D13 >0 THEN currentsmoker = 1;
	IF currentsmoker = 0 AND D14_01 >0 THEN currentsmoker = 1;
	IF currentsmoker = 0 THEN DO;
		IF D12 = . THEN d12 = 0;
		IF D13 = . THEN D13 = 0;
		IF D14_01 = . THEN D14_01 = 0;
		IF D14_02 = . THEN D14_02 = 0;
		IF D14_03 = . THEN D14_03 = 0;
		IF D14_04 = . THEN D14_04 = 0;
	END;
	
	*current smokers that did not answer any of the roa;
	IF 	currentsmoker = 1 AND D12 = . AND D13 = . AND D14_01 = . AND D14_02 = . AND 
		D14_03 = . AND D14_04= . THEN dna = 1; ELSE dna = 0; 
	
	IF currentsmoker = 1 AND dna = 0 THEN DO;
		IF D12 = . THEN d12 = 0;
		IF D13 = . THEN D13 = 0;
		IF D14_01 = . THEN D14_01 = 0;
		IF D14_02 = . THEN D14_02 = 0;
		IF D14_03 = . THEN D14_03 = 0;
		IF D14_04 = . THEN D14_04 = 0;
	END;
	
	IF currentsmoker = 1 THEN DO;
		IF D12 IN (1,2,3) THEN cigarette = 1; ELSE IF D12 = . THEN cigarette = 999; ELSE cigarette = 0; 
		IF D13 IN (1,2,3) THEN ryo = 1; ELSE IF D13 = . THEN ryo = 999; ELSE ryo = 0; 
		IF D14_01 IN (1,2,3) THEN cigarillos = 1; ELSE IF D14_01 = . THEN cigarillos = 999; ELSE cigarillos = 0; 	
		IF D14_02 IN (1,2,3) THEN cigar = 1; ELSE IF D14_02 = . THEN cigar = 999; ELSE cigar = 0; 	
		IF D14_03 IN (1,2,3) THEN waterpipe = 1; ELSE IF D14_03 = . THEN waterpipe = 999; ELSE waterpipe = 0; 	
		IF D14_04 IN (1,2,3) THEN pipe = 1; ELSE IF D14_04 = . THEN pipe = 999; ELSE pipe = 0; 
	END;
	IF currentsmoker = 0 THEN DO;
		cigarette = 0; 
		ryo = 0;
		cigarillos = 0;
		cigar = 0;
		waterpipe = 0;
		pipe =0;
	END;
	
	IF ASGS3 ~= . AND ASGS4 = . THEN area = ASGS3;
	ELSE IF ASGS4 ~= . AND ASGS3 = . THEN area = ASGS4;
	
	IF sex = 1 THEN males = 1; ELSE males = 0;
	
	IF education IN (1,2) THEN educationall =1;
	ELSE IF education IN (3,4) THEN educationall =2;
	ELSE IF education IN (5) THEN educationall= 3;
	 
	IF AgeGroup1460p IN (1,2) THEN ageg = 1;
	ELSE IF AgeGroup1460p IN (3) THEN ageg = 2;
	ELSE IF AgeGroup1460p IN (4,5) THEN ageg = 3;
	ELSE IF AgeGroup1460p IN (6) THEN ageg = 4;
	
	IF maritalstatus = -1 THEN maritalstatus = .;
	IF hhincome IN (-1,17,-2) THEN hhincome = .;
	IF persincome IN (-1,-2,16) THEN persincome = .;
	
	IF hhincome IN (1,2,3) THEN hhincomeall = 1;
	ELSE IF hhincome IN (4,5) THEN hhincomeall = 2;
	ELSE IF hhincome IN (6,7,8) THEN hhincomeall = 3;
	ELSE IF hhincome IN (9,10,11,13,15) THEN hhincomeall = 4;
	
	IF persincome IN (1,2,3,4) THEN persincomeall = 1;
	ELSE IF persincome IN (5,6,7) THEN persincomeall = 2;
	ELSE IF persincome IN (8,9,10) THEN persincomeall = 3;
	ELSE IF persincome IN (11,12,13,14) THEN persincomeall = 4;
	
	IF cob = 1 THEN australia = 1;
	ELSE IF cob IN (-2) THEN australia = .; 
	ELSE IF cob IN(2,3,4,5,6,7,8,9,10) THEN australia = 0;
	
	IF employmentall = -1 THEN employmentall = .;
	
	IF diabetes = 0 THEN diabetes = .;
	ELSE IF diabetes = 2 THEN diabetes = 0;
	
	IF Heart_Disease = 0 THEN Heart_Disease = .;
	ELSE IF Heart_Disease = 2 THEN Heart_Disease = 0;
	
	IF Hypertension = 0 THEN Hypertension = .;
	ELSE IF Hypertension = 2 THEN Hypertension = 0;
	
	IF Asthma = 0 THEN Asthma = .;
	ELSE IF Asthma = 2 THEN Asthma = 0;
	
	IF Cancer = 0 THEN Cancer = .;
	ELSE IF Cancer = 2 THEN Cancer= 0;
	
	IF Chronic = 0 THEN Chronic= .;
	ELSE IF Chronic = 2 THEN Chronic = 0;
	
	IF depression = 0 THEN depression = .;
	ELSE IF depression = 2 THEN depression = 0;
	
	IF anxiety = 0 THEN anxiety = .;
	ELSE IF anxiety = 2 THEN anxiety = 0;
	
	IF RcntEcst = -1 THEN RcntEcst = .;
	ELSE IF RcntEcst = 2 THEN RcntEcst = 0;
	
	IF RcntAmph = -1 THEN RcntAmph = .;
	ELSE IF RcntAmph = 2 THEN RcntAmph = 0;
	
	IF RcntCoca = -1 THEN RcntCoca = .;
	ELSE IF RcntCoca = 2 THEN RcntCoca = 0;
	
	IF RcntHall = -1 THEN RcntHall = .;
	ELSE IF RcntHall = 2 THEN RcntHall = 0;
	
	IF RcntInha = -1 THEN RcntInha = .;
	ELSE IF RcntInha = 2 THEN RcntInha = 0;
	
	IF RcntKeta = -1 THEN RcntKeta = .;
	ELSE IF RcntKeta = 2 THEN RcntKeta = 0;
	
	IF RcntOpioids = -1 THEN RcntOpioids = .;
	ELSE IF RcntOpioids = 2 THEN RcntOpioids = 0;
	
	IF E5 = -2 THEN alcohol = .;
	ELSE IF E5 = 1 THEN alcohol = 1;
	ELSE IF E5 = 2 THEN alcohol = 0;
	
RUN;

**------------------------------MPLUS dataset----------------------------------;
DATA temp2 (KEEP = 	hh_id weight_7 joint bong vape edible 
					cigarette ryo cigarillos cigar waterpipe 
					pipe);
	SET temp;
RUN;


**----------Table 1--------------------**;
PROC SURVEYFREQ DATA = temp;
	STRATA strata;
	WEIGHT weight_7;
	TABLES mar_12m currentsmoker joint bong vape edible cigarette ryo cigarillos cigar waterpipe pipe/ROW;
RUN;

PROC SURVEYFREQ DATA = temp;
	STRATA strata;
	WEIGHT weight_7;
	WHERE mar_12m =   1;
	TABLES joint bong vape edible cigarette ryo cigarillos cigar waterpipe pipe/ROW;
RUN;

PROC SURVEYFREQ DATA = temp;
	STRATA strata;
	WEIGHT weight_7;
	WHERE currentsmoker =   1;
	TABLES joint bong vape edible cigarette ryo cigarillos cigar waterpipe pipe/ROW;
RUN;

DATA perm.temp;	SET temp; RUN;
PROC SORT DATA = perm.lca;	BY HH_ID; RUN;
PROC PRINT DATA = perm.lca (OBS = 500) ; RUN;
DATA lca (KEEP = c	bchw1	bchw2	bchw3	bchw4	HH_ID); SET perm.lca; RUN;

DATA perm.final;
	MERGE temp lca;
	BY hh_id;
RUN;

PROC EXPORT DATA=perm.final
    OUTFILE="/folders/myfolders/final.csv"
    DBMS=csv;
RUN;


PROC SURVEYFREQ DATA = perm.final;
	STRATA strata;
	WEIGHT weight_7;
	TABLES males ageg maritalstatus
		    hhincomeall
		    persincomeall
		    area
		    educationall
		   	australia
		   	employmentall
		   	Diabetes Heart_Disease Hypertension
		   	Asthma Cancer Chronic k10rc  
		   	RcntEcst RcntAmph RcntCoca RcntHall
		   	RcntInha RcntOpioids alcohol /ROW;
	TABLES c*(males ageg maritalstatus
		    hhincomeall
		    persincomeall
		    area
		    educationall
		   	australia
		   	employmentall
		   	Diabetes Heart_Disease Hypertension
		   	Asthma Cancer Chronic k10rc  
		   	RcntEcst RcntAmph RcntCoca RcntHall
		   	RcntInha RcntOpioids alcohol )/ROW;
RUN;

*-------------------IMPUTE MISSING ------------------------------------------;	
PROC TRANSPOSE DATA=lca OUT=lca_t;
   BY hh_id;
   VAR bchw1-bchw4;
RUN;
	
*Dataset for analysis;
DATA final_long1;
	MERGE lca_t   final  ;
	BY HH_ID;
RUN;
	   	

PROC FREQ DATA =final_long1 ; TABLES males CURF_age
		   	maritalstatus
		    hhincome 
		    education
		   	australia
		   	employmentall
		   	Diabetes Heart_Disease Hypertension
		   	Asthma Cancer Chronic depression anxiety
		   	k10rc  
		   	RcntEcst RcntAmph RcntCoca RcntHall
		   	RcntInha RcntKeta RcntOpioids alcohol c;RUN;

PROC FREQ DATA = final_long1;
TABLE weight_7;
RUN;

*Impute 1 (binary);
PROC MI DATA = final_long1 NIMPUTE= 2 OUT = MI_NEW SEED = 88888  ROUND = 1 ;
	VAR 	males CURF_age
		   	never_mar divorced /*married*/
		    /*hhincome_high*/ hhincome_highavg hhincome_lowavg hhincome_low
		    highsch posthighsch /*tertiary*/
		   	australia
		   	notinlabourf unemployed /*currentemployed*/
		   	Diabetes Heart_Disease Hypertension
		   	Asthma Cancer Chronic depression anxiety
		   	/*k10rc_low*/ k10rc_mod k10rc_high 
		   	RcntEcst RcntAmph RcntCoca RcntHall
		   	RcntInha RcntKeta RcntOpioids alcohol c ;
   	FREQ weight_7;
RUN;

*Impute 2 (orginal categorical) -- use this;
PROC MI DATA = final_long1 NIMPUTE= 10 OUT = MI_NEW SEED = 88888  ROUND = 1 ;
	VAR 	 males CURF_age 
		   	maritalstatus
		    hhincome  
		    education
		    australia
		   	employmentall
		   	Diabetes Heart_Disease Hypertension
		   	Asthma Cancer Chronic depression anxiety
		   	k10rc  
		   	RcntEcst RcntAmph RcntCoca RcntHall
		   	RcntInha RcntKeta RcntOpioids alcohol c  ;
   	FREQ weight_7;
RUN;

PROC FREQ DATA =  final_long1;
TABLES males CURF_age
		   	maritalstatus
		    hhincome 
		    education;
		   RUN;
PROC FREQ DATA = MI_NEW;
TABLES males CURF_age
		   	maritalstatus
		    hhincome 
		    education;
		   RUN;
PROC FREQ DATA = MI_NEW;
	TABLES males CURF_age
		   	maritalstatus
		    hhincome 
		    education
		   	australia
		   	 employmentall
		   	Diabetes Heart_Disease Hypertension
		   	Asthma Cancer Chronic depression anxiety
		   	k10rc  
		   	RcntEcst RcntAmph RcntCoca RcntHall
		   	RcntInha RcntKeta RcntOpioids alcohol c;
RUN;
 
DATA MI_NEW;
	SET MI_NEW;
	
	never_mar = 0; divorced = 0; married = 0;
	IF maritalstatus = 1 THEN never_mar = 1; 
	IF maritalstatus = 2 THEN divorced = 1;
	IF maritalstatus IN (3,4) THEN married = 1;
	
	hhincome_high = 0; hhincome_highavg = 0; hhincome_lowavg = 0; hhincome_low = 0;
	IF hhincome IN (1,2,3) THEN hhincome_high = 1;
	IF hhincome IN (4,5) THEN hhincome_highavg = 1;
	IF hhincome IN (6,7,8) THEN hhincome_lowavg = 1;
	IF hhincome IN (9,10,11,12,13,15) THEN hhincome_low = 1;
	
	highsch = 0; posthighsch = 0; tertiary = 0;
	IF education IN (1,2) THEN highsch = 1;
	IF education IN (3,4) THEN posthighsch = 1;
	IF education IN (5) THEN tertiary = 1;
	
	notinlabourf = 0; unemployed = 0; currentemployed = 0;
	IF employmentall IN (0,1) THEN notinlabourf = 1;
	IF employmentall = 2 THEN unemployed = 1;
	IF employmentall = 3 THEN currentemployed = 1;
	
	k10rc_low = 0; k10rc_mod = 0; k10rc_high = 0;
	IF k10rc = 1 THEN k10rc_low = 1;  
	IF k10rc = 2 THEN k10rc_mod = 1;
	IF k10rc = 3 THEN k10rc_high = 1;
RUN;	
	
PROC LOGISTIC DATA = MI_NEW;
	BY _imputation_ ; 
	CLASS c(ref = '2') males (ref = '0') 
		  age18below (ref = '0') age19_29 (ref = '0') age30_44 (ref = '0') 
		  age45_59 (ref = '0') never_mar (ref = '0') divorced(ref = '0') 
		  hhincome_highavg (ref = '0') hhincome_lowavg (ref = '0') hhincome_low(ref = '0') 
		  highsch (ref = '0') posthighsch (ref = '0') 
		  notinlabourf (ref = '0') unemployed (ref = '0')
		  Diabetes (ref = '0') Heart_Disease (ref = '0') Hypertension (ref = '0') 
		  Asthma (ref = '0') Cancer (ref = '0') Chronic (ref = '0') depression (ref = '0') 
		  anxiety(ref = '0') k10rc_mod (ref = '0') k10rc_high (ref = '0') 
		   RcntEcst (ref = '0') RcntAmph (ref = '0') RcntCoca (ref = '0') RcntHall (ref = '0') 
		   RcntInha (ref = '0') RcntKeta (ref = '0') RcntOpioids (ref = '0') alcohol (ref = '0') /PARAM = ref;
	 
	MODEL c = males age18below age19_29 age30_44 age45_59 /*age60p*/
		   never_mar divorced /*married*/
		   /*hhincome_high*/ hhincome_highavg hhincome_lowavg hhincome_low
		   highsch posthighsch /*tertiary*/ australia
		   notinlabourf unemployed /*currentemployed*/
		   Diabetes Heart_Disease Hypertension
		   Asthma Cancer Chronic depression anxiety
		   /*k10rc_low*/ k10rc_mod k10rc_high
		   RcntEcst RcntAmph RcntCoca RcntHall
		   RcntInha RcntKeta RcntOpioids alcohol 
			  col1 / link = glogit;
	ODS OUTPUT ParameterEstimates=a1;
RUN;

DATA a1_class1;
	SET A1;
	IF response = 1;
RUN;
PROC MIANALYZE PARMS = a1_class1;
	MODELEFFECTS intercept males age18below age19_29 age30_44 age45_59  
		   never_mar divorced  
		    hhincome_highavg hhincome_lowavg hhincome_low
		   highsch posthighsch  australia
		   notinlabourf unemployed  
		   Diabetes Heart_Disease Hypertension
		   Asthma Cancer Chronic depression anxiety
		   k10rc_mod k10rc_high
		   RcntEcst RcntAmph RcntCoca RcntHall
		   RcntInha RcntKeta RcntOpioids alcohol;
*	STDERR stderr;
	ODS OUTPUT parameterestimates = mianalyze_parms;
	 
RUN;
DATA OR;
	SET mianalyze_parms;
	OR=exp(estimate);
	LCL_OR=exp(LCLMean);
	UCL_OR=exp(UCLMean);
RUN;

proc print;

*var parm a OR LCL_OR UCL_OR waldchisq probchisq;

run;

DATA a1_class1;
	SET A1;
	IF response = 3;
RUN;
PROC MIANALYZE PARMS = a1_class1;
	MODELEFFECTS intercept males age18below age19_29 age30_44 age45_59  
		   never_mar divorced  
		    hhincome_highavg hhincome_lowavg hhincome_low
		   highsch posthighsch  australia
		   notinlabourf unemployed  
		   Diabetes Heart_Disease Hypertension
		   Asthma Cancer Chronic depression anxiety
		   k10rc_mod k10rc_high
		   RcntEcst RcntAmph RcntCoca RcntHall
		   RcntInha RcntKeta RcntOpioids alcohol;
*	STDERR stderr;
	 
RUN;

DATA a1_class1;
	SET A1;
	IF response = 4;
RUN;
PROC MIANALYZE PARMS = a1_class1;
	MODELEFFECTS intercept males age18below age19_29 age30_44 age45_59  
		   never_mar divorced  
		    hhincome_highavg hhincome_lowavg hhincome_low
		   highsch posthighsch  australia
		   notinlabourf unemployed  
		   Diabetes Heart_Disease Hypertension
		   Asthma Cancer Chronic depression anxiety
		   k10rc_mod k10rc_high
		   RcntEcst RcntAmph RcntCoca RcntHall
		   RcntInha RcntKeta RcntOpioids alcohol;
*	STDERR stderr;
	 
RUN;

DATA final;
	SET perm.final;
	IF hhincome IN (1,2,3) THEN hhincome_4cat = 1;
	ELSE IF hhincome IN (4,5) THEN  hhincome_4cat = 2;
	ELSE IF hhincome IN (6,7,8) THEN hhincome_4cat = 3;
	ELSE IF hhincome IN (9,10,11,12,13,15) THEN hhincome_4cat = 4;
	
	IF education IN (1,2,3) THEN highschless = 1;
	ELSE IF education IN (4,5) THEN highschless = 0;
	

RUN;

PROC SURVEYFREQ DATA = perm.final;
	STRATA strata;
	WEIGHT weight_7;
	TABLES c*(males maritalstatus hhincome_4cat highschless australia
	employmentall diabetes heart_disease hypertension asthma cancer chronic depression anxiety k10rc rcntecst rcntamph rcntcoca rcnthall rcntinha rcntketa rcntopioids alcohol) /ROW;
RUN;

PROC SORT DATA = PERM.final; BY c; RUN;
PROC SURVEYMEANS DATA = PERM.final;
	STRATA strata;
	WEIGHT weight_7;
	BY c;
	VAR curf_age  ;
RUN;
