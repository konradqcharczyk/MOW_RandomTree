#IBM
percent = 0.8
data_file = "MartialStatus.csv"

all_data = read.csv(data_file, header = TRUE)

targ <- "MaritalStatus"
preds <- c("ď.żAge","Attrition" ,"BusinessTravel","DailyRate","Department" ,"DistanceFromHome",
           "Education","EducationField", "EmployeeCount", "EmployeeNumber", "EnvironmentSatisfaction",  "Gender"  ,
           "HourlyRate","JobInvolvement","JobLevel", "JobRole", "JobSatisfaction", "MonthlyIncome", "MonthlyRate"   ,           "NumCompaniesWorked" ,
           "Over18", "OverTime", "PercentSalaryHike", "PerformanceRating", "RelationshipSatisfaction", "StandardHours",
           "StockOptionLevel", "TotalWorkingYears", "TrainingTimesLastYear", "WorkLifeBalance", "YearsAtCompany" ,          "YearsInCurrentRole",
           "YearsSinceLastPromotion","YearsWithCurrManager")
ntrees <- 10

k_cross_validation(4, all_data, targ, preds, 0.7,  ntrees, 0, 30, 10, 30, TRUE, 0.8)

#FIFA
percent = 0.8
data_file = "Fifa.csv"

all_data = read.csv(data_file, header = TRUE)

targ <- "Overall"
preds <- c("Age","Nationality","Potential","Club","Special","Preferred.Foot","International.Reputation","Weak.Foot","Skill.Moves","Work.Rate","Body.Type",
               "Position","Jersey.Number",
           "Loaned.From"    , "Contract.Valid.Until", "Crossing"     ,"Finishing"    ,
           "HeadingAccuracy", "ShortPassing"        ,"Volleys"      ,"Dribbling"    ,
           "Curve"          , "FKAccuracy"          ,"LongPassing"  ,"BallControl"  ,
           "Acceleration"   , "SprintSpeed"         ,"Agility"      ,"Reactions"    ,
           "Balance"        , "ShotPower"           ,"Jumping"      ,"Stamina"      ,
           "Strength"       , "LongShots"           ,"Aggression"   ,"Interceptions",
           "Positioning"    , "Vision"              ,"Penalties"    ,"Composure"    ,
           "Marking"        , "StandingTackle"      ,"SlidingTackle","GKDiving"     ,
           "GKHandling"     , "GKKicking"           ,"GKPositioning","GKReflexes" )

ntrees <- 5

k_cross_validation(4, all_data, targ, preds, 0.7,  ntrees, 0.005, 100, 3, 30, FALSE)
