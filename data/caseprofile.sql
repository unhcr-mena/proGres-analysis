USE [DWH]
GO

SET ANSI_NULLS ON
GO

SET QUOTED_IDENTIFIER ON
GO

CREATE VIEW [dbo].[caseprofile]
AS
SELECT                I.CaseNo,
                      I.CoO CountryOrigin,
                      I.CoA CountryAsylum,
                      DATENAME(mm, I.ArrivalDate) Montharrival,
                      DATENAME(yyyy, I.ArrivalDate) YearArrival,
                      I.[CoO_L1] cool1,
                      I.[CoO_L2] cool2,
                      I.[CoO_L3] cool3,
                      I.[CoA_L1]  coal1,
                      I.[CoA_L2] coal2,
                      I.[CoA_L3] coal3,
                      I.MarriageStatusCode dem_marriage,
                      I.IndividualAge dem_age,
                      I.Sex dem_sex,
                      I.Ethnicity dem_ethn,
                      I.Religion dem_religion,
                      I.NationalityCode dem_birth_country,
                      I.EducationLevelCode edu_highest

 FROM  [DWH].[dbo].[T_AllIndividuals] I

 LEFT JOIN
      (SELECT     CaseNo,
           COUNT(DISTINCT IndividualGUID) Num_Inds,
           AVG(IndividualAge) AVG_Age,
           STDEV(IndividualAge) STDEV_Age,
            Count(DISTINCT CASE WHEN (IndividualAge < 15) THEN (IndividualGUID) ELSE (NULL) END) Child_0_14,
            Count(DISTINCT CASE WHEN (IndividualAge < 18) THEN (IndividualGUID) ELSE (NULL) END) Child_0_17,
            Count(DISTINCT CASE WHEN (IndividualAge < 19) THEN (IndividualGUID) ELSE (NULL) END) Child_0_18,
            Count(DISTINCT CASE WHEN (IndividualAge < 15) THEN (IndividualGUID) ELSE (NULL) END) * 100 / COUNT(DISTINCT IndividualGUID)  percentage_0_14,
            Count(DISTINCT CASE WHEN (IndividualAge < 18) THEN (IndividualGUID) ELSE (NULL) END) * 100 / COUNT(DISTINCT IndividualGUID)  percentage_0_17,
            Count(DISTINCT CASE WHEN (IndividualAge < 19) THEN (IndividualGUID) ELSE (NULL) END) * 100 / COUNT(DISTINCT IndividualGUID) percentage_0_18
      FROM [DWH].[dbo].[T_AllIndividuals]
           WHERE     Current_process_Status IN ('a')
              GROUP BY CaseNo) AS Cal_1
              ON I.CaseNo = Cal_1.CaseNo
    LEFT JOIN
       (SELECT DISTINCT CaseNo,
       PERCENTILE_CONT(0.5) WITHIN GROUP (ORDER BY IndividualAge) OVER (PARTITION BY CaseNo) AS Median_Age
        FROM      [DWH].[dbo].[T_AllIndividuals]
         WHERE     (Current_process_Status = 'A')) Cal_2
          ON I.CaseNo = Cal_2.CaseNo
WHERE     (I.Current_process_Status = 'A') AND I.Relationship = 'PA'

GO

--SELECT     count(IndividualGUID)   FROM [DWH].[dbo].[T_AllIndividuals]

--SELECT     count(CaseNo)   FROM [DWH].[dbo].[caseprofile]