
-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -
-- DATA MINING TABLES CREATION:

---- Table for Case profile -- pivot of specific needs and list of relevant events
-- caseprofile
-- T_SPneedsBreak
-- T_EventsSpecific

---- Merging Event, Specific need and Individual Information 
-- T_EventsSpecificInd
-- T_EventsSpecificIndSPneeds

---- Aggregation at case level
-- T_EventsSpecificPA
-- T_SPneedsCaseLevel
-- T_EventsSpecificIndSPneedsInd

-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -
/*****************************************************************************************************
 Author:  Edouard LEGOUPIL
  Email:  legoupil@unhcr.org
   Date:  Feb 2016
Purpose:  Creating Summary Tables for data mining with R
DB Vers:  proGres Datawarehouse MENA
Version:  1.0 - View Creation

*****************************************************************************************************/


DROP TABLE [caseprofile];

SELECT I.CaseNo,
    I.CoO CountryOrigin,
    I.[CoO_L1] cool1,
    I.[LocationLevel1ID] cool1id,
    I.[CoO_L2] cool2,
    I.[LocationLevel2ID] cool2id,
    I.[CoO_L3] cool3,
    I.[LocationLevel3ID] cool3id,
    I.[LocationLevel4ID] cool4id,   
    I.CoA CountryAsylum,
    I.[CoA_L1] coal1,
    I.[CoA_LocationLevel1ID] coal1id,
    I.[CoA_L2] coal2,
    I.[CoA_LocationLevel2ID] coal2id,
    I.[CoA_L3] coal3, 
    I.[CoALocationLevel3ID] coal3id,
    I.[CoA_LocationLevel4ID] coal4id,
    Cal_1.Num_Inds,
    Cal_1.Child_0_14,
    Cal_1.Youth_15_17,
    Cal_1.Work_15_64,
    Cal_1.Eldern_65,
    Cal_1.Male,
    Cal_1.Female,
    Cal_1.NOGender,
 
    Cal_1.AVG_Age,
    Cal_1.STDEV_Age,
    DATENAME(mm, I.ArrivalDate) Montharrival,
    DATENAME(yyyy, I.ArrivalDate) YearArrival,
    I.MarriageStatusCode dem_marriage,
    I.Relationship dem_relation,
    I.IndividualAge dem_age,
    I.AgeGroup dem_agegroup,
    I.Sex dem_sex,
    I.Ethnicity dem_ethn,
    I.Religion dem_religion,
    I.NationalityCode dem_birth_country,
    I.OccupationCode occupationcode,
    I.Occupation occupation,
    I.EducationLevelCode edu_highest,
    I.RefStatus,
    DATENAME(yyyy, RefugeeStatusDate) RefugeeStatusDate,
    I.RefStatCategory

    INTO [caseprofile]

FROM[DWH].[dbo].[T_AllIndividuals] I

LEFT JOIN
    (SELECT CaseNo,
        COUNT(DISTINCT IndividualGUID) Num_Inds,
        AVG(IndividualAge) AVG_Age,
        STDEV(IndividualAge) STDEV_Age,
        Count( CASE WHEN(IndividualAge < 15) THEN(IndividualGUID) ELSE(NULL) END) Child_0_14,
        Count( CASE WHEN(IndividualAge < 19 AND IndividualAge > 14) THEN(IndividualGUID) ELSE(NULL) END) Youth_15_17,
        Count( CASE WHEN(IndividualAge < 65 AND IndividualAge > 14) THEN(IndividualGUID) ELSE(NULL) END) Work_15_64,
        Count( CASE WHEN(IndividualAge > 64) THEN(IndividualGUID) ELSE(NULL) END) Eldern_65,
        Count( CASE WHEN(Sex = 'M') THEN(Sex) ELSE(NULL) END) Male,
        Count( CASE WHEN(Sex = 'F') THEN(Sex) ELSE(NULL) END) Female,
	   Count( CASE WHEN(Sex not in  ('F','M')) THEN('Empty')  END) NOGender
	    FROM[DWH].[dbo].[T_AllIndividuals] WHERE Current_process_Status IN('a') GROUP BY CaseNo) AS Cal_1
ON I.CaseNo = Cal_1.CaseNo

WHERE I.Current_process_Status = 'A' AND I.Relationship = 'PA'


-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -
-- --Using pivot functions
DROP TABLE T_SPneedsBreak;
-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -
SELECT *

    INTO T_SPneedsBreak

FROM
    (SELECT A.IndividualGUID, A.[IndividualID], A.[SPNeeds], A.[SPNeeds] as code, B.[CaseNo] --Colums to pivot
    FROM[T_SPneedswithdetails] A LEFT OUTER JOIN dbo.T_AllIndividuals B ON A.IndividualID = B.IndividualID WHERE[VulnerabilityActive] = '1'
        AND SPNeeds in (
            'CR', 'CR-AF', 'CR-CC', 'CR-CH', 'CR-CL', 'CR-CP', 'CR-CS',
            'CR-LO', 'CR-LW', 'CR-MS', 'CR-NE', 'CR-SE', 'CR-TP', 'DS', 'DS-BD', 'DS-DF', 'DS-MM', 'DS-MS', 'DS-PM',
            'DS-PS', 'DS-SD', 'ER', 'ER-FR', 'ER-MC', 'ER-NF', 'ER-OC', 'ER-SC', 'ER-UR', 'FU', 'FU-FR', 'FU-TR',
            'LP', 'LP-AF', 'LP-AN', 'LP-AP', 'LP-BN', 'LP-CR', 'LP-DA', 'LP-DN', 'LP-DO', 'LP-DP', 'LP-DT', 'LP-ES',
            'LP-FR', 'LP-IH', 'LP-LS', 'LP-MD', 'LP-MM', 'LP-MS', 'LP-NA', 'LP-ND', 'LP-PV', 'LP-RD', 'LP-RP', 'LP-RR',
            'LP-ST', 'LP-TA', 'LP-TC', 'LP-TD', 'LP-TO', 'LP-TR', 'LP-UP', 'LP-VA', 'LP-VF', 'LP-VO', 'LP-VP', 'LP-WP',
            'PG', 'PG-HR', 'PG-LC', 'SC', 'SC-CH', 'SC-FC', 'SC-IC', 'SC-NC', 'SC-SC', 'SC-UC', 'SC-UF', 'SC-UM',
            'SM', 'SM-AD', 'SM-CC', 'SM-CI', 'SM-DP', 'SM-MI', 'SM-MN', 'SM-OT', 'SP', 'SP-CG', 'SP-GP', 'SP-PT',
            'SV', 'SV-FM', 'SV-GM', 'SV-HK', 'SV-HP', 'SV-SS', 'SV-VA', 'SV-VF', 'SV-VO', 'TR', 'TR-HO', 'TR-PI',
            'TR-WV', 'WR', 'WR-GM', 'WR-HR',
            'WR-LC', 'WR-PY', 'WR-SF', 'WR-UW', 'WR-WF', 'WR-WR')
    )
as sourcetable

pivot(
    COUNT(IndividualGUID) --Pivot on this column

    for [SPNeeds] --Make colum where SPNeeds is in one of these.

    in([CR], [CR - AF], [CR - CC], [CR - CH], [CR - CL], [CR - CP], [CR - CS], [CR - LO], [CR - LW], [CR - MS], [CR - NE], [CR - SE], [CR - TP], [DS], [DS - BD], [DS - DF], [DS - MM], [DS - MS], [DS - PM], [DS - PS], [DS - SD], [ER], [ER - FR], [ER - MC], [ER - NF], [ER - OC], [ER - SC], [ER - UR], [FU], [FU - FR], [FU - TR], [LP], [LP - AF], [LP - AN], [LP - AP], [LP - BN], [LP - CR], [LP - DA], [LP - DN], [LP - DO], [LP - DP], [LP - DT], [LP - ES], [LP - FR], [LP - IH], [LP - LS], [LP - MD], [LP - MM], [LP - MS], [LP - NA], [LP - ND], [LP - PV], [LP - RD], [LP - RP], [LP - RR], [LP - ST], [LP - TA], [LP - TC], [LP - TD], [LP - TO], [LP - TR], [LP - UP], [LP - VA], [LP - VF], [LP - VO], [LP - VP], [LP - WP], [PG], [PG - HR], [PG - LC], [SC], [SC - CH], [SC - FC], [SC - IC], [SC - NC], [SC - SC], [SC - UC], [SC - UF], [SC - UM], [SM], [SM - AD], [SM - CC], [SM - CI], [SM - DP], [SM - MI], [SM - MN], [SM - OT], [SP], [SP - CG], [SP - GP], [SP - PT], [SV], [SV - FM], [SV - GM], [SV - HK], [SV - HP], [SV - SS], [SV - VA], [SV - VF], [SV - VO], [TR], [TR - HO], [TR - PI], [TR - WV], [WR], [WR - GM], [WR - HR], [WR - LC], [WR - PY], [WR - SF], [WR - UW], [WR - WF], [WR - WR])
)
as CountSpecificNeeds--Pivot table alias


--- List events of specific interest for analysis and rank them by date in case there woudl be duplicate

-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -
DROP TABLE[T_EventsSpecific];
-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -

select * ,
    RECORDRANK = RANK() OVER(PARTITION BY[IndividualID], 
    [EventID] ORDER BY[EventLogEffectiveDate] DESC),
    RECORDRANKRESULT = RANK() OVER(PARTITION BY[IndividualID], 
    [ResultID] ORDER BY[EventLogResultEffectiveDate] DESC)
    
into T_EventsSpecific
from

    (SELECT
    A.[IndividualID], 
    --A.[CaseNo],
    A.[EventID], A.[ResultID],
    CONVERT(VARCHAR(10), A.[EventLogEffectiveDate], 103) as EventLogEffectiveDate, 
    A.[EventLogstatus],
    isnull(CONVERT(VARCHAR(10), A.[EventLogResultEffectiveDate], 103), CONVERT(VARCHAR(10), A.[EventLogEffectiveDate], 103)) as EventLogResultEffectiveDate, 
    A.[ReasonCode] as code,
    B.[EventReasonText] as ReasonCode

    FROM[DWH].[dbo].[T_Events] A
    LEFT JOIN  
    (SELECT  [EventReasonCode]
      ,[EventReasonLanguageCode]
      ,[EventReasonText]
  FROM [proGres].[dbo].[codeEventReasonText] WHERE [EventReasonLanguageCode] = 'ENG') B ON B.[EventReasonCode]= A.[ReasonCode]

    WHERE[EventLogstatus] < > 'x'
    --AND[ResultID] in ('RST04', 'RST06', 'RST19') 
    AND LEFT([EventID], 3) = 'RST'
    OR[EventID] = 'REG38'
    OR[EventID] = 'VOL26'

)
AS t
ORDER BY[IndividualID], [EventID]


-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -
-- -Now reformatting from temp table
-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -

-- -Information at the individual level & computing delays between events

-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -

DROP TABLE T_EventsSpecificInd;
-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -

SELECT
J.IndividualID,
    J.CaseNo,
    J.CurrentSize,
    J.TotalSize,
    J.Current_process_Status,
    L.EventLogEffectiveDate AS rst01_considered,
    L.ReasonCode AS rst01_Reasonconsidered,
    DATEDIFF(day, J.RegisteredOn, L.EventLogEffectiveDate) as registered_rst_01,
    M.EventLogResultEffectiveDate AS rst04_notqualified,
    M.ReasonCode AS rst04_Reasonnotqualified,
    DATEDIFF(day, L.EventLogEffectiveDate, M.EventLogResultEffectiveDate) as rst04_01,
    C.EventLogResultEffectiveDate AS rst06_Interview,
    C.ReasonCode AS rst06_ReasonInterview,
    D.EventLogEffectiveDate AS rst09_Assessment_Complete,
    D.ReasonCode AS rst09_ReasonAssessment_Complete,
    DATEDIFF(day, L.EventLogEffectiveDate, D.EventLogEffectiveDate) as rst09_01,
    G.EventLogEffectiveDate AS rst18_CaseSubmitted,
    G.ReasonCode AS rst18_ReasonCaseSubmitted,
    DATEDIFF(day, D.EventLogEffectiveDate, G.EventLogEffectiveDate) as rst18_09,
    H.EventLogEffectiveDate AS rst21_CaseResbmitted,
    H.ReasonCode AS rst21_ReasonCaseResbmitted,
    DATEDIFF(day, G.EventLogEffectiveDate, H.EventLogEffectiveDate) as rst21_18,
    F.EventLogResultEffectiveDate AS rst19_CaseAccepted,
    F.ReasonCode AS rst19_ReasonCaseAccepted,
    DATEDIFF(day, G.EventLogEffectiveDate, F.EventLogResultEffectiveDate) as rst19_18,
    E.EventLogEffectiveDate AS rst43_Referred_to_Hub,
    E.ReasonCode AS rst43_ReasonReferred_to_Hub,
    K.EventLogResultEffectiveDate AS rst33_DepartureConfirmed,
    K.ReasonCode AS rst33_ReasonDepartureConfirmed,
    DATEDIFF(day, F.EventLogResultEffectiveDate, K.EventLogResultEffectiveDate) as rst33_19,
    N.EventLogEffectiveDate AS reg38_SpontaneousDeparture,
    N.ReasonCode AS reg38_ReasonSpontaneousDeparture,
    DATEDIFF(day, J.RegisteredOn, N.EventLogEffectiveDate) as registered_Spontaneousdepart,
    O.EventLogResultEffectiveDate AS vol26_VolrepDepartureConfirmed,
    O.ReasonCode AS vol26_ReasonVolrepDepartureConfirmed,
    DATEDIFF(day, J.RegisteredOn, O.EventLogResultEffectiveDate) as registered_VolDepart,
    CONVERT(VARCHAR(10), J.CaseCreatedOn, 103) AS CaseCreatedOn,
    CONVERT(VARCHAR(10), J.ArrivalDate, 103) AS ArrivalDate,
    CONVERT(VARCHAR(10), J.RegisteredOn, 103) AS RegisteredOn,
    CONVERT(VARCHAR(10), J.RefugeeStatusDate, 103) AS RefugeeStatusDate,
    J.CoO, J.CoA,
    J.NationalityCode,
    J.Relationship,
    J.Sex,
    CONVERT(VARCHAR(10), J.DOB, 103) AS DOB,
    J.MarriageStatusCode,
    J.IndividualAge,
    J.AgeGroup,
    J.Religion,
    J.Ethnicity,
    J.Occupation,
    J.OccupationCode,
    J.EducationLevelCode,
    J.HasSPNeed,
    J.RefStatus,
    J.RefStatCategory

into T_EventsSpecificInd
FROM dbo.T_AllIndividuals AS J

LEFT OUTER JOIN
    (SELECT IndividualID, EventLogResultEffectiveDate, ReasonCode, RECORDRANK FROM dbo.T_EventsSpecific AS CE WHERE(ResultID = 'RST06') AND(RECORDRANKRESULT = 1)) AS C ON J.IndividualID = C.IndividualID

LEFT OUTER JOIN
    (SELECT IndividualID, EventLogEffectiveDate, ReasonCode, RECORDRANK FROM dbo.T_EventsSpecific AS DE WHERE(EventID = 'RST09') AND(RECORDRANK = 1)) AS D ON J.IndividualID = D.IndividualID

LEFT OUTER JOIN
    (SELECT IndividualID, EventLogEffectiveDate, ReasonCode, RECORDRANK FROM dbo.T_EventsSpecific AS EE WHERE(EventID = 'RST43') AND(RECORDRANK = 1)) AS E ON J.IndividualID = E.IndividualID

LEFT OUTER JOIN
    (SELECT IndividualID, EventLogResultEffectiveDate, ReasonCode, RECORDRANK FROM dbo.T_EventsSpecific AS FE WHERE(ResultID = 'RST19') AND(RECORDRANKRESULT = 1)) AS F ON J.IndividualID = F.IndividualID

LEFT OUTER JOIN
    (SELECT IndividualID, EventLogEffectiveDate, ReasonCode, RECORDRANK FROM dbo.T_EventsSpecific AS GE WHERE(EventID = 'RST18') AND(RECORDRANK = 1)) AS G ON J.IndividualID = G.IndividualID

LEFT OUTER JOIN
    (SELECT IndividualID, EventLogEffectiveDate, ReasonCode, RECORDRANK FROM dbo.T_EventsSpecific AS HE WHERE(EventID = 'RST21') AND(RECORDRANK = 1)) AS H ON J.IndividualID = H.IndividualID

LEFT OUTER JOIN
    (SELECT IndividualID, EventLogResultEffectiveDate, ReasonCode, RECORDRANK FROM dbo.T_EventsSpecific AS KE WHERE(EventID = 'RST33') AND(RECORDRANK = 1)) AS K ON J.IndividualID = K.IndividualID

LEFT OUTER JOIN
    (SELECT IndividualID, EventLogEffectiveDate, ReasonCode, RECORDRANK FROM dbo.T_EventsSpecific AS LE WHERE(EventID = 'RST01') AND(RECORDRANK = 1)) AS L ON J.IndividualID = L.IndividualID

LEFT OUTER JOIN
    (SELECT IndividualID, EventLogResultEffectiveDate, ReasonCode, RECORDRANK FROM dbo.T_EventsSpecific AS ME WHERE(ResultID = 'RST04') AND(RECORDRANKRESULT = 1)) AS M ON J.IndividualID = M.IndividualID

LEFT OUTER JOIN
    (SELECT IndividualID, EventLogEffectiveDate, ReasonCode, RECORDRANK FROM dbo.T_EventsSpecific AS NE WHERE(EventID = 'REG38') AND(RECORDRANK = 1)) AS N ON J.IndividualID = N.IndividualID

LEFT OUTER JOIN
    (SELECT IndividualID, EventLogResultEffectiveDate, ReasonCode, RECORDRANK FROM dbo.T_EventsSpecific AS OE WHERE(EventID = 'VOL26') AND(RECORDRANK = 1)) AS O ON J.IndividualID = O.IndividualID

WHERE (C.EventLogResultEffectiveDate IS NOT NULL) OR
(D.EventLogEffectiveDate IS NOT NULL) OR
(E.EventLogEffectiveDate IS NOT NULL) OR
(F.EventLogResultEffectiveDate IS NOT NULL) OR
(G.EventLogEffectiveDate IS NOT NULL) OR
(H.EventLogEffectiveDate IS NOT NULL) OR
(K.EventLogResultEffectiveDate IS NOT NULL) OR
(L.EventLogEffectiveDate IS NOT NULL) OR
(M.EventLogResultEffectiveDate IS NOT NULL) OR
(N.EventLogEffectiveDate IS NOT NULL) OR
(O.EventLogResultEffectiveDate IS NOT NULL);


-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -

-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -
DROP TABLE   T_EventsSpecificIndSPneeds ;

SELECT A.[IndividualID],
              A.[CaseNo],
              A.[CurrentSize],
              A.[TotalSize],
              A.[Current_process_Status],
              A.[rst01_considered],
              A.[rst01_Reasonconsidered],
              A.[registered_rst_01],
              A.[rst04_notqualified],
              A.[rst04_Reasonnotqualified],
              A.[rst04_01],
              A.[rst06_Interview],
              A.[rst06_ReasonInterview],
              A.[rst09_Assessment_Complete],
              A.[rst09_ReasonAssessment_Complete],
              A.[rst09_01],
              A.[rst18_CaseSubmitted],
              A.[rst18_ReasonCaseSubmitted],
              A.[rst18_09],
              A.[rst21_CaseResbmitted],
              A.[rst21_ReasonCaseResbmitted],
              A.[rst21_18],
              A.[rst19_CaseAccepted],
              A.[rst19_ReasonCaseAccepted],
              A.[rst19_18],
              A.[rst43_Referred_to_Hub],
              A.[rst43_ReasonReferred_to_Hub],
              A.[rst33_DepartureConfirmed],
              A.[rst33_ReasonDepartureConfirmed],
              A.[rst33_19],
              A.[reg38_SpontaneousDeparture],
              A.[reg38_ReasonSpontaneousDeparture],
              A.[registered_Spontaneousdepart],
              A.[vol26_VolrepDepartureConfirmed],
              A.[vol26_ReasonVolrepDepartureConfirmed],
              A.[registered_VolDepart],
              A.[CaseCreatedOn],
              A.[CoO],
              A.[CoA],
              A.[NationalityCode],
              A.[Relationship],
              A.[Sex],
              A.[DOB],
              A.[MarriageStatusCode],
              A.[IndividualAge],
              A.[AgeGroup],
              A.[Religion],
              A.[Ethnicity],
              A.[OccupationCode],
              A.[Occupation],
              A.[EducationLevelCode],
              A.[HasSPNeed],
              A.[ArrivalDate],
              A.[RegisteredOn],
              A.[RefStatus],
              A.[RefStatCategory],
              A.[RefugeeStatusDate],
              C.[CR] as Child_at_risk,
              C.[CR-AF] as Child_at_risk_associated_with_armed_forces_or_groups,
              C.[CR-CC] as Child_at_risk_carer,
              C.[CR-CH] as Child_at_risk_headed_household,
              C.[CR-CL] as Child_at_risk_in_conflict_with_the_law,
              C.[CR-CP] as Child_at_risk_parent,
              C.[CR-CS] as Child_at_risk_spouse,
              C.[CR-LO] as Child_at_risk_engaged_in_other_forms_of_child_labour,
              C.[CR-LW] as Child_at_risk_engaged_in_worst_forms_of_child_labour,
              C.[CR-MS] as Child_at_riskMinor_spouse,
              C.[CR-NE] as Child_at_risk_of_not_attending_school,
              C.[CR-SE] as Child_at_risk_with_special_education_needs,
              C.[CR-TP] as Child_at_risk_Teenage_pregnancy,
              C.[DS] as Disability,
              C.[DS-BD] as Disability_Visual_impairment_including_blindness_,
              C.[DS-DF] as Disability_Hearing_Impairment_including_deafness_,
              C.[DS-MM] as Disability_Mental_disability_moderate,
              C.[DS-MS] as Disability_Mental_disability_severe,
              C.[DS-PM] as Disability_Physical_disability_moderate,
              C.[DS-PS] as Disability_Physical_disability_severe,
              C.[DS-SD] as Disability_Speech_impairment_disability,
              C.[ER] as Older_person_at_risk,
              C.[ER-FR] as Older_person_at_risk_unable_to_care_for_self,
              C.[ER-MC] as Older_person_at_risk_with_children,
              C.[ER-NF] as Older_person_at_risk_Unaccompanied_older_person,
              C.[ER-OC] as Older_person_at_risk_without_younger_family_members,
              C.[ER-SC] as Older_person_at_risk_with_separated_children,
              C.[ER-UR] as Older_person_at_risk_Single_without_accompy_family_members,
              C.[FU] as Family_unity,
              C.[FU-FR] as Family_unity_reunification_required,
              C.[FU-TR] as Family_unity_Tracing_required,
              C.[LP] as Specific_legal_and_physical_protection_needs,
              C.[LP-AF] as Specific_legal_and_physical_protection_needs_Formerly_associated_with_armed_forces_or_groups,
              C.[LP-AN] as Specific_legal_and_physical_protection_needs_Violence_abuse_or_neglect,
              C.[LP-AP] as Specific_legal_and_physical_protection_needs_Alleged_perpetrator,
              C.[LP-BN] as Specific_legal_and_physical_protection_needs_Unmet_basic_needs,
              C.[LP-CR] as Specific_legal_and_physical_protection_needs_Criminal_record,
              C.[LP-DA] as Specific_legal_and_physical_protection_needs_Detained_held_in_country_of_asylum,
              C.[LP-DN] as Specific_legal_and_physical_protection_needs_Currently_detained_held_in_country_of_asylum,
              C.[LP-DO] as Specific_legal_and_physical_protection_needs_Detained_held_in_country_of_origin,
              C.[LP-DP] as Specific_legal_and_physical_protection_needs_Formerly_detained_held_in_country_of_asylum,
              C.[LP-DT] as Specific_legal_and_physical_protection_needs_Detained_held_elsewhere,
              C.[LP-ES] as Specific_legal_and_physical_protection_needs_Individual_excluded_or_marginalised_from_society,
              C.[LP-FR] as Specific_legal_and_physical_protection_needs_Family_reunion_required,
              C.[LP-IH] as Specific_legal_and_physical_protection_needs_In_hiding,
              C.[LP-LS] as Specific_legal_and_physical_protection_needs_Lack_of_durable_solutions_prospects,
              C.[LP-MD] as Specific_legal_and_physical_protection_needs_Multiple_displacements,
              C.[LP-MM] as Specific_legal_and_physical_protection_needs_Mixed_marriage,
              C.[LP-MS] as Specific_legal_and_physical_protection_needs_Marginalized_from_society_or_community,
              C.[LP-NA] as Specific_legal_and_physical_protection_needs_No_access_to_services,
              C.[LP-ND] as Specific_legal_and_physical_protection_needs_No_legal_documentation,
              C.[LP-PV] as Specific_legal_and_physical_protection_needs_Durable_solutions_related_vulnerability,
              C.[LP-RD] as Specific_legal_and_physical_protection_needs_At_risk_of_removal,
              C.[LP-RP] as Specific_legal_and_physical_protection_needs_At_risk_due_to_profile,
              C.[LP-RR] as Specific_legal_and_physical_protection_needs_At_risk_of_refoulement,
              C.[LP-ST] as Specific_legal_and_physical_protection_needs_Security_threat_to_UNHCR_partner_staff_or_others,
              C.[LP-TA] as Specific_legal_and_physical_protection_needs_Survivor_of_torture_violence_in_asylum,
              C.[LP-TC] as Specific_legal_and_physical_protection_needs_Tracing_required,
              C.[LP-TD] as Specific_legal_and_physical_protection_needs_At_risk_of_deportation,
              C.[LP-TO] as Specific_legal_and_physical_protection_needs_Survivor_of_torture_violence_in_home_country,
              C.[LP-TR] as Specific_legal_and_physical_protection_needs_At_risk_of_refoulement2,
              C.[LP-UP] as Specific_legal_and_physical_protection_needs_Urgent_need_of_physical_protection,
              C.[LP-VA] as Specific_legal_and_physical_protection_needs_Victim_of_domestic_violence_SGBV_in_asylum,
              C.[LP-VF] as Specific_legal_and_physical_protection_needs_Victim_of_domestic_violence_SGBV_during_flight,
              C.[LP-VO] as Specific_legal_and_physical_protection_needs_Victim_of_domestic_violence_SGBV_in_home_country,
              C.[LP-VP] as Specific_legal_and_physical_protection_needs_Alleged_perpetrator_of_violence,
              C.[LP-WP] as Specific_legal_and_physical_protection_needs_Absence_of_witness_protection,
              C.[PG] as Pregnant_or_lactating,
              C.[PG-HR] as Pregnant_or_lactating_High_risk_pregnancy,
              C.[PG-LC] as Pregnant_or_lactating_Lactating,
              C.[SC] as Unaccompanied_or_separated_child,
              C.[SC-CH] as Unaccompanied_or_separated_Single_Child_headed_household,
              C.[SC-FC] as Unaccompanied_or_separated_Child_in_foster_care,
              C.[SC-IC] as Unaccompanied_or_separated_Child_in_institutional_care,
              C.[SC-NC] as Unaccompanied_or_separated_Neglected_child_with_extended_family,
              C.[SC-SC] as Unaccompanied_or_separated_child_Separated_child,
              C.[SC-UC] as Unaccompanied_or_separated_child_Unaccompanied_child,
              C.[SC-UF] as Unaccompanied_or_separated_child_Child_in_foster_care2,
              C.[SC-UM] as Unaccompanied_or_separated_child_Unaccompanied_minor,
              C.[SM] as Serious_medical_condition,
              C.[SM-AD] as Serious_medical_condition_Addiction,
              C.[SM-CC] as Serious_medical_condition_Critical_medical,
              C.[SM-CI] as Serious_medical_condition_Chronic_illness,
              C.[SM-DP] as Serious_medical_condition_Difficult_pregnancy,
              C.[SM-MI] as Serious_medical_condition_Mental_illness,
              C.[SM-MN] as Serious_medical_condition_Malnutrition,
              C.[SM-OT] as Serious_medical_condition_Other_medical_condition,
              C.[SP] as Single_parent,
              C.[SP-CG] as Single_parent_Single_HR_caregiver,
              C.[SP-GP] as Single_parent_Single_HR_grandparent,
              C.[SP-PT] as Single_parent_Single_HR_parent,
              C.[SV] as SGBV,
              C.[SV-FM] as SGBV_Threat_of_forced_marriage,
              C.[SV-GM] as SGBV_Female_genital_mutilation,
              C.[SV-HK] as SGBV_Threat_of_honour_killing_violence,
              C.[SV-HP] as SGBV_Harmful_traditional_practices,
              C.[SV-SS] as SGBV_Survival_sex,
              C.[SV-VA] as SGBV_Exposure_to_SGBV,
              C.[SV-VF] as SGBV_Exposure_to_SGBV_during_flight,
              C.[SV-VO] as SGBV_Exposure_to_SGBV_in_country_of_origin,
              C.[TR] as Torture,
              C.[TR-HO] as Torture_Forced_to_egregious_acts,
              C.[TR-PI] as Torture_Psych_and_or_physical_impairment_due_to_torture,
              C.[TR-WV] as Torture_Witness_of_violence_to_other,
              C.[WR] as Woman_at_risk,
              C.[WR-GM] as Woman_at_risk_Threat_of_female_genital_mutilation,
              C.[WR-HR] as Woman_at_risk_Single_female_household_representative,
              C.[WR-LC] as Woman_at_risk_Lactating_at_risk,
              C.[WR-PY] as Woman_at_risk_In_polygamous_marriage_or_relationship,
              C.[WR-SF] as Woman_at_risk_Single_woman,
              C.[WR-UW] as Woman_at_risk_Woman_unaccompanied_by_adult_male_family_member,
              C.[WR-WF] as Woman_at_risk_Woman_associated_with_fighting_forces,
              C.[WR-WR] as Woman_at_risk_Woman_at_risk_unspecified

    INTO   T_EventsSpecificIndSPneeds

    FROM [DWH].[dbo].[T_EventsSpecificInd] A

     LEFT OUTER JOIN  [DWH].[dbo].[T_SPneedsBreak] C
       ON A.[IndividualID]= C.[IndividualID]



-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -
--Now aggregating at the case level 
-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -


-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -
--Aggregation at the case level

DROP TABLE T_SPneedsCaseLevel;

select C.[CaseNo],
SUM(C.[CR]) as Child_at_risk,
SUM(C.[CR - AF]) as Child_at_risk_associated_with_armed_forces_or_groups,
SUM(C.[CR - CC]) as Child_at_risk_carer,
SUM(C.[CR - CH]) as Child_at_risk_headed_household,
SUM(C.[CR - CL]) as Child_at_risk_in_conflict_with_the_law,
SUM(C.[CR - CP]) as Child_at_risk_parent,
SUM(C.[CR - CS]) as Child_at_risk_spouse,
SUM(C.[CR - LO]) as Child_at_risk_engaged_in_other_forms_of_child_labour,
SUM(C.[CR - LW]) as Child_at_risk_engaged_in_worst_forms_of_child_labour,
SUM(C.[CR - MS]) as Child_at_riskMinor_spouse,
SUM(C.[CR - NE]) as Child_at_risk_of_not_attending_school,
SUM(C.[CR - SE]) as Child_at_risk_with_special_education_needs,
SUM(C.[CR - TP]) as Child_at_risk_Teenage_pregnancy,
SUM(C.[DS]) as Disability,
SUM(C.[DS - BD]) as Disability_Visual_impairment_including_blindness_,
SUM(C.[DS - DF]) as Disability_Hearing_Impairment_including_deafness_,
SUM(C.[DS - MM]) as Disability_Mental_disability_moderate,
SUM(C.[DS - MS]) as Disability_Mental_disability_severe,
SUM(C.[DS - PM]) as Disability_Physical_disability_moderate,
SUM(C.[DS - PS]) as Disability_Physical_disability_severe,
SUM(C.[DS - SD]) as Disability_Speech_impairment_disability,
SUM(C.[ER]) as Older_person_at_risk,
SUM(C.[ER - FR]) as Older_person_at_risk_unable_to_care_for_self,
SUM(C.[ER - MC]) as Older_person_at_risk_with_children,
SUM(C.[ER - NF]) as Older_person_at_risk_Unaccompanied_older_person,
SUM(C.[ER - OC]) as Older_person_at_risk_without_younger_family_members,
SUM(C.[ER - SC]) as Older_person_at_risk_with_separated_children,
SUM(C.[ER - UR]) as Older_person_at_risk_Single_without_accompy_family_members,
SUM(C.[FU]) as Family_unity,
SUM(C.[FU - FR]) as Family_unity_reunification_required,
SUM(C.[FU - TR]) as Family_unity_Tracing_required,
SUM(C.[LP]) as Specific_legal_and_physical_protection_needs,
SUM(C.[LP - AF]) as Specific_legal_and_physical_protection_needs_Formerly_associated_with_armed_forces_or_groups,
SUM(C.[LP - AN]) as Specific_legal_and_physical_protection_needs_Violence_abuse_or_neglect,
SUM(C.[LP - AP]) as Specific_legal_and_physical_protection_needs_Alleged_perpetrator,
SUM(C.[LP - BN]) as Specific_legal_and_physical_protection_needs_Unmet_basic_needs,
SUM(C.[LP - CR]) as Specific_legal_and_physical_protection_needs_Criminal_record,
SUM(C.[LP - DA]) as Specific_legal_and_physical_protection_needs_Detained_held_in_country_of_asylum,
SUM(C.[LP - DN]) as Specific_legal_and_physical_protection_needs_Currently_detained_held_in_country_of_asylum,
SUM(C.[LP - DO]) as Specific_legal_and_physical_protection_needs_Detained_held_in_country_of_origin,
SUM(C.[LP - DP]) as Specific_legal_and_physical_protection_needs_Formerly_detained_held_in_country_of_asylum,
SUM(C.[LP - DT]) as Specific_legal_and_physical_protection_needs_Detained_held_elsewhere,
SUM(C.[LP - ES]) as Specific_legal_and_physical_protection_needs_Individual_excluded_or_marginalised_from_society,
SUM(C.[LP - FR]) as Specific_legal_and_physical_protection_needs_Family_reunion_required,
SUM(C.[LP - IH]) as Specific_legal_and_physical_protection_needs_In_hiding,
SUM(C.[LP - LS]) as Specific_legal_and_physical_protection_needs_Lack_of_durable_solutions_prospects,
SUM(C.[LP - MD]) as Specific_legal_and_physical_protection_needs_Multiple_displacements,
SUM(C.[LP - MM]) as Specific_legal_and_physical_protection_needs_Mixed_marriage,
SUM(C.[LP - MS]) as Specific_legal_and_physical_protection_needs_Marginalized_from_society_or_community,
SUM(C.[LP - NA]) as Specific_legal_and_physical_protection_needs_No_access_to_services,
SUM(C.[LP - ND]) as Specific_legal_and_physical_protection_needs_No_legal_documentation,
SUM(C.[LP - PV]) as Specific_legal_and_physical_protection_needs_Durable_solutions_related_vulnerability,
SUM(C.[LP - RD]) as Specific_legal_and_physical_protection_needs_At_risk_of_removal,
SUM(C.[LP - RP]) as Specific_legal_and_physical_protection_needs_At_risk_due_to_profile,
SUM(C.[LP - RR]) as Specific_legal_and_physical_protection_needs_At_risk_of_refoulement,
SUM(C.[LP - ST]) as Specific_legal_and_physical_protection_needs_Security_threat_to_UNHCR_partner_staff_or_others,
SUM(C.[LP - TA]) as Specific_legal_and_physical_protection_needs_Survivor_of_torture_violence_in_asylum,
SUM(C.[LP - TC]) as Specific_legal_and_physical_protection_needs_Tracing_required,
SUM(C.[LP - TD]) as Specific_legal_and_physical_protection_needs_At_risk_of_deportation,
SUM(C.[LP - TO]) as Specific_legal_and_physical_protection_needs_Survivor_of_torture_violence_in_home_country,
SUM(C.[LP - TR]) as Specific_legal_and_physical_protection_needs_At_risk_of_refoulement2,
SUM(C.[LP - UP]) as Specific_legal_and_physical_protection_needs_Urgent_need_of_physical_protection,
SUM(C.[LP - VA]) as Specific_legal_and_physical_protection_needs_Victim_of_domestic_violence_SGBV_in_asylum,
SUM(C.[LP - VF]) as Specific_legal_and_physical_protection_needs_Victim_of_domestic_violence_SGBV_during_flight,
SUM(C.[LP - VO]) as Specific_legal_and_physical_protection_needs_Victim_of_domestic_violence_SGBV_in_home_country,
SUM(C.[LP - VP]) as Specific_legal_and_physical_protection_needs_Alleged_perpetrator_of_violence,
SUM(C.[LP - WP]) as Specific_legal_and_physical_protection_needs_Absence_of_witness_protection,
SUM(C.[PG]) as Pregnant_or_lactating,
SUM(C.[PG - HR]) as Pregnant_or_lactating_High_risk_pregnancy,
SUM(C.[PG - LC]) as Pregnant_or_lactating_Lactating,
SUM(C.[SC]) as Unaccompanied_or_separated_child,
SUM(C.[SC - CH]) as Unaccompanied_or_separated_Single_Child_headed_household,
SUM(C.[SC - FC]) as Unaccompanied_or_separated_Child_in_foster_care,
SUM(C.[SC - IC]) as Unaccompanied_or_separated_Child_in_institutional_care,
SUM(C.[SC - NC]) as Unaccompanied_or_separated_Neglected_child_with_extended_family,
SUM(C.[SC - SC]) as Unaccompanied_or_separated_child_Separated_child,
SUM(C.[SC - UC]) as Unaccompanied_or_separated_child_Unaccompanied_child,
SUM(C.[SC - UF]) as Unaccompanied_or_separated_child_Child_in_foster_care2,
SUM(C.[SC - UM]) as Unaccompanied_or_separated_child_Unaccompanied_minor,
SUM(C.[SM]) as Serious_medical_condition,
SUM(C.[SM - AD]) as Serious_medical_condition_Addiction,
SUM(C.[SM - CC]) as Serious_medical_condition_Critical_medical,
SUM(C.[SM - CI]) as Serious_medical_condition_Chronic_illness,
SUM(C.[SM - DP]) as Serious_medical_condition_Difficult_pregnancy,
SUM(C.[SM - MI]) as Serious_medical_condition_Mental_illness,
SUM(C.[SM - MN]) as Serious_medical_condition_Malnutrition,
SUM(C.[SM - OT]) as Serious_medical_condition_Other_medical_condition,
SUM(C.[SP]) as Single_parent,
SUM(C.[SP - CG]) as Single_parent_Single_HR_caregiver,
SUM(C.[SP - GP]) as Single_parent_Single_HR_grandparent,
SUM(C.[SP - PT]) as Single_parent_Single_HR_parent,
SUM(C.[SV]) as SGBV,
SUM(C.[SV - FM]) as SGBV_Threat_of_forced_marriage,
SUM(C.[SV - GM]) as SGBV_Female_genital_mutilation,
SUM(C.[SV - HK]) as SGBV_Threat_of_honour_killing_violence,
SUM(C.[SV - HP]) as SGBV_Harmful_traditional_practices,
SUM(C.[SV - SS]) as SGBV_Survival_sex,
SUM(C.[SV - VA]) as SGBV_Exposure_to_SGBV,
SUM(C.[SV - VF]) as SGBV_Exposure_to_SGBV_during_flight,
SUM(C.[SV - VO]) as SGBV_Exposure_to_SGBV_in_country_of_origin,
SUM(C.[TR]) as Torture,
SUM(C.[TR - HO]) as Torture_Forced_to_egregious_acts,
SUM(C.[TR - PI]) as Torture_Psych_and_or_physical_impairment_due_to_torture,
SUM(C.[TR - WV]) as Torture_Witness_of_violence_to_other,
SUM(C.[WR]) as Woman_at_risk,
SUM(C.[WR - GM]) as Woman_at_risk_Threat_of_female_genital_mutilation,
SUM(C.[WR - HR]) as Woman_at_risk_Single_female_household_representative,
SUM(C.[WR - LC]) as Woman_at_risk_Lactating_at_risk,
SUM(C.[WR - PY]) as Woman_at_risk_In_polygamous_marriage_or_relationship,
SUM(C.[WR - SF]) as Woman_at_risk_Single_woman,
SUM(C.[WR - UW]) as Woman_at_risk_Woman_unaccompanied_by_adult_male_family_member,
SUM(C.[WR - WF]) as Woman_at_risk_Woman_associated_with_fighting_forces,
SUM(C.[WR - WR]) as Woman_at_risk_Woman_at_risk_unspecified

INTO T_SPneedsCaseLevel

FROM [DWH].[dbo].[T_SPneedsBreak] C

--ORDER BY IndividualID--order
GROUP BY[CaseNo]


-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -
--DROP TABLE T_EventsSpecificPA;
-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -

DROP TABLE T_EventsSpecificPA;

SELECT[CaseNo], [CurrentSize], [TotalSize], [Current_process_Status], [rst01_considered], [rst01_Reasonconsidered], [registered_rst_01], [rst04_notqualified], [rst04_Reasonnotqualified], [rst04_01], [rst06_Interview], [rst06_ReasonInterview], [rst09_Assessment_Complete], [rst09_ReasonAssessment_Complete], [rst09_01], [rst18_CaseSubmitted], [rst18_ReasonCaseSubmitted], [rst18_09], [rst21_CaseResbmitted], [rst21_ReasonCaseResbmitted], [rst19_CaseAccepted], [rst19_ReasonCaseAccepted], [rst43_Referred_to_Hub], [rst43_ReasonReferred_to_Hub], [rst33_DepartureConfirmed], [rst33_ReasonDepartureConfirmed], [reg38_SpontaneousDeparture], [reg38_ReasonSpontaneousDeparture], [registered_Spontaneousdepart], [vol26_VolrepDepartureConfirmed], [vol26_ReasonVolrepDepartureConfirmed], [registered_VolDepart], [CaseCreatedOn], [CoO], [CoA], [NationalityCode], [ArrivalDate], [RegisteredOn], [RefStatus], [RefStatCategory], [RefugeeStatusDate]

INTO T_EventsSpecificPA
FROM[DWH].[dbo].[T_EventsSpecificInd]
WHERE Relationship = 'PA'
ORDER BY[CaseNo]



-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -
-- -T_EventsSpecificIndSPneedsCase
-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -
DROP TABLE  T_EventsSpecificIndSPneedsCase;
 
SELECT
A.[CaseNo],
A.[CurrentSize],
A.[TotalSize],
A.[Current_process_Status],
A.[rst01_considered],
A.[rst01_Reasonconsidered],
A.[rst04_notqualified],
A.[rst04_Reasonnotqualified],
A.[rst06_Interview],
A.[rst06_ReasonInterview],
A.[rst09_Assessment_Complete],
A.[rst09_ReasonAssessment_Complete],
A.[rst18_CaseSubmitted],
A.[rst18_ReasonCaseSubmitted],
A.[rst21_CaseResbmitted],
A.[rst21_ReasonCaseResbmitted],
A.[rst19_CaseAccepted],
A.[rst19_ReasonCaseAccepted],
A.[rst43_Referred_to_Hub],
A.[rst43_ReasonReferred_to_Hub],
A.[rst33_DepartureConfirmed],
A.[rst33_ReasonDepartureConfirmed],
A.[reg38_VoluntaryDeparture],
A.[reg38_ReasonVoluntaryDeparture],
A.[vol26_VolrepDepartureConfirmed],
A.[vol26_ReasonVolrepDepartureConfirmed],
A.[CaseCreatedOn],
A.[CoO],
A.[CoA],
A.[NationalityCode],
A.[ArrivalDate],
A.[RegisteredOn],
A.[RefStatus],
A.[RefStatCategory],
A.[RefugeeStatusDate],
C.[Child_at_risk],
C.[Child_at_risk_associated_with_armed_forces_or_groups],
C.[Child_at_risk_carer],
C.[Child_at_risk_headed_household],
C.[Child_at_risk_in_conflict_with_the_law],
C.[Child_at_risk_parent],
C.[Child_at_risk_spouse],
C.[Child_at_risk_engaged_in_other_forms_of_child_labour],
C.[Child_at_risk_engaged_in_worst_forms_of_child_labour],
C.[Child_at_riskMinor_spouse],
C.[Child_at_risk_of_not_attending_school],
C.[Child_at_risk_with_special_education_needs],
C.[Child_at_risk_Teenage_pregnancy],
C.[Disability],
C.[Disability_Visual_impairment_including_blindness_],
C.[Disability_Hearing_Impairment_including_deafness_],
C.[Disability_Mental_disability_moderate],
C.[Disability_Mental_disability_severe],
C.[Disability_Physical_disability_moderate],
C.[Disability_Physical_disability_severe],
C.[Disability_Speech_impairment_disability],
C.[Older_person_at_risk],
C.[Older_person_at_risk_unable_to_care_for_self],
C.[Older_person_at_risk_with_children],
C.[Older_person_at_risk_Unaccompanied_older_person],
C.[Older_person_at_risk_without_younger_family_members],
C.[Older_person_at_risk_with_separated_children],
C.[Older_person_at_risk_Single_without_accompy_family_members],
C.[Family_unity],
C.[Family_unity_reunification_required],
C.[Family_unity_Tracing_required],
C.[Specific_legal_and_physical_protection_needs],
C.[Specific_legal_and_physical_protection_needs_Formerly_associated_with_armed_forces_or_groups],
C.[Specific_legal_and_physical_protection_needs_Violence_abuse_or_neglect],
C.[Specific_legal_and_physical_protection_needs_Alleged_perpetrator],
C.[Specific_legal_and_physical_protection_needs_Unmet_basic_needs],
C.[Specific_legal_and_physical_protection_needs_Criminal_record],
C.[Specific_legal_and_physical_protection_needs_Detained_held_in_country_of_asylum],
C.[Specific_legal_and_physical_protection_needs_Currently_detained_held_in_country_of_asylum],
C.[Specific_legal_and_physical_protection_needs_Detained_held_in_country_of_origin],
C.[Specific_legal_and_physical_protection_needs_Formerly_detained_held_in_country_of_asylum],
C.[Specific_legal_and_physical_protection_needs_Detained_held_elsewhere],
C.[Specific_legal_and_physical_protection_needs_Individual_excluded_or_marginalised_from_society],
C.[Specific_legal_and_physical_protection_needs_Family_reunion_required],
C.[Specific_legal_and_physical_protection_needs_In_hiding],
C.[Specific_legal_and_physical_protection_needs_Lack_of_durable_solutions_prospects],
C.[Specific_legal_and_physical_protection_needs_Multiple_displacements],
C.[Specific_legal_and_physical_protection_needs_Mixed_marriage],
C.[Specific_legal_and_physical_protection_needs_Marginalized_from_society_or_community],
C.[Specific_legal_and_physical_protection_needs_No_access_to_services],
C.[Specific_legal_and_physical_protection_needs_No_legal_documentation],
C.[Specific_legal_and_physical_protection_needs_Durable_solutions_related_vulnerability],
C.[Specific_legal_and_physical_protection_needs_At_risk_of_removal],
C.[Specific_legal_and_physical_protection_needs_At_risk_due_to_profile],
C.[Specific_legal_and_physical_protection_needs_At_risk_of_refoulement],
C.[Specific_legal_and_physical_protection_needs_Security_threat_to_UNHCR_partner_staff_or_others],
C.[Specific_legal_and_physical_protection_needs_Survivor_of_torture_violence_in_asylum],
C.[Specific_legal_and_physical_protection_needs_Tracing_required],
C.[Specific_legal_and_physical_protection_needs_At_risk_of_deportation],
C.[Specific_legal_and_physical_protection_needs_Survivor_of_torture_violence_in_home_country],
C.[Specific_legal_and_physical_protection_needs_At_risk_of_refoulement2],
C.[Specific_legal_and_physical_protection_needs_Urgent_need_of_physical_protection],
C.[Specific_legal_and_physical_protection_needs_Victim_of_domestic_violence_SGBV_in_asylum],
C.[Specific_legal_and_physical_protection_needs_Victim_of_domestic_violence_SGBV_during_flight],
C.[Specific_legal_and_physical_protection_needs_Victim_of_domestic_violence_SGBV_in_home_country],
C.[Specific_legal_and_physical_protection_needs_Alleged_perpetrator_of_violence],
C.[Specific_legal_and_physical_protection_needs_Absence_of_witness_protection],
C.[Pregnant_or_lactating],
C.[Pregnant_or_lactating_High_risk_pregnancy],
C.[Pregnant_or_lactating_Lactating],
C.[Unaccompanied_or_separated_child],
C.[Unaccompanied_or_separated_Single_Child_headed_household],
C.[Unaccompanied_or_separated_Child_in_foster_care],
C.[Unaccompanied_or_separated_Child_in_institutional_care],
C.[Unaccompanied_or_separated_Neglected_child_with_extended_family],
C.[Unaccompanied_or_separated_child_Separated_child],
C.[Unaccompanied_or_separated_child_Unaccompanied_child],
C.[Unaccompanied_or_separated_child_Child_in_foster_care2],
C.[Unaccompanied_or_separated_child_Unaccompanied_minor],
C.[Serious_medical_condition],
C.[Serious_medical_condition_Addiction],
C.[Serious_medical_condition_Critical_medical],
C.[Serious_medical_condition_Chronic_illness],
C.[Serious_medical_condition_Difficult_pregnancy],
C.[Serious_medical_condition_Mental_illness],
C.[Serious_medical_condition_Malnutrition],
C.[Serious_medical_condition_Other_medical_condition],
C.[Single_parent],
C.[Single_parent_Single_HR_caregiver],
C.[Single_parent_Single_HR_grandparent],
C.[Single_parent_Single_HR_parent],
C.[SGBV],
C.[SGBV_Threat_of_forced_marriage],
C.[SGBV_Female_genital_mutilation],
C.[SGBV_Threat_of_honour_killing_violence],
C.[SGBV_Harmful_traditional_practices],
C.[SGBV_Survival_sex],
C.[SGBV_Exposure_to_SGBV],
C.[SGBV_Exposure_to_SGBV_during_flight],
C.[SGBV_Exposure_to_SGBV_in_country_of_origin],
C.[Torture],
C.[Torture_Forced_to_egregious_acts],
C.[Torture_Psych_and_or_physical_impairment_due_to_torture],
C.[Torture_Witness_of_violence_to_other],
C.[Woman_at_risk],
C.[Woman_at_risk_Threat_of_female_genital_mutilation],
C.[Woman_at_risk_Single_female_household_representative],
C.[Woman_at_risk_Lactating_at_risk],
C.[Woman_at_risk_In_polygamous_marriage_or_relationship],
C.[Woman_at_risk_Single_woman],
C.[Woman_at_risk_Woman_unaccompanied_by_adult_male_family_member],
C.[Woman_at_risk_Woman_associated_with_fighting_forces],
C.[Woman_at_risk_Woman_at_risk_unspecified]

INTO T_EventsSpecificIndSPneedsCase
FROM[DWH].[dbo].[T_EventsSpecificPA] A

LEFT OUTER JOIN[DWH].[dbo].[T_SPneedsCaseLevel] C
ON A.[CaseNo] = C.[CaseNo]
