
-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -
-- DATA MINING TABLES CREATION:

---- Table for Case profile -- pivot of specific needs and list of relevant events
-- caseprofile
-- T_SPneedsBreak

-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -
/*****************************************************************************************************
 Author:  Edouard LEGOUPIL
  Email:  legoupil@unhcr.org
   Date:  Feb 2018
Purpose:  Creating Summary Tables for data mining with R
DB Vers:  proGres Datawarehouse MENA
Version:  1.0 - View Creation

*****************************************************************************************************/


DROP TABLE caseprofile1;
SELECT  
       -- CONVERT(uniqueidentifier, PP.ProcessingGroupNumber) CaseNo, 
        PP.ProcessingGroupNumber CaseNo,
        COUNT(DISTINCT II.IndividualGUID) Num_Inds,
        AVG(II.IndividualAge) AVG_Age,
        STDEV(II.IndividualAge) STDEV_Age,
        Count( CASE WHEN(II.IndividualAge < 15) THEN(II.IndividualGUID) ELSE(NULL) END) Child_0_14,
        Count( CASE WHEN(II.IndividualAge < 19 AND IndividualAge > 14) THEN(II.IndividualGUID) ELSE(NULL) END) Youth_15_17,
        Count( CASE WHEN(II.IndividualAge < 65 AND IndividualAge > 14) THEN(II.IndividualGUID) ELSE(NULL) END) Work_15_64,
        Count( CASE WHEN(II.IndividualAge > 64) THEN(II.IndividualGUID) ELSE(NULL) END) Eldern_65,
		Count( CASE WHEN(II.SexCode = 'M') THEN(SexCode) ELSE(NULL) END) Male,
        Count( CASE WHEN(II.SexCode = 'F') THEN(SexCode) ELSE(NULL) END) Female,
	    Count( CASE WHEN(II.SexCode not in  ('F','M')) THEN('Empty')  END) NOGender,
	    Count( CASE WHEN(IPGG.RelationshipToPrincipalRepresentative ='HUS' or IPGG.RelationshipToPrincipalRepresentative ='EXM' or IPGG.RelationshipToPrincipalRepresentative ='WIF' 
		                 or IPGG.RelationshipToPrincipalRepresentative ='EXF' or IPGG.RelationshipToPrincipalRepresentative ='CLH' or IPGG.RelationshipToPrincipalRepresentative ='CLW') THEN(II.IndividualGUID) ELSE(NULL) END) couple,
		Count( CASE WHEN(IPGG.RelationshipToPrincipalRepresentative ='SCF' or IPGG.RelationshipToPrincipalRepresentative ='SCM' or IPGG.RelationshipToPrincipalRepresentative ='FCF'
                		or IPGG.RelationshipToPrincipalRepresentative ='FCM' or IPGG.RelationshipToPrincipalRepresentative ='SON' or IPGG.RelationshipToPrincipalRepresentative ='DAU' and II.IndividualAge < 19) THEN(II.IndividualGUID) ELSE(NULL) END) minordependant
    
    
	INTO [proGres].[dbo].[caseprofile1]
	
	FROM dbo.dataProcessGroup AS PP
     
	 INNER JOIN dbo.dataIndividualProcessGroup AS IPGG ON PP.ProcessingGroupGUID = IPGG.ProcessingGroupGUID
     INNER JOIN dbo.dataIndividual AS II ON IPGG.IndividualGUID = II.IndividualGUID
	
	WHERE ProcessStatusCode IN('A') GROUP BY ProcessingGroupNumber

--------------------------------------------------------------
---------------------------------------------------------------
DROP TABLE caseprofile2;

SELECT P.ProcessingGroupNumber CaseNo,
       P.ProcessingGroupSize Num_Inds1,
       IPG.RelationshipToPrincipalRepresentative Relationship,	   
       IPG.PrincipalRepresentative Relationshippa,
       I.OriginCountryCode CountryOrigin,
       I.NationalityCode dem_birth_country,
       DATENAME(mm, I.ArrivalDate) Montharrival,
       DATENAME(yyyy, I.ArrivalDate) YearArrival,
       I.SexCode dem_sex,
       I.IndividualAge dem_age,
       I.IndividualAgeCohortCode dem_agegroup,
       I.EthnicityCode dem_ethn,
       I.ReligionCode dem_religion,
       I.MarriageStatusCode dem_marriage,
       I.EducationLevelCode edu_highest,
       I.RefugeeStatusCode,
       I.RefugeeStatusDate,
       I.RefugeeStatusCategoryCode,
      -- I.RefugeeStatusCategoryDate,
       I.RefugeeStatusLegalBasisCode,
     --  I.RefugeeStatusLegalBasisDate,
       I.ProcessStatusCode,
     --  I.ProcessStatusDate,
       I.ProcessStatusReasonCode,
     --  I.ProcessStatusReasonDate,
       I.SPNeeds,
       I.HasSPNeed,
       I.OccupationCode occupationcode,   
	   
       K.LocationLevel1Description cool1,
       K.LocationLevel1ID cool1id,
       K.LocationLevel2Description cool2,
       K.LocationLevel2ID cool2id,
       K.LocationLevel3Description cool3,
       K.LocationLevel3ID cool3id,
       K.LocationLevel4Description cool4,
       K.LocationLevel4ID cool4id,
       K.LocationLevel5Description cool5,
       K.LocationLevel5ID cool5id,
	   
       J.LocationLevel1Description coal1,
       J.LocationLevel1ID coal1id,
       J.LocationLevel2Description coal2,
       J.LocationLevel2ID coal2id,
       J.LocationLevel3Description coal3,
       J.LocationLevel3ID coal3id,
       J.LocationLevel4Description coal4,
       J.LocationLevel4ID coal4id,
       J.LocationLevel5Description coal5,
       J.LocationLevel5ID coal5id
	   
    
	INTO [proGres].[dbo].[caseprofile2]

FROM dbo.dataProcessGroup AS P
INNER JOIN dbo.dataIndividualProcessGroup AS IPG ON P.ProcessingGroupGUID = IPG.ProcessingGroupGUID
INNER JOIN dbo.dataIndividual AS I ON IPG.IndividualGUID = I.IndividualGUID
INNER JOIN dbo.vdataAddressCOA AS J ON IPG.IndividualGUID = J.IndividualGUID
INNER JOIN dbo.vdataAddressCOO AS K ON IPG.IndividualGUID = K.IndividualGUID
LEFT OUTER JOIN dbo.dataProcessGroupPhyFile AS PGF ON PGF.ProcessingGroupGUID = P.ProcessingGroupGUID
	
WHERE I.ProcessStatusCode = 'A' AND IPG.PrincipalRepresentative = 1


--------------------------------------------------------------------------
DROP TABLE caseprofile;
SELECT P.CaseNo,
       P.Num_Inds1,
       P.Relationship,	   
       P.Relationshippa,
       P.CountryOrigin,
       P.dem_birth_country,
       P.Montharrival,
       P.YearArrival,
       P.dem_sex,
       P.dem_age,
       P.dem_agegroup,
       P.dem_ethn,
       P.dem_religion,
       P.dem_marriage,
       P.edu_highest,
       P.RefugeeStatusCode,
       P.RefugeeStatusCategoryCode,
       P.RefugeeStatusLegalBasisCode,
       P.ProcessStatusCode,
       P.ProcessStatusReasonCode,
       P.SPNeeds,
       P.HasSPNeed,
       P.occupationcode,   
	   
       P.cool1,
       P.cool1id,
       P.cool2,
       P.cool2id,
       P.cool3,
       P.cool3id,
       P.cool4,
       P.cool4id,
       P.cool5,
       P.cool5id,
	   
       P.coal1,
       P.coal1id,
       P.coal2,
       P.coal2id,
       P.coal3,
       P.coal3id,
       P.coal4,
       P.coal4id,
       P.coal5,
       P.coal5id,
        Cal_1.Num_Inds,
		Cal_1.Child_0_14,
		Cal_1.Youth_15_17,
		Cal_1.Work_15_64,
		Cal_1.Eldern_65,
		Cal_1.Male,
		Cal_1.Female,
		Cal_1.NOGender,
		Cal_1.couple,
		Cal_1.minordependant, 
		Cal_1.AVG_Age,
		Cal_1.STDEV_Age
	INTO [proGres].[dbo].[caseprofile]
FROM caseprofile2 as P
LEFT JOIN caseprofile1 AS Cal_1  ON P.CaseNo = Cal_1.CaseNo




-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -
-- --Using pivot functions
DROP TABLE T_SPneedsBreak;
-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -
SELECT *

    INTO T_SPneedsBreak

FROM
    (SELECT I.IndividualGUID,  
	I.VulnerabilityDetailsCode as SPNeeds, 
	I.VulnerabilityDetailsCode as code, 
	P.ProcessingGroupNumber CaseNo --Colums to pivot
    FROM  dataVulnerability as I 
   INNER JOIN dbo.dataIndividual AS II ON I.IndividualGUID = II.IndividualGUID
    INNER JOIN dbo.dataIndividualProcessGroup AS IPG ON IPG.IndividualGUID = II.IndividualGUID
    INNER JOIN dbo.dataProcessGroup AS P  ON P.ProcessingGroupGUID = IPG.ProcessingGroupGUID
	WHERE I.VulnerabilityActive = 1    
		AND VulnerabilityDetailsCode in (
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
            'WR-LC', 'WR-PY', 'WR-SF', 'WR-UW', 'WR-WF', 'WR-WR'))
as sourcetable

pivot(
    COUNT(IndividualGUID) --Pivot on this column

    for SPNeeds --Make colum where SPNeeds is in one of these.

    in([CR], [CR - AF], [CR - CC], [CR - CH], [CR - CL], [CR - CP], [CR - CS], [CR - LO], [CR - LW], [CR - MS], [CR - NE], [CR - SE], [CR - TP], [DS], [DS - BD], [DS - DF], [DS - MM], [DS - MS], [DS - PM], [DS - PS], [DS - SD], [ER], [ER - FR], [ER - MC], [ER - NF], [ER - OC], [ER - SC], [ER - UR], [FU], [FU - FR], [FU - TR], [LP], [LP - AF], [LP - AN], [LP - AP], [LP - BN], [LP - CR], [LP - DA], [LP - DN], [LP - DO], [LP - DP], [LP - DT], [LP - ES], [LP - FR], [LP - IH], [LP - LS], [LP - MD], [LP - MM], [LP - MS], [LP - NA], [LP - ND], [LP - PV], [LP - RD], [LP - RP], [LP - RR], [LP - ST], [LP - TA], [LP - TC], [LP - TD], [LP - TO], [LP - TR], [LP - UP], [LP - VA], [LP - VF], [LP - VO], [LP - VP], [LP - WP], [PG], [PG - HR], [PG - LC], [SC], [SC - CH], [SC - FC], [SC - IC], [SC - NC], [SC - SC], [SC - UC], [SC - UF], [SC - UM], [SM], [SM - AD], [SM - CC], [SM - CI], [SM - DP], [SM - MI], [SM - MN], [SM - OT], [SP], [SP - CG], [SP - GP], [SP - PT], [SV], [SV - FM], [SV - GM], [SV - HK], [SV - HP], [SV - SS], [SV - VA], [SV - VF], [SV - VO], [TR], [TR - HO], [TR - PI], [TR - WV], [WR], [WR - GM], [WR - HR], [WR - LC], [WR - PY], [WR - SF], [WR - UW], [WR - WF], [WR - WR])
)
as CountSpecificNeeds--Pivot table alias


-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -
--Now aggregating at the case level 
-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -


-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -
--Aggregation at the case level

DROP TABLE T_SPneedsCaseLevel;

select 
C.[CaseNo],
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

FROM T_SPneedsBreak C

--ORDER BY IndividualID--order
GROUP BY[CaseNo]
    

	  
