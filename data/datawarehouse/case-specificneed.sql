

SET ANSI_NULLS ON
GO

SET QUOTED_IDENTIFIER ON
GO


CREATE VIEW [dbo].[casespecificneed]
AS

SELECT  A.CaseNo,
      spa.[IndividualID],
      spa.[VulnerabilityCode],
      spc.[VulnerabilityText],
      spa.[SPNeeds],
      spb.[VulnerabilityDetailsText]--,
     -- spa.[VulnerabilityActive],
    --  spa.[SPNStatus]

FROM dbo.fn_getActiveIndividuals('31/07/2015') A
  INNER JOIN
         [DWH].[dbo].[T_SPneedswithdetails] spa
   ON (A.IndividualID = spa.IndividualID)

   LEFT OUTER JOIN
         (SELECT  [VulnerabilityDetailsCode],[VulnerabilityDetailsText]
          FROM [proGres].[dbo].[codeVulnerabilityDetailsText]   WHERE [VulnerabilityDetailsLanguageCode] = 'ENG' ) spb
  ON spa.[SPNeeds] = spb.[VulnerabilityDetailsCode]

  LEFT OUTER JOIN
          (SELECT [VulnerabilityCode],[VulnerabilityText]
          FROM [proGres].[dbo].[codeVulnerabilityText] WHERE [VulnerabilityLanguageCode] = 'ENG' ) spc
  ON spa.[VulnerabilityCode] = spc.[VulnerabilityCode]

  WHERE spa.[VulnerabilityActive] = '1'

  --GROUP BY A.CaseNo, spa.[IndividualID],  spa.[VulnerabilityCode]
 -- ORDER BY A.CaseNo, spb.[VulnerabilityDetailsText]


 SELECT DISTINCT [VulnerabilityCode], [SPNeeds] FROM     [DWH].[dbo].[T_SPneedswithdetails]


 ---- Using pivot functions
--DROP TABLE T_SPneedsCaseInd;

 SELECT *

 INTO  T_SPneedsCaseInd

 FROM


            (SELECT A.IndividualGUID, A.[IndividualID], A.[SPNeeds], A.[SPNeeds] as code, B.[CaseNo]  -- Colums to pivot
                    FROM [T_SPneedswithdetails] A
                    LEFT OUTER  JOIN dbo.T_AllIndividuals B ON A.IndividualID = B.IndividualID
                    WHERE [VulnerabilityActive] = '1'
                    AND  SPNeeds in (
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
                as  sourcetable

                pivot (
                  COUNT(IndividualGUID)   -- Pivot on this column

                for [SPNeeds] -- Make colum where SPNeeds is in one of these.

                 in ([CR], [CR-AF], [CR-CC], [CR-CH], [CR-CL], [CR-CP], [CR-CS],
                     [CR-LO], [CR-LW], [CR-MS], [CR-NE], [CR-SE], [CR-TP], [DS], [DS-BD], [DS-DF], [DS-MM], [DS-MS], [DS-PM],
                     [DS-PS], [DS-SD], [ER], [ER-FR], [ER-MC], [ER-NF], [ER-OC], [ER-SC], [ER-UR], [FU], [FU-FR], [FU-TR],
                     [LP], [LP-AF], [LP-AN], [LP-AP], [LP-BN], [LP-CR], [LP-DA], [LP-DN], [LP-DO], [LP-DP], [LP-DT], [LP-ES],
                     [LP-FR], [LP-IH], [LP-LS], [LP-MD], [LP-MM], [LP-MS], [LP-NA], [LP-ND], [LP-PV], [LP-RD], [LP-RP], [LP-RR],
                     [LP-ST], [LP-TA], [LP-TC], [LP-TD], [LP-TO], [LP-TR], [LP-UP], [LP-VA], [LP-VF], [LP-VO], [LP-VP], [LP-WP],
                     [PG], [PG-HR], [PG-LC], [SC], [SC-CH], [SC-FC], [SC-IC], [SC-NC], [SC-SC], [SC-UC], [SC-UF], [SC-UM],
                     [SM], [SM-AD], [SM-CC], [SM-CI], [SM-DP], [SM-MI], [SM-MN], [SM-OT], [SP], [SP-CG], [SP-GP], [SP-PT],
                     [SV], [SV-FM], [SV-GM], [SV-HK], [SV-HP], [SV-SS], [SV-VA], [SV-VF], [SV-VO], [TR], [TR-HO], [TR-PI],
                     [TR-WV], [WR], [WR-GM], [WR-HR],
                     [WR-LC], [WR-PY], [WR-SF], [WR-UW], [WR-WF], [WR-WR])
                     )
                as CountSpecificNeeds                 -- Pivot table alias



---------------------------------------------------
-- Aggregation at the case level

--DROP TABLE T_SPneedsCaseLevel;

select C.[CaseNo],
            SUM(C.[CR]) as Child_at_risk,
            SUM(C.[CR-AF]) as Child_at_risk_associated_with_armed_forces_or_groups,
            SUM(C.[CR-CC]) as Child_at_risk_carer,
            SUM(C.[CR-CH]) as Child_at_risk_headed_household,
            SUM(C.[CR-CL]) as Child_at_risk_in_conflict_with_the_law,
            SUM(C.[CR-CP]) as Child_at_risk_parent,
            SUM(C.[CR-CS]) as Child_at_risk_spouse,
            SUM(C.[CR-LO]) as Child_at_risk_engaged_in_other_forms_of_child_labour,
            SUM(C.[CR-LW]) as Child_at_risk_engaged_in_worst_forms_of_child_labour,
            SUM(C.[CR-MS]) as Child_at_riskMinor_spouse,
            SUM(C.[CR-NE]) as Child_at_risk_of_not_attending_school,
            SUM(C.[CR-SE]) as Child_at_risk_with_special_education_needs,
            SUM(C.[CR-TP]) as Child_at_risk_Teenage_pregnancy,
            SUM(C.[DS]) as Disability,
            SUM(C.[DS-BD]) as Disability_Visual_impairment_including_blindness_,
            SUM(C.[DS-DF]) as Disability_Hearing_Impairment_including_deafness_,
            SUM(C.[DS-MM]) as Disability_Mental_disability_moderate,
            SUM(C.[DS-MS]) as Disability_Mental_disability_severe,
            SUM(C.[DS-PM]) as Disability_Physical_disability_moderate,
            SUM(C.[DS-PS]) as Disability_Physical_disability_severe,
            SUM(C.[DS-SD]) as Disability_Speech_impairment_disability,
            SUM(C.[ER]) as Older_person_at_risk,
            SUM(C.[ER-FR]) as Older_person_at_risk_unable_to_care_for_self,
            SUM(C.[ER-MC]) as Older_person_at_risk_with_children,
            SUM(C.[ER-NF]) as Older_person_at_risk_Unaccompanied_older_person,
            SUM(C.[ER-OC]) as Older_person_at_risk_without_younger_family_members,
            SUM(C.[ER-SC]) as Older_person_at_risk_with_separated_children,
            SUM(C.[ER-UR]) as Older_person_at_risk_Single_without_accompy_family_members,
            SUM(C.[FU]) as Family_unity,
            SUM(C.[FU-FR]) as Family_unity_reunification_required,
            SUM(C.[FU-TR]) as Family_unity_Tracing_required,
            SUM(C.[LP]) as Specific_legal_and_physical_protection_needs,
            SUM(C.[LP-AF]) as Specific_legal_and_physical_protection_needs_Formerly_associated_with_armed_forces_or_groups,
            SUM(C.[LP-AN]) as Specific_legal_and_physical_protection_needs_Violence_abuse_or_neglect,
            SUM(C.[LP-AP]) as Specific_legal_and_physical_protection_needs_Alleged_perpetrator,
            SUM(C.[LP-BN]) as Specific_legal_and_physical_protection_needs_Unmet_basic_needs,
            SUM(C.[LP-CR]) as Specific_legal_and_physical_protection_needs_Criminal_record,
            SUM(C.[LP-DA]) as Specific_legal_and_physical_protection_needs_Detained_held_in_country_of_asylum,
            SUM(C.[LP-DN]) as Specific_legal_and_physical_protection_needs_Currently_detained_held_in_country_of_asylum,
            SUM(C.[LP-DO]) as Specific_legal_and_physical_protection_needs_Detained_held_in_country_of_origin,
            SUM(C.[LP-DP]) as Specific_legal_and_physical_protection_needs_Formerly_detained_held_in_country_of_asylum,
            SUM(C.[LP-DT]) as Specific_legal_and_physical_protection_needs_Detained_held_elsewhere,
            SUM(C.[LP-ES]) as Specific_legal_and_physical_protection_needs_Individual_excluded_or_marginalised_from_society,
            SUM(C.[LP-FR]) as Specific_legal_and_physical_protection_needs_Family_reunion_required,
            SUM(C.[LP-IH]) as Specific_legal_and_physical_protection_needs_In_hiding,
            SUM(C.[LP-LS]) as Specific_legal_and_physical_protection_needs_Lack_of_durable_solutions_prospects,
            SUM(C.[LP-MD]) as Specific_legal_and_physical_protection_needs_Multiple_displacements,
            SUM(C.[LP-MM]) as Specific_legal_and_physical_protection_needs_Mixed_marriage,
            SUM(C.[LP-MS]) as Specific_legal_and_physical_protection_needs_Marginalized_from_society_or_community,
            SUM(C.[LP-NA]) as Specific_legal_and_physical_protection_needs_No_access_to_services,
            SUM(C.[LP-ND]) as Specific_legal_and_physical_protection_needs_No_legal_documentation,
            SUM(C.[LP-PV]) as Specific_legal_and_physical_protection_needs_Durable_solutions_related_vulnerability,
            SUM(C.[LP-RD]) as Specific_legal_and_physical_protection_needs_At_risk_of_removal,
            SUM(C.[LP-RP]) as Specific_legal_and_physical_protection_needs_At_risk_due_to_profile,
            SUM(C.[LP-RR]) as Specific_legal_and_physical_protection_needs_At_risk_of_refoulement,
            SUM(C.[LP-ST]) as Specific_legal_and_physical_protection_needs_Security_threat_to_UNHCR_partner_staff_or_others,
            SUM(C.[LP-TA]) as Specific_legal_and_physical_protection_needs_Survivor_of_torture_violence_in_asylum,
            SUM(C.[LP-TC]) as Specific_legal_and_physical_protection_needs_Tracing_required,
            SUM(C.[LP-TD]) as Specific_legal_and_physical_protection_needs_At_risk_of_deportation,
            SUM(C.[LP-TO]) as Specific_legal_and_physical_protection_needs_Survivor_of_torture_violence_in_home_country,
            SUM(C.[LP-TR]) as Specific_legal_and_physical_protection_needs_At_risk_of_refoulement2,
            SUM(C.[LP-UP]) as Specific_legal_and_physical_protection_needs_Urgent_need_of_physical_protection,
            SUM(C.[LP-VA]) as Specific_legal_and_physical_protection_needs_Victim_of_domestic_violence_SGBV_in_asylum,
            SUM(C.[LP-VF]) as Specific_legal_and_physical_protection_needs_Victim_of_domestic_violence_SGBV_during_flight,
            SUM(C.[LP-VO]) as Specific_legal_and_physical_protection_needs_Victim_of_domestic_violence_SGBV_in_home_country,
            SUM(C.[LP-VP]) as Specific_legal_and_physical_protection_needs_Alleged_perpetrator_of_violence,
            SUM(C.[LP-WP]) as Specific_legal_and_physical_protection_needs_Absence_of_witness_protection,
            SUM(C.[PG]) as Pregnant_or_lactating,
            SUM(C.[PG-HR]) as Pregnant_or_lactating_High_risk_pregnancy,
            SUM(C.[PG-LC]) as Pregnant_or_lactating_Lactating,
            SUM(C.[SC]) as Unaccompanied_or_separated_child,
            SUM(C.[SC-CH]) as Unaccompanied_or_separated_Single_Child_headed_household,
            SUM(C.[SC-FC]) as Unaccompanied_or_separated_Child_in_foster_care,
            SUM(C.[SC-IC]) as Unaccompanied_or_separated_Child_in_institutional_care,
            SUM(C.[SC-NC]) as Unaccompanied_or_separated_Neglected_child_with_extended_family,
            SUM(C.[SC-SC]) as Unaccompanied_or_separated_child_Separated_child,
            SUM(C.[SC-UC]) as Unaccompanied_or_separated_child_Unaccompanied_child,
            SUM(C.[SC-UF]) as Unaccompanied_or_separated_child_Child_in_foster_care2,
            SUM(C.[SC-UM]) as Unaccompanied_or_separated_child_Unaccompanied_minor,
            SUM(C.[SM]) as Serious_medical_condition,
            SUM(C.[SM-AD]) as Serious_medical_condition_Addiction,
            SUM(C.[SM-CC]) as Serious_medical_condition_Critical_medical,
            SUM(C.[SM-CI]) as Serious_medical_condition_Chronic_illness,
            SUM(C.[SM-DP]) as Serious_medical_condition_Difficult_pregnancy,
            SUM(C.[SM-MI]) as Serious_medical_condition_Mental_illness,
            SUM(C.[SM-MN]) as Serious_medical_condition_Malnutrition,
            SUM(C.[SM-OT]) as Serious_medical_condition_Other_medical_condition,
            SUM(C.[SP]) as Single_parent,
            SUM(C.[SP-CG]) as Single_parent_Single_HR_caregiver,
            SUM(C.[SP-GP]) as Single_parent_Single_HR_grandparent,
            SUM(C.[SP-PT]) as Single_parent_Single_HR_parent,
            SUM(C.[SV]) as SGBV,
            SUM(C.[SV-FM]) as SGBV_Threat_of_forced_marriage,
            SUM(C.[SV-GM]) as SGBV_Female_genital_mutilation,
            SUM(C.[SV-HK]) as SGBV_Threat_of_honour_killing_violence,
            SUM(C.[SV-HP]) as SGBV_Harmful_traditional_practices,
            SUM(C.[SV-SS]) as SGBV_Survival_sex,
            SUM(C.[SV-VA]) as SGBV_Exposure_to_SGBV,
            SUM(C.[SV-VF]) as SGBV_Exposure_to_SGBV_during_flight,
            SUM(C.[SV-VO]) as SGBV_Exposure_to_SGBV_in_country_of_origin,
            SUM(C.[TR]) as Torture,
            SUM(C.[TR-HO]) as Torture_Forced_to_egregious_acts,
            SUM(C.[TR-PI]) as Torture_Psych_and_or_physical_impairment_due_to_torture,
            SUM(C.[TR-WV]) as Torture_Witness_of_violence_to_other,
            SUM(C.[WR]) as Woman_at_risk,
            SUM(C.[WR-GM]) as Woman_at_risk_Threat_of_female_genital_mutilation,
            SUM(C.[WR-HR]) as Woman_at_risk_Single_female_household_representative,
            SUM(C.[WR-LC]) as Woman_at_risk_Lactating_at_risk,
            SUM(C.[WR-PY]) as Woman_at_risk_In_polygamous_marriage_or_relationship,
            SUM(C.[WR-SF]) as Woman_at_risk_Single_woman,
            SUM(C.[WR-UW]) as Woman_at_risk_Woman_unaccompanied_by_adult_male_family_member,
            SUM(C.[WR-WF]) as Woman_at_risk_Woman_associated_with_fighting_forces,
            SUM(C.[WR-WR]) as Woman_at_risk_Woman_at_risk_unspecified

INTO T_SPneedsCaseLevel


    FROM T_SPneedsCaseInd

--ORDER BY IndividualID       -- order
GROUP BY [CaseNo]