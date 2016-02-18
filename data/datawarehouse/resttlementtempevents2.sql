

--DROP TABLE [T_resettlementEvents];
select * ,  
    RECORDRANK  = RANK() OVER (PARTITION BY [IndividualID],[EventID] ORDER BY [EventLogEffectiveDate] DESC ),
    RECORDRANKRESULT  = RANK() OVER (PARTITION BY [IndividualID],[ResultID] ORDER BY [EventLogResultEffectiveDate] DESC )
    into T_resettlementEvents
    from

            (SELECT
                             [IndividualID],
                             [CaseNo],
                             [EventID],
                             [ResultID],
                             CONVERT(VARCHAR(10), [EventLogEffectiveDate], 103) as EventLogEffectiveDate,
                             [EventLogstatus],
                             isnull(CONVERT(VARCHAR(10), [EventLogResultEffectiveDate], 103),CONVERT(VARCHAR(10), [EventLogEffectiveDate], 103)) as EventLogResultEffectiveDate ,
                             [ReasonCode]

                FROM [DWH].[dbo].[T_Events]

                WHERE [EventLogstatus] <> 'x'
            -- AND [ResultID] in ('RST04','RST06','RST19')
                AND  LEFT( [EventID], 3) = 'RST'
                OR [EventID] = 'REG38'
                OR [EventID] = 'VOL26'

                )
                    AS t
ORDER BY [IndividualID],[EventID]


 ---------------------------------------------------------
--- Now reformatting from temp table
 ---------------------------------------------------------

--- Information at the individual level

--DROP TABLE T_resettlementEvents2;

-- DROP TABLE T_resettlementEvents2;

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
                         K.EventLogEffectiveDate AS rst33_DepartureConfirmed,
                         K.ReasonCode AS rst33_ReasonDepartureConfirmed,
                         DATEDIFF(day, F.EventLogResultEffectiveDate, K.EventLogEffectiveDate) as rst33_19,
                         N.EventLogEffectiveDate AS reg38_SpontaneousDeparture,
                         N.ReasonCode AS reg38_ReasonSpontaneousDeparture,
                         DATEDIFF(day, J.RegisteredOn, N.EventLogEffectiveDate) as registered_Spontaneousdepart, 
                         O.EventLogEffectiveDate AS vol26_VolrepDepartureConfirmed,
                         O.ReasonCode AS vol26_ReasonVolrepDepartureConfirmed,
                         DATEDIFF(day, J.RegisteredOn, O.EventLogEffectiveDate) as registered_VolDepart, 
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
                         J.EducationLevelCode,
                         J.HasSPNeed,
                         J.RefStatus,
                         J.RefStatCategory

	            into T_resettlementEvents2
                    FROM    dbo.T_AllIndividuals AS J

                               LEFT OUTER JOIN
                             (SELECT        IndividualID, EventLogResultEffectiveDate, ReasonCode, RECORDRANK
                               FROM            dbo.T_resettlementEvents AS CE
                               WHERE        (ResultID = 'RST06') AND (RECORDRANKRESULT = 1)) AS C ON J.IndividualID = C.IndividualID

                               LEFT OUTER JOIN
                             (SELECT        IndividualID, EventLogEffectiveDate, ReasonCode, RECORDRANK
                               FROM            dbo.T_resettlementEvents AS DE
                               WHERE        (EventID = 'RST09') AND (RECORDRANK = 1)) AS D ON J.IndividualID = D.IndividualID

                               LEFT OUTER JOIN
                             (SELECT        IndividualID, EventLogEffectiveDate, ReasonCode, RECORDRANK
                               FROM            dbo.T_resettlementEvents AS EE
                               WHERE        (EventID = 'RST43') AND (RECORDRANK = 1)) AS E ON J.IndividualID = E.IndividualID

                               LEFT OUTER JOIN
                             (SELECT        IndividualID, EventLogResultEffectiveDate, ReasonCode, RECORDRANK
                               FROM            dbo.T_resettlementEvents AS FE
                               WHERE        (ResultID = 'RST19') AND (RECORDRANKRESULT = 1)) AS F ON J.IndividualID = F.IndividualID

                               LEFT OUTER JOIN
                             (SELECT        IndividualID, EventLogEffectiveDate, ReasonCode, RECORDRANK
                               FROM            dbo.T_resettlementEvents AS GE
                               WHERE        (EventID = 'RST18') AND (RECORDRANK = 1)) AS G ON J.IndividualID = G.IndividualID

                               LEFT OUTER JOIN
                             (SELECT        IndividualID, EventLogEffectiveDate, ReasonCode, RECORDRANK
                               FROM            dbo.T_resettlementEvents AS HE
                               WHERE        (EventID = 'RST21') AND (RECORDRANK = 1)) AS H ON J.IndividualID = H.IndividualID

                               LEFT OUTER JOIN
                             (SELECT        IndividualID, EventLogEffectiveDate, ReasonCode, RECORDRANK
                               FROM            dbo.T_resettlementEvents AS KE
                               WHERE        (EventID = 'RST33') AND (RECORDRANK = 1)) AS K ON J.IndividualID = K.IndividualID

                               LEFT OUTER JOIN
                             (SELECT        IndividualID, EventLogEffectiveDate, ReasonCode, RECORDRANK
                               FROM            dbo.T_resettlementEvents AS LE
                               WHERE        (EventID = 'RST01') AND (RECORDRANK = 1)) AS L ON J.IndividualID = L.IndividualID

                               LEFT OUTER JOIN
                             (SELECT        IndividualID, EventLogResultEffectiveDate, ReasonCode, RECORDRANK
                               FROM            dbo.T_resettlementEvents AS ME
                               WHERE        (ResultID = 'RST04') AND (RECORDRANKRESULT = 1)) AS M ON J.IndividualID = M.IndividualID

                               LEFT OUTER JOIN
                             (SELECT        IndividualID, EventLogEffectiveDate, ReasonCode, RECORDRANK
                               FROM            dbo.T_resettlementEvents AS NE
                               WHERE        (EventID = 'REG38') AND (RECORDRANK = 1)) AS N ON J.IndividualID = N.IndividualID

                               LEFT OUTER JOIN
                             (SELECT        IndividualID, EventLogEffectiveDate, ReasonCode, RECORDRANK
                               FROM            dbo.T_resettlementEvents AS OE
                               WHERE        (EventID = 'VOL26') AND (RECORDRANK = 1)) AS O ON J.IndividualID = O.IndividualID

WHERE        (C.EventLogResultEffectiveDate IS NOT NULL) OR
                         (D.EventLogEffectiveDate IS NOT NULL) OR
                         (E.EventLogEffectiveDate IS NOT NULL) OR
                         (F.EventLogResultEffectiveDate IS NOT NULL) OR
                         (G.EventLogEffectiveDate IS NOT NULL) OR
                         (H.EventLogEffectiveDate IS NOT NULL) OR
                         (K.EventLogEffectiveDate IS NOT NULL) OR
                         (L.EventLogEffectiveDate IS NOT NULL) OR
                         (M.EventLogResultEffectiveDate IS NOT NULL)OR
                         (N.EventLogEffectiveDate IS NOT NULL)OR
                         (O.EventLogEffectiveDate IS NOT NULL)



 ---------------------------------------------------------
-- Now aggregating at the case level and computing delays
 ---------------------------------------------------------

 -- 355 991

   SELECT DISTINCT [CaseNo]  FROM [DWH].[dbo].[T_resettlementEvents2] ORDER BY [CaseNo]

 -- 297 486
 SELECT  DISTINCT [CaseNo]
      ,[CurrentSize]
      ,[TotalSize]
      ,[Current_process_Status]
      ,[rst01_considered]
      ,[rst01_Reasonconsidered]
      ,[rst04_notqualified]
      ,[rst04_Reasonnotqualified]
      ,[rst06_Interview]
      ,[rst06_ReasonInterview]
      ,[rst09_Assessment_Complete]
      ,[rst09_ReasonAssessment_Complete]
      ,[rst43_Referred_to_Hub]
      ,[rst43_ReasonReferred_to_Hub]
      ,[rst18_CaseSubmitted]
      ,[rst18_ReasonCaseSubmitted]
      ,[rst21_CaseResbmitted]
      ,[rst21_ReasonCaseResbmitted]
      ,[rst19_CaseAccepted]
      ,[rst19_ReasonCaseAccepted]
      ,[rst33_DepartureConfirmed]
      ,[rst33_ReasonDepartureConfirmed]
      ,[CaseCreatedOn]
  FROM [DWH].[dbo].[T_resettlementEvents2] ORDER BY [CaseNo]


 -- 398 088
 SELECT  DISTINCT [CaseNo]
      ,[CurrentSize]
      ,[TotalSize]
      ,[Current_process_Status]
      ,[rst01_considered]
      ,[rst01_Reasonconsidered]
      ,[rst04_notqualified]
      ,[rst04_Reasonnotqualified]
      ,[rst06_Interview]
      ,[rst06_ReasonInterview]
      ,[rst09_Assessment_Complete]
      ,[rst09_ReasonAssessment_Complete]
      ,[rst43_Referred_to_Hub]
      ,[rst43_ReasonReferred_to_Hub]
      ,[rst18_CaseSubmitted]
      ,[rst18_ReasonCaseSubmitted]
      ,[rst21_CaseResbmitted]
      ,[rst21_ReasonCaseResbmitted]
      ,[rst19_CaseAccepted]
      ,[rst19_ReasonCaseAccepted]
      ,[rst33_DepartureConfirmed]
      ,[rst33_ReasonDepartureConfirmed]
      ,[CaseCreatedOn]
      ,[CoO]
      ,[CoA]
      ,[NationalityCode]
      ,[ArrivalDate]
      ,[RegisteredOn]
      ,[RefStatus]
      ,[RefStatCategory]
      ,[RefugeeStatusDate]
  FROM [DWH].[dbo].[T_resettlementEvents2] ORDER BY [CaseNo]

  
  -- 332 491
--DROP TABLE T_resettlementEvents3; 
  
 SELECT [CaseNo]
      ,[CurrentSize]
      ,[TotalSize]
      ,[Current_process_Status]
      ,[rst01_considered]
      ,[rst01_Reasonconsidered]
      ,[registered_rst_01]
      ,[rst04_notqualified]
      ,[rst04_Reasonnotqualified]
      ,[rst04_01]
      ,[rst06_Interview]
      ,[rst06_ReasonInterview]
      ,[rst09_Assessment_Complete]
      ,[rst09_ReasonAssessment_Complete]
      ,[rst09_01]
      ,[rst18_CaseSubmitted]
      ,[rst18_ReasonCaseSubmitted]
      ,[rst18_09]
      ,[rst21_CaseResbmitted]
      ,[rst21_ReasonCaseResbmitted]
      ,[rst19_CaseAccepted]
      ,[rst19_ReasonCaseAccepted]
      ,[rst43_Referred_to_Hub]
      ,[rst43_ReasonReferred_to_Hub]
      ,[rst33_DepartureConfirmed]
      ,[rst33_ReasonDepartureConfirmed]
      ,[reg38_SpontaneousDeparture]
      ,[reg38_ReasonSpontaneousDeparture]
      ,[registered_Spontaneousdepart]
      ,[vol26_VolrepDepartureConfirmed]
      ,[vol26_ReasonVolrepDepartureConfirmed]
      ,[registered_VolDepart]
      ,[CaseCreatedOn]
      ,[CoO]
      ,[CoA]
      ,[NationalityCode]
      ,[ArrivalDate]
      ,[RegisteredOn]
      ,[RefStatus]
      ,[RefStatCategory]
      ,[RefugeeStatusDate]

  INTO T_resettlementEvents3
  FROM [DWH].[dbo].[T_resettlementEvents2]
  WHERE  Relationship = 'PA'
  ORDER BY [CaseNo]