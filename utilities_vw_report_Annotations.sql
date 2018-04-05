ALTER  VIEW [dbo].[vw_report_Annotations]
AS
SELECT
  tblEvent.tblEventID as EventID,
  tblEvent.NameOfEvent as event_name,
  tblSmartImage.tblSmartImageID as sample,
  tblAnnotations.tblSampleID,
  xArea.Code as ices_area,
  xArea.Code as fao_code,
  CatchDate as catch_date,
  a.age as age,
  FishLength as length,
  FishWeight as weight,
  xMat.Code as maturity,
  xSex.Code as sex,
  tblAnnotations.SmartUser as reader_name,
  tblAnnotations.SmartUser as reader_lastname,
  Number as reader_number,
  ExpertiseLevel as expertise,
  xCountry.Description as country,
  NULL as institution,
  upper(xCountry.Code) as iso_code,
  'R' + FORMAT(Number, '00') + ' ' + upper(xCountry.Code) as reader,
  tblAnnotations.tblAnnotationID as AnnotationID,
  tblSamples.FishID
FROM
  tblAnnotations
left join
  tblSmartImage
on
  tblSmartImage.tblSmartImageID = tblAnnotations.tblSmartImageID
left join
  tblSamples on tblAnnotations.tblSampleID = tblSamples.tblSampleID
left join
  tblEventParticipants
on tblEventParticipants.tblEventID = tblSamples.tblEventID and
   tblEventParticipants.SmartUser = tblAnnotations.SmartUser
left join
  tblEvent
on
  tblEvent.tblEventID = tblSamples.tblEventID
left join
  (select tblAnnotationID, count(tblDotsID) as age
   from tblDots
   group by tblAnnotationID) as a
on
  tblAnnotations.tblAnnotationID = a.tblAnnotationID
left join
  tblDoYouHaveAccess
on
  tblDoYouHaveAccess.SmartUser = tblEventParticipants.SmartUser
left join
  tblCode as xArea
on
  xArea.tblCodeID = tblSamples.tblCodeID_AreaCode
left join
  tblCode as xSex
on
  xSex.tblCodeID = tblSamples.tblCodeID_Sex
left join
  tblCode as xMat
on
  xMat.tblCodeID = tblSamples.tblCodeID_MaturityStage
left join
  tblCode as xCountry
on
  xCountry.tblCodeID = tblDoYouHaveAccess.tblCodeID_Country
where
  tblAnnotations.IsApproved = 1 and
  tblEventParticipants.Number is not null and
  a.age is not null
