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
  isnull(a.age, 0) as age,
  FishLength as length,
  FishWeight as weight,
  xMat.Code as maturity,
  xSex.Code as sex,
  xPrepMethod.Description as prep_method,
  xStock.Code as stock,
  Number as reader_number,
  ExpertiseLevel as expertise,
  xCountry.Description as country,
  NULL as institute,
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
left join
  tblCode as xPrepMethod
on
  xPrepMethod.tblCodeID = tblSamples.tblCodeID_PreparationMethod
left join
  tblCode as xStock
on
  xStock.tblCodeID = tblSamples.tblCodeID_StockCode
where
  tblEventParticipants.Number is not null
