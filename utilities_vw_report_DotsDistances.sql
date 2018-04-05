ALTER  VIEW [dbo].[vw_report_DotsDistances]
AS
SELECT
  tblDots.tblEventID as EventID,
  NULL as test,
  'R' + FORMAT(reader_number, '00') as reader1,
  tblSmartImage.tblSmartImageID as sample,
  reader_number,
  DotIndex as mark,
  tblDots.X as xpos,
  tblDots.Y as ypos,
  1.0 as distance,
  xArea.Code as ices_area,
  'R' + FORMAT(reader_number, '00') + ' ' + upper(xCountry.Code) as reader
FROM
  tblDots
left join
  tblAnnotations
on
  tblDots.tblAnnotationID = tblAnnotations.tblAnnotationID
left join
  tblSmartImage
on
  tblSmartImage.tblSmartImageID = tblAnnotations.tblSmartImageID
left join
  tblSamples
on
  tblSamples.tblSampleID = tblAnnotations.tblSampleID
left join
  tblDoYouHaveAccess
on
  tblDoYouHaveAccess.SmartUser = tblSamples.SmartUser
left join
  tblCode as xArea
on
  xArea.tblCodeID = tblSamples.tblCodeID_AreaCode
left join
  tblCode as xCountry
on
  xCountry.tblCodeID = tblDoYouHaveAccess.tblCodeID_Country
inner join
  (select tblEventID, SmartUser, isnull(Number, 99) as reader_number
   from tblEventParticipants) as xReader
on
  xReader.tblEventID = tblSamples.tblEventID and
  xReader.SmartUser = tblAnnotations.SmartUser
where tblAnnotations.IsApproved = 1

