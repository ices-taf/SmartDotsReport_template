ALTER  VIEW [dbo].[vw_report_DotsDistances]
AS
SELECT
  tblDots.tblEventID as EventID,
  'R' + FORMAT(reader_number, '00') as reader1,
  tblSmartImage.tblSmartImageID as sample,
  tblAnnotations.tblAnnotationID as AnnotationID,
  reader_number,
  tblAnnotations.SmartUser as smartUser,
  DotIndex as mark,
  ((X-X1) * cos(-theta) - (Y-Y1) * sin(-theta)) / Scale as distance,
  (X-X1) * cos(-theta) - (Y-Y1) * sin(-theta) as pixelDistance,
  Scale as pixelsPerMillimeter,
  xArea.Code as ices_area,
  'R' + FORMAT(reader_number, '00') + ' ' + upper(xCountry.Code) as reader
FROM
  tblDots
left join
  (select *, atn2(Y2 - Y1, X2 - X1) as theta
   from
     tblLines
   where
     abs(Y2 - Y1) > 0 and abs(X2 - X1) > 0) as xLines
on
  tblDots.tblAnnotationID = xLines.tblAnnotationID
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
  (select tblEventID, SmartUser, Number as reader_number
   from tblEventParticipants) as xReader
on
  xReader.tblEventID = tblSamples.tblEventID and
  xReader.SmartUser = tblAnnotations.SmartUser
where
  tblAnnotations.IsApproved = 1 and
  xReader.reader_number is not null
