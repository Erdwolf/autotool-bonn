
-- $Id$
-------------------------------------------------------------------------------
-- usage: cat eval.sql | mysql -t -u<user> -p<pass> <db>
-------------------------------------------------------------------------------

SELECT COUNT(*) AS 'Anzahl eingeschriebener StudentInnen' 
FROM student 
WHERE MNr > 1023;

-------------------------------------------------------------------------------

SELECT COUNT(*) AS 'Anzahl gestellter Aufgaben insgesamt' FROM aufgabe;

-------------------------------------------------------------------------------

SELECT SUM(OK)+SUM(NO) AS 'Einsendungen insgesamt'
     , SUM(OK) AS 'davon OK'
     , SUM(NO) AS 'davon NO'
     , COUNT(DISTINCT(SA.SNr)) AS 'von verschiedenen StudentInnen'
FROM stud_aufg as SA, student as S
WHERE (S.SNr=SA.SNr) and (S.MNr>1023) and (OK>0 OR No>0);

-------------------------------------------------------------------------------

SELECT SA.SNr AS 'Einsendungen von SNr'
     , SUM(Ok) AS 'OK'
     , SUM(No) AS 'NO'
     , SUM(Ok)+SUM(No) AS 'Gesamt' 
     , COUNT(DISTINCT(SA.ANr)) AS 'zu verschiedenen Aufgaben'
FROM stud_aufg AS SA, student AS S 
WHERE (S.SNr=SA.SNr AND S.MNr>1023) AND (Ok>0 OR No>0) 
GROUP BY SA.SNr;

-------------------------------------------------------------------------------

SELECT Typ AS 'Aufgaben Typ'
     , Count(ANr) AS 'Anzahl Aufgaben dieses Typs' 
FROM aufgabe 
GROUP BY Typ;

-------------------------------------------------------------------------------

SELECT A.Name as 'Aufgabe'
     , A.Typ
     , COUNT(*) as 'Verschiedene StudentInnen, die eingesandt haben'
     , SUM(OK)+SUM(NO) as 'Einsendungen insgesamt'
     , SUM(OK) as 'davon OK'
     , SUM(NO) as 'davon NO'
FROM aufgabe as A,stud_aufg as SA, student as S 
WHERE (A.ANr=SA.ANr and S.SNr=SA.SNr and S.MNr>1023 and (OK>0 or NO>0)) 
GROUP BY A.ANr
ORDER BY A.Typ;

-------------------------------------------------------------------------------
-- longer this list