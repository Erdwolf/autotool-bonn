-- Table structure for table 'admin'
--

CREATE TABLE admin (
  AdNr int(10) unsigned NOT NULL auto_increment,
  Name varchar(20) NOT NULL default '',
  Passwort varchar(20) NOT NULL default '',
  PRIMARY KEY  (AdNr)
) TYPE=MyISAM;

--
-- Table structure for table 'aufgabe'
--

CREATE TABLE aufgabe (
  ANr int(11) NOT NULL auto_increment,
  Name varchar(30) NOT NULL default '',
  Subject varchar(20) NOT NULL default '',
  VNr int(10) NOT NULL default '0',
  Path varchar(100) binary NOT NULL default '.',
  Highscore enum('low','high','no') NOT NULL default 'no',
  Von datetime NOT NULL default '0000-00-00 00:00:00',
  Bis datetime NOT NULL default '0000-00-00 00:00:00',
  PRIMARY KEY  (ANr)
) TYPE=MyISAM COMMENT='aufgabe';

--
-- Dumping data for table 'aufgabe'
--


INSERT INTO aufgabe VALUES (8,'TM','SUCC',2,'hugs . util challenger ss03/bk/aufgaben','low','2003-01-01 00:00:00','2003-05-05 13:00:00');
INSERT INTO aufgabe VALUES (9,'TM','EXPO',2,'hugs . util challenger ss03/bk/aufgaben','low','2003-01-01 00:00:00','2003-05-05 13:00:00');
INSERT INTO aufgabe VALUES (10,'TM','STEP1',2,'hugs . util challenger ss03/bk/aufgaben','low','2003-01-01 00:00:00','2003-05-05 13:00:00');
INSERT INTO aufgabe VALUES (11,'TM','STEP2',2,'hugs . util challenger ss03/bk/aufgaben','low','2003-01-01 00:00:00','2003-05-05 13:00:00');
INSERT INTO aufgabe VALUES (12,'TM','STEP3',2,'hugs . util challenger ss03/bk/aufgaben','low','2003-01-01 00:00:00','2003-05-05 13:00:00');
INSERT INTO aufgabe VALUES (13,'TM','STEP4',2,'hugs . util challenger ss03/bk/aufgaben','low','2003-01-01 00:00:00','2003-06-09 13:00:00');
INSERT INTO aufgabe VALUES (14,'TM','BIN',2,'hugs . util challenger ss03/bk/aufgaben','low','2003-01-01 00:00:00','2003-05-05 13:00:00');
INSERT INTO aufgabe VALUES (15,'LOOP','TIMES',2,'hugs . util challenger ss03/bk/aufgaben','low','2003-05-07 13:00:00','2003-07-21 13:00:00');
INSERT INTO aufgabe VALUES (16,'LOOP','SQRT',2,'hugs . util challenger ss03/bk/aufgaben','low','2003-05-07 13:00:00','2003-07-21 13:00:00');
INSERT INTO aufgabe VALUES (17,'LOOP','PRIM',2,'hugs . util challenger ss03/bk/aufgaben','low','2003-05-07 13:00:00','2003-07-21 13:00:00');
INSERT INTO aufgabe VALUES (18,'LOOP','FIB',2,'hugs . util challenger ss03/bk/aufgaben','low','2003-05-07 13:00:00','2003-07-21 13:00:00');
INSERT INTO aufgabe VALUES (19,'LOOP','BIG',2,'hugs . util challenger ss03/bk/aufgaben','high','2003-05-07 13:00:00','2003-05-19 13:00:00');
INSERT INTO aufgabe VALUES (20,'TM','FACTOR',2,'hugs . util challenger ss03/bk/aufgaben','low','2003-05-07 13:00:00','2003-05-19 13:00:00');
INSERT INTO aufgabe VALUES (21,'FUN','TIMES',2,'hugs . util ss03/bk/aufgaben','low','2003-01-01 13:00:00','2003-01-01 13:00:00');
INSERT INTO aufgabe VALUES (22,'FUN','SQRT',2,'hugs . util ss03/bk/aufgaben','low','2003-05-20 13:00:00','2003-06-02 13:00:00');
INSERT INTO aufgabe VALUES (23,'FUN','FIB',2,'hugs . util ss03/bk/aufgaben','low','2003-05-20 13:00:00','2003-06-02 13:00:00');
INSERT INTO aufgabe VALUES (24,'FUN','QUIZ',2,'hugs . util ss03/bk/aufgaben','low','2003-05-20 13:00:00','2003-06-09 13:00:00');
INSERT INTO aufgabe VALUES (25,'NFA','1',2,'hugs . util ss03/bk/aufgaben','low','2003-01-01 13:00:00','2003-01-01 13:00:00');
INSERT INTO aufgabe VALUES (26,'NFA','2',2,'hugs . util ss03/bk/aufgaben','low','2003-01-01 13:00:00','2003-01-01 13:00:00');
INSERT INTO aufgabe VALUES (27,'NFA','3',2,'hugs . util ss03/bk/aufgaben','low','2003-01-01 13:00:00','2003-01-01 13:00:00');
INSERT INTO aufgabe VALUES (28,'PCP','QUIZ',2,'hugs . util challenger ss03/bk/aufgaben','low','2003-06-27 15:00:00','2003-07-07 13:00:00');
INSERT INTO aufgabe VALUES (29,'SAT','QUIZ',2,'hugs . util challenger ss03/bk/aufgaben','low','2003-06-30 13:00:00','2003-07-07 13:00:00');

--
-- Table structure for table 'gruppe'
--

CREATE TABLE gruppe (
  GNr int(10) unsigned NOT NULL auto_increment,
  VNr int(10) unsigned NOT NULL default '0',
  Name varchar(100) default 'GruppenName <empty>',
  MaxStudents int(10) unsigned NOT NULL default '0',
  Referent varchar(60) NOT NULL default 'unbekannt',
  PRIMARY KEY  (GNr)
) TYPE=MyISAM;

--
-- Dumping data for table 'gruppe'
--


INSERT INTO gruppe VALUES (12,1,'Do 9.15 B-Woche',0,'unbekannt');
INSERT INTO gruppe VALUES (11,1,'Do 9.15 A-Woche',0,'unbekannt');
INSERT INTO gruppe VALUES (10,1,'Fr 9.15 B-Woche',0,'R. Hartwig');
INSERT INTO gruppe VALUES (9,1,'Fr 9.15 A-Woche',0,'R. Hartwig');
INSERT INTO gruppe VALUES (8,1,'Di 13.15 B-Woche',0,'R. Hartwig');
INSERT INTO gruppe VALUES (7,1,'Di 13.15 A-Woche',0,'R. Hartwig');
INSERT INTO gruppe VALUES (13,1,'Do 13.15 A-Woche',0,'unbekannt');
INSERT INTO gruppe VALUES (14,1,'Do 13.15 B-Woche',0,'unbekannt');
INSERT INTO gruppe VALUES (15,2,'Di 9.15 A-Woche',30,'J. Waldmann');
INSERT INTO gruppe VALUES (16,2,'Di 9.15 B-Woche',30,'J. Waldmann');
INSERT INTO gruppe VALUES (17,2,'Do 9.15 A-Woche',30,'R. Hartwig');
INSERT INTO gruppe VALUES (18,2,'Do 9.15 B-Woche',30,'R. Hartwig');
INSERT INTO gruppe VALUES (19,2,'Mo 13.15 A-Woche',30,'R. Hartwig');
INSERT INTO gruppe VALUES (20,2,'Mo 13.15 B-Woche',30,'R. Hartwig');
INSERT INTO gruppe VALUES (21,3,'keine',10000,'');

--
-- Table structure for table 'punkte'
--

CREATE TABLE punkte (
  Rubrik varchar(25) default NULL,
  MNr varchar(12) default NULL,
  punkte int(11) default NULL
) TYPE=MyISAM;

--
-- Dumping data for table 'punkte'
--


--
-- Table structure for table 'stud_aufg'
--

CREATE TABLE stud_aufg (
  SNr int(10) unsigned NOT NULL default '0',
  ANr int(10) unsigned NOT NULL default '0',
  Ok int(11) default NULL,
  No int(11) default NULL,
  size int(11) default NULL,
  Scoretime datetime NOT NULL default '0000-00-00 00:00:00',
  PRIMARY KEY  (SNr,ANr)
) TYPE=MyISAM COMMENT='Aufgaben Student';

--
-- Dumping data for table 'stud_aufg'
--


INSERT INTO stud_aufg VALUES (1,13,0,3,NULL,'0000-00-00 00:00:00');
INSERT INTO stud_aufg VALUES (2,9,3,9,5,'0000-00-00 00:00:00');
INSERT INTO stud_aufg VALUES (3,11,1,4,4,'0000-00-00 00:00:00');
INSERT INTO stud_aufg VALUES (4,10,1,1,NULL,'0000-00-00 00:00:00');
INSERT INTO stud_aufg VALUES (1,8,7,1,3,'2003-04-24 20:13:09');
INSERT INTO stud_aufg VALUES (2,11,1,3,NULL,'0000-00-00 00:00:00');
INSERT INTO stud_aufg VALUES (3,10,1,2,NULL,'0000-00-00 00:00:00');

--
-- Table structure for table 'stud_grp'
--

CREATE TABLE stud_grp (
  SNr int(10) unsigned NOT NULL default '0',
  GNr int(10) unsigned NOT NULL default '0',
  PRIMARY KEY  (SNr,GNr)
) TYPE=MyISAM;

--
-- Dumping data for table 'stud_grp'
--


INSERT INTO stud_grp VALUES (27,17);

--
-- Table structure for table 'student'
--

CREATE TABLE student (
  SNr int(10) unsigned NOT NULL auto_increment,
  Passwort varchar(20) NOT NULL default 'empty',
  MNr varchar(12) NOT NULL default '',
  Name varchar(25) NOT NULL default '',
  Vorname varchar(25) NOT NULL default '',
  Email varchar(150) NOT NULL default '',
  Status varchar(10) NOT NULL default 'inaktiv',
  PRIMARY KEY  (SNr)
) TYPE=MyISAM COMMENT='angemeldete Studenten';

--
-- Dumping data for table 'student'
--

INSERT INTO student VALUES (1,'empty','7777','Marxy','Karl','karl@marx','inaktiv');
INSERT INTO student VALUES (2,'111111','8900000','heile','heile','lishapei@hotmail.com','inaktiv');
INSERT INTO student VALUES (3,'empty','1234','meier','klaus','sternhaus@yahoo.de','inaktiv');
INSERT INTO student VALUES (4,'guest','555','-','-','guest','inaktiv');

--
-- Table structure for table 'vorlesung'
--

CREATE TABLE vorlesung (
  VNr int(10) unsigned NOT NULL auto_increment,
  Name varchar(50) NOT NULL default '',
  PRIMARY KEY  (VNr)
) TYPE=MyISAM COMMENT='vorlesung';

--
-- Dumping data for table 'vorlesung'
--


INSERT INTO vorlesung VALUES (1,'Logik');
INSERT INTO vorlesung VALUES (2,'Berechenbarkeit und Komplexitaet');
INSERT INTO vorlesung VALUES (3,'');

