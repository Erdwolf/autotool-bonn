CREATE TABLE `semester` (
  `ENr` int(11) NOT NULL auto_increment,
  `UNr` int(10) NOT NULL,
  `Name` varchar(30) NOT NULL,
  `Von` datetime NOT NULL,
  `Bis` datetime NOT NULL,
  PRIMARY KEY  (`ENr`)
) TYPE=MyISAM COMMENT='semester';

CREATE TABLE `direktor` (
  `SNr` int(10) default NULL,
  `UNr` int(10) default NULL
) TYPE=MyISAM;


CREATE TABLE `minister` (
  `SNr` int(10) default NULL
) TYPE=MyISAM;

ALTER TABLE vorlesung
    ADD COLUMN ENr int(10);

