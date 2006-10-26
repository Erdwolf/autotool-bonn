#!/usr/bin/perl -w


sub wayout {
    print "caught an interrupt\n";
    goto done;
}

$SIG{INT} = 'wayout';
$SIG{ALRM} = 'wayout';

$title = "HILBERT demo output";

$hilbertdir = "/var/www/hilbert";
$hilbertname = "$hilbertdir/hilbert$$";
$hilbertin = "$hilbertname.in";
$hilbertout = "$hilbertname.out";

print "Content-Type: text/html\n\n";
print "<HTML>\n";
print "<HEAD>\n";
print "<TITLE>$title</TITLE>\n";
print "<LINK REV=\"made\" HREF=\"mailto:joe\@informatik.uni-leipzig.de\">";
print "</HEAD>\n";

print "<BODY><H1>$title</H1>\n";

if ($ENV{"REQUEST_METHOD"} eq "POST") {

    read (STDIN, $buffer, $ENV{"CONTENT_LENGTH"});
    @pairs = split (/&/, $buffer);
    foreach $pair (@pairs) {
	($name, $value) = split (/=/, $pair);
	$value =~ tr/+/ /;
	$value =~ s/%([a-fA-F0-9][a-fA-F0-9])/pack("C", hex($1))/eg;
	$FORM{$name} = $value;
    }

    open (HILBERT, "> $hilbertin");
    print HILBERT "$FORM{HILBERT_INPUT}"; 
    close (HILBERT);

    system "cat $hilbertin | $hilbertdir/HILBERT current=code +RTS -H6M -K1M -RTS - > $hilbertout 2>&1";

    open (HILBERT, "< $hilbertout");
    while (<HILBERT>) { 
	print "<BR>\n";
	print "$_\n"; 
    }
    close (HILBERT);
    
    system "date >> $hilbertdir/HILBERT.log";

    open (LOG, ">> $hilbertdir/HILBERT.log");
    print LOG "call $$ from $ENV{'REMOTE_HOST'} = $ENV{'REMOTE_ADDR'}\n";
    close (LOG);

    if ($ENV{'REMOTE_HOST'} eq 'isun11.informatik.uni-leipzig.de') {
	# das bin ich selbst
	system "rm -f $hilbertin; rm -f $hilbertout";
    }

} else {
    print "could not read your input\n";
}

done:

print "<HR><A HREF=\"http://www.informatik.uni-leipzig.de/~joe/hilbert/\">\n";
print "back to <TT>HILBERT</TT></A>\n";
print "<HR><ADDRESS>\n";
print "<A HREF=\"http://www.informatik.uni-leipzig.de/~joe/\">\n";
print "<TT>http://www.informatik.uni-leipzig.de/~joe/</TT></A>\n";
print "<A HREF=\"mailto:joe\@informatik.uni-leipzig.de\">\n";
print "<TT>mailto:joe\@informatik.uni-leipzig.de</TT></A>\n";
print "</ADDRESS>\n";

print "</BODY>";
print "</HTML>\n";


