#! /usr/bin/perl

## ------------------------------------------------------------
## Build file name

#TEST=TESTCASES, REGIONAL or VORTEX
my $TEST = $ARGV[0];

#date
my ($sec,$min,$hour,$mday,$mon,$year,$wday,$yday,$isdst) = localtime(time);
$mymon = $mon+1;

#Day syntax
if ($mday < 10) { 
$mday2="0".$mday
} 
else {
$mday2=$mday
}
print "myday=", $mday, "\n";


#Month syntax
if ($mymon < 10) { 
$mymon2="0".$mymon
} 
else {
$mymon2=$mymon
}
#print $mydate, "\n";
#---------------------------------------

$mydate=1900+$year.$mymon2.$mday2;
print $mydate, "\n";
#---------------------------------------

$res=`date`;
$_ = $res;
if (/([0-9]+:[0-9]+):/) {
#  print "hour= ", $1, "\n";
  $hour = $1;
}

# svn version number
#$res = `svn info /home/gcambon/SVN_3/romsagrif/Roms_tools/Roms_Agrif` ;
#$res = `grep "vision" svninfos | head -n 1` ;
#print "res=", $res, "\n";
#$_ = $res;
#if (/vision\W*:\W*(\d+)/) {
#$SVNnum = $1;
#print "Revision = ", $SVNnum , "\n";
#}

# create output file
$filenameOUT = "Results_".$TEST."_".$mydate;
#print "filenameOUT = ", $filenameOUT , "\n";
open(FILEOUT, ">>$filenameOUT");

$filenameIN = "Recap_".$TEST."_".$mydate;
open(FILEIN, "$filenameIN");

print FILEOUT "DATE:", $mydate, "\n";
print FILEOUT "------------------------", "\n";

## ------------------------------------------------------------
## parse input file
$start = 0;
while(<FILEIN>) {

  # print file header
  if (!$start) {
    print "AAAAAAAAAAAA\n";
    if (/^\s*TEST\s*RVTK_DEBUG/) {
      $start = 1;
    }
    else {
      print FILEOUT $_;
    }
  }

  # look for BUGBIN
  else {
#  # print Level Agrif
   if (/^\s*GRID#/) {
     print "grid#\n";
     # print last level 
      $level=$_;
#      print "LEVEL is",$level,"\n";
#     if ($print_bugbin) {
#      print FILEOUT "\n$bugbin";
#       }
#      $print_bugbin = 0;
      print FILEOUT "$_";
     }

    # print BUGBIN
    elsif (/^\s*BUGBIN/) {
      print "bugbin\n"; 
      $bugbin = $_;
      $print_bugbin = 1;
      
      # print last TEST if not done
      if ($print_test) {
       print FILEOUT "\n$test";
      }
      $print_test = 0;
      print FILEOUT "$_";
    }

    # record TEST
    elsif (/^\s*TEST/) {
      print "test\n";
      $test = $_;
      $print_test = 1;
    }
  }
}
