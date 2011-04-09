#!/usr/bin/perl -w
# Copyright © 2006-2010 Jamie Zawinski <jwz@jwz.org>
#
# Permission to use, copy, modify, distribute, and sell this software and its
# documentation for any purpose is hereby granted without fee, provided that
# the above copyright notice appear in all copies and that both that
# copyright notice and this permission notice appear in supporting
# documentation.  No representations are made about the suitability of this
# software for any purpose.  It is provided "as is" without express or 
# implied warranty.
#
# Parses the database files in the iPhone backup files and saves the
# output in multiple files: one file per phone number messages have
# been sent to or received from, per month.
# E.g., ~/Documents/SMS Backup/YYYY-MM.NNNNNNNNNN.txt
#
# It only changes files when needed, and will never delete a message
# from one of the files (so if your phone's messages database has shrunk,
# your old archived messages won't be lost.)
#
# Created: 21-Jun-2006 for PalmOS; rewritten for iPhone on 6-Mar-2010.

require 5;
use diagnostics;
use strict;
use POSIX;
use DBI;

my $progname = $0; $progname =~ s@.*/@@g;
my $version = q{ $Revision: 1.4 $ }; $version =~ s/^[^0-9]+([0-9.]+).*$/$1/;

my $verbose = 0;
my $debug_p = 0;

my $iphone_backup_dir = ($ENV{HOME} . 
                         "/Library/Application Support/MobileSync/Backup/");

my $addressbook_db = ($ENV{HOME} .
                      "/Library/Application Support/AddressBook/" .
                      "AddressBook-v22.abcddb");

# This magic number is the hash of the iPhone SMS database name: We could
# just "use Digest::SHA1" and do sha1_hex("HomeDomain-Library/SMS/sms.db")
# but the hash doesn't change...
#
my $sms_db_name = "3d0d7e5fb2ce288813306e4d4636395e047a3d28";

# For future reference, other hashed databases names include:
#
#   HomeDomain-Library/CallHistory/call_history.db
#   HomeDomain-Library/AddressBook/AddressBook.sqlitedb
#   HomeDomain-Library/AddressBook/AddressBookImages.sqlitedb
#   HomeDomain-Library/Notes/notes.db
#   HomeDomain-Library/Voicemail/voicemail.db
#   HomeDomain-Library/Calendar/Calendar.sqlitedb


my $output_dir   = "$ENV{HOME}/Documents/SMS Backup";

$ENV{PATH} = "$ENV{HOME}/bin:$ENV{PATH}";   # for cron, bleh.


my %phone_number_map;

# Loads the address-book DB and populates the %phone_number_map with
# a map of phone numbers to real names.
#
# It would probably make more sense to read the number->name map out
# of the iPhone's copy of the address-book DB rather than the host Mac's
# address book, but I had a hard time figuring out how to do that, so
# fuck it.  Close enough.
#
sub load_addressbook() {
  my %attr;

  print STDERR "opening $addressbook_db...\n" if ($verbose > 2);

  my $dbh = DBI->connect("dbi:SQLite:dbname=" . $addressbook_db, '', '',
                         \%attr);

  my $sth = $dbh->prepare ("SELECT " .
                           " pn.zfullnumber, " .
                           " pn.zlabel, " .
                           " r.zfirstname, " .
                           " r.zlastname, " .
                           " r.zorganization " .
                           "FROM ZABCDPHONENUMBER pn ".
                           "JOIN ZABCDRECORD r " .
                           "ON pn.zowner = r.z_pk " .
                           "ORDER BY r.zfirstname, r.zlastname");
  $sth->execute();

  while (my $h = $sth->fetchrow_hashref()) {
    my $fn  = $h->{ZFIRSTNAME};
    my $ln  = $h->{ZLASTNAME};
    my $org = $h->{ZORGANIZATION};
    my $name = ($fn && $ln ? "$fn $ln" : $fn || $ln ? ($fn || $ln) :
                $org ? $org : '???');
    my $phone = $h->{ZFULLNUMBER};
    $phone =~ s/[-.()_\s]//gs           # "(415) 555-1212"   ==>  "4155551212".
      unless ($phone =~ m/[@]/s);
    $phone =~ s/^\+?1(\d{10})$/$1/s;    # "+1 415 555 1212"  ==>  "4155551212".

    print STDERR "$progname: addr: $phone\t$name\n" if ($verbose > 4);
    $phone_number_map{$phone} = $name;
  }
}


sub sms_backup_1($) {
  my ($db_file) = @_;

  print STDERR "opening $db_file...\n" if ($verbose > 2);

  my %attr;
  my $dbh = DBI->connect("dbi:SQLite:dbname=" . $db_file, '', '', \%attr);

  my $sth = $dbh->prepare("SELECT * FROM message");
  $sth->execute();

  my %output;

  while (my $h = $sth->fetchrow_hashref()) {
    my $flags = $h->{flags};
    my $date  = $h->{date};
    my $addr  = $h->{address};
    my $text  = $h->{text};
    my $subj  = $h->{subject};
    my $head  = $h->{headers};
    my $recip = $h->{recipients};

    if ($subj && $text) { $text = "$subj\n$text"; }
    elsif ($subj && !$text) { $text = $subj; }
    elsif (!defined($text)) { $text = ''; }

    # Sometimes the 'address' phone number is in an XML blob in 'recipients',
    # for no reason that I can discern.
    #
    if (!$addr && $recip) {
      $addr = '';
      $recip =~ s@<string>([^<>]+)</string>@{
        $addr .= ($addr ? ", " : "") . $1;
        ''}@gsexi;
    }

    $addr =~ s/[-.()_\s]//gs           # "(415) 555-1212"   ==>  "4155551212".
      unless $addr =~ m/[@]/s;
    $addr =~ s/^\+?1(\d{10})$/$1/s;    # "+1 415 555 1212"  ==>  "4155551212".

    $text =~ s/(^\n+|\n+$)//gs;
    $text =~ s/\n/\n\t/gs;             # indent continuation lines

    my $type = (($flags & 1) ? '>' : '<');
    my $timestr = strftime ("%a %b %d %I:%m %p", localtime($date));

    my $name = $phone_number_map{$addr} || '';
    my $line = "$type $timestr $addr $name \t$text\n";

    my $month_str = strftime ("%Y-%m", localtime ($date));
    my $filename = "$output_dir/$month_str.$addr.txt";

    print STDERR "$progname: got: $line\n" if ($verbose > 5);

    $output{"$filename"} = ($output{"$filename"} || '') . $line;
  }

  foreach my $file (sort keys (%output)) {
    my $body = $output{$file};
    write_changes ($file, $body);
  }
}


sub sms_backup() {

  # Iterate over each subdirectory in the backup dir, and save SMS messages
  # from every database in those dirs.

  load_addressbook();

  local *DIR;
  opendir (DIR, $iphone_backup_dir) || error ("$iphone_backup_dir: $!");
  my @files = sort readdir(DIR);
  closedir DIR;
  foreach my $d (@files) {
    next if ($d =~ m/^\./);
    my $f = "$iphone_backup_dir$d/$sms_db_name.mddata";  # iPhone 3.x name
    sms_backup_1 ($f) if (-f $f);
    $f = "$iphone_backup_dir$d/$sms_db_name";            # iPhone 4.x name
    sms_backup_1 ($f) if (-f $f);
  }
}



# Ok, it's not really CSV.  Each line in the file begins with > or <
# except that lines beginning with TAB are continuation lines.
#
sub csv_split($) {
  my ($body) = @_;

  $body =~ s/^([<>] )/\001$1/gm;
  my @lines = split (/\001/, $body);
  shift @lines; # lose first blank line
  return @lines;
}


sub write_changes($$) {
  my ($file, $nbody) = @_;

  my $obody = '';
  my %olines;
  my $count = 0;
  my $count2 = 0;
  local *IN;
  if (open (IN, "<$file")) {
    while (<IN>) { $obody .= $_; }
    close IN;
    foreach my $line (csv_split ($obody)) {
      $count++;
      $olines{$line} = 1;
    }
  }

  my @nlines = ();
  foreach my $line (csv_split ($nbody)) {
    if (! $olines{$line}) {
      $count++;
      $count2++;
      push @nlines, $line;
    }
  }

  my ($year, $mon) = ($file =~ m@/(\d\d\d\d)-(\d\d)\.[^/]+$@);
  my @now = localtime(time);
  my $now = (($now[5] + 1900) * 10000 + $now[4]);
  my $then = ($year * 10000 + $mon);
  my $old_p = ($now - $then) > 2;

  # NOTE: As a sanity-check, we refuse to write out any SMS messages
  # that are more than a few months old.  This was necessary back when
  # this was a PalmOS backup script, because the Palm SMS database
  # would often get corrupted and trash the dates to be random
  # years-old numbers.  If you don't want this check (for example,
  # when running this script the first time) then uncomment the
  # following line.  I don't know if this sanity-check is still
  # necessary here in the iPhone world, but it seems cautious anyway.
  #
  #   $old_p = 0;


  if ($#nlines < 0) {
    if ($verbose > 1) {
      $file =~ s@^.*/@@;
      print STDERR "$progname: $file: unchanged\n";
    }

  } else {
    local *OUT;

    if (! $debug_p || $old_p) {
      open (OUT, ">$file") || error ("$file: $!");
      print OUT $obody;
    }

    foreach (@nlines) {
      if ($verbose > 2 && $obody ne '') {
        print STDERR "+ $_";
      }
      print OUT $_ unless ($debug_p || $old_p);
    }
    close OUT unless ($debug_p || $old_p);

    if ($verbose) {
      $file =~ s@^.*/@@;
      print STDERR ("$progname: " .
                    (($debug_p || $old_p) ? "didn't write" : "wrote") .
                    ($old_p ? " old file" : "") .
                    " $file ($count2 of $count lines)\n");
    }
  }
}


sub error($) {
  my ($err) = @_;
  print STDERR "$progname: $err\n";
  exit 1;
}

sub usage() {
  print STDERR "usage: $progname [--verbose] [--debug]\n";
  exit 1;
}

sub main() {
  while ($#ARGV >= 0) {
    $_ = shift @ARGV;
    if ($_ eq "--verbose") { $verbose++; }
    elsif (m/^-v+$/) { $verbose += length($_)-1; }
    elsif ($_ eq "--debug") { $debug_p++; }
    elsif (m/^-./) { usage; }
    else { usage; }
  }

  $verbose += 3 if ($debug_p);

  sms_backup();
}

main();
exit 0;
