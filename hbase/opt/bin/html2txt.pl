#!/usr/pkg/bin/perl -s

# Written by Robert Stockton (rgs@cs.cmu.edu).
# Known to be imcomplete.  Expected to be maintainable by author, and somewhat
# accessible to others.

# Notes:
#   * Doesn't deal with tabs as well as it might
#   * Doesn't cover the entire HTML language yet
#     * Anchors are simply ignored
#     * Will not understand forms at all.

$BCap = 1 unless defined($Bcap);
$Bbracket = "" unless defined($Bbracket);
$ICap = 1 unless defined($Icap);
$Ibracket = "" unless defined($Ibracket);
$linelen = 78 unless defined($linelen);
$H1cap = 1 unless defined($H1cap);
$H1under = 1 unless defined ($H1under);
$H2cap = 0 unless defined ($H2cap);
$H2under = 1 unless defined ($H2under);
$margin = 0 unless defined ($margin);

$tag = "TEXT";
$col = 0;
$dirty = 0;

$text = "";
$blank = 1;
$prefix = "";
while (<>) {
    chop;
    s/^[ \t]+// unless $ispre;
    $rest = $_;
    while ($rest ne "") {
	($_, $rest) = split(/</, $rest, 2);
	if ($_ ne "") {
	    if ($tag =~ /^(I|EM|CITE|VAR|DFN)$/) {
		tr/a-z/A-Z/ if $ICap;
		$_ = $Ibracket . $_ . $Ibracket;
		&AddText(*text, $_);
	    } elsif ($tag =~ /^(B|STRONG)$/) {
		tr/a-z/A-Z/ if $BCap;
		$_ = $Bbracket . $_ . $Bbracket;
		&AddText(*text, $_);
	    } elsif ($tag =~ /^A$/) {
		&AddText(*text, $_);
	    } elsif ($tag =~ /^TITLE$/) {
	    } else {
		&AddText(*text, $_);
	    }
	}

	if ($rest ne "") {
	    while (index($rest, ">") == -1) {
		chop($_ = <>);
		$rest .= " " . $_;
	    }
	    ($delim, $rest) = split(/>/, $rest, 2);

	    $delim =~ tr/a-z/A-Z/;
	    $delim =~ s/ .*//;
	    
	    if ($delim =~ /^\/([A-Z0-9]+)/) {
		if ($1 ne $tag) {
		    print(STDERR "Tag mismatch:  $tag vs. $delim\n");
		}
		if (!($tag =~ /^(B|I|EM|CITE|VAR|STRONG|DFN|TT|CODE|SAMP|KBD|HEAD|BODY|A|UNKNOWN)$/)) {
		    $blank = &break($tag, *text, $blank, 1);
		}
		if ($tag =~ /^(MENU|UL|OL|DL)$/) {
		    $margin = pop(@margins);
		    $counter = pop(@counters);
		} elsif ($tag =~ /^(BLOCKQUOTE)$/) {
		    $margin = pop(@margins);
		} elsif ($tag =~ /^(PRE)$/) {
		    $ispre--;
		}
		$tag = pop(@tags);
	    } elsif ($delim =~ /^!/) { # comment
		$blank = &break($tag, *text, $blank, 1);
	    } elsif ($delim =~ /^P$/) {
		$blank = &break($tag, *text, $blank, 1);
	    } elsif ($delim =~ /^BR$/) {
		if ($ispre) {
		    &AddEol(*text);
		} elsif ($text eq "") {
		    print("\n");
		    $blank = 1;
		} else {
		    $blank = &break($tag, *text, $blank, 0);
		};
	    } elsif ($delim =~ /^HR$/) {
		$blank = &break($tag, *text, $blank, 1);
		$blank = &break("HR", *text, $blank, 1);
	    } elsif ($delim =~ /^IMG$/) {
		$blank = &break($tag, *text, $blank, 1);
                $ispre++;
                $text = "    *** INLINE IMAGE IGNORED ***";
		$blank = &break($tag, *text, $blank, 1);
		$ispre--;
	    } elsif ($delim =~ /^LI$/) {
		$rest =~ s/^[ \t]+//;
		$blank = &break($tag, *text, $blank, 0);
		$tag eq "UL" && ($prefix = "* ");
		$tag eq "MENU" && ($prefix = "* ");
		$tag eq "OL" && ($prefix = $counter++ . ". ");
	    } elsif ($delim =~ /^DT$/) {
		$rest =~ s/^[ \t]+//;
		$blank = &break($tag, *text, $blank, 0);
		$margin = $margins[$#margins];
	    } elsif ($delim =~ /^DD$/) {
		$rest =~ s/^[ \t]+//;
		$blank = &break($tag, *text, $blank, 0);
		$margin = $margins[$#margins] + 4;
	    } else {
		if (!($delim =~ /^(B|I|EM|CITE|VAR|STRONG|DFN|TT|CODE|SAMP|KBD|HEAD|BODY|A)$/)) {
		    $blank = &break($tag, *text, $blank, 1);
		}
		if ($delim =~ /^(MENU|UL|OL|DL)$/) {
		    push(@margins, $margin);
		    push(@counters, $counter);
		    $margin += 4;
		    $counter = 1;
		} elsif ($delim =~ /^(BLOCKQUOTE)$/) {
		    push(@margins, $margin);
		    $margin += 4;
		} elsif ($delim =~ /^(PRE)$/) {
		    $ispre++;
		}
		push(@tags, $tag);
		$tag = $delim;
	    }
	}
    }
    &AddEol(*text);
}
&break($tag, *text, $blank, 0);

sub break {
    local($tag, *text, $blank, $forceblank) = @_;

    $ispre && return &PREbreak(*text, $blank);
    $text =~ s/^[ \t]+//;

    $tag =~ /^(TEXT|HEAD|BODY|A|UNKNOWN|B|I|EM|CITE|VAR|STRONG|DFN|TT|CODE|SAMP|KBD|UL|DL|MENU|OL|BLOCKQUOTE)$/ &&
	return &TEXTbreak(*text, $blank, $forceblank);
    $tag eq "H1" && return &H1break(*text, $blank);
    $tag eq "H2" && return &H2break(*text, $blank);
    $tag =~ /^(H[3-6])$/ &&
	return &H3break(*text, $blank);
    $tag eq "HR" && return &HRbreak(*text, $blank);
    $blank;
}

sub AddText {
    local(*text, $_) = @_;

    s/\&gt;/>/ig;
    s/\&lt;/</ig;
    s/\&amp;/&/ig;

    $text .= $_;
}

sub AddEol {
    local(*text) = @_;

    if ($ispre) {
	$text .= "\n";
    } else {
	$space = "";
	$text =~ s/[ \t]+$//;
	if ($text ne "") {
	    $text =~ /[^-.:?!]$/ && ($space = " ");
	    $text =~ /[.:?!]$/ && ($space = "  ");
	}
	$text .= $space;
    }
}

sub TEXTbreak {
    local(*text, $blank, $wantblank) = @_;

    $text =~ s/[ \t]+$//;
    if ($text eq "") {
	$blank++ || print("\n") if $wantblank;
	return $blank;
    }

    while (length($text) > $linelen - $margin - 2) {
	$offset = rindex($text, ' ', $linelen - $margin - 2);
	$offset = index($text, ' ') if $offset == -1;
	$offset = length($text) if $offset == -1;

	($sub = substr($text, 0, $offset)) =~ s/ +$//;
	print(' ' x ($margin-length($prefix)) . "$prefix$sub\n");
	$prefix = "";
	substr($text, 0, $offset) = "";
	$text =~ s/^ +//;
    }
    print(' ' x ($margin-length($prefix)) . "$prefix$text\n");
    $prefix = "";
    $text = "";

    print("\n") if $wantblank;
    $wantblank;
}

sub PREbreak {
    local(*text, $blank) = @_;

    ($text eq "") && return $blank;
    print("\n") unless $blank;

    $text =~ s/^\n+//;
    $text =~ s/\n+$//;

    @lines = split(/\n/, $text);
    foreach $line (@lines) {
	print(' ' x ($margin-length($prefix)) . "$prefix$line\n");
	$prefix = "";
    }
    $text = "";
    print("\n");
    return 1;
}

sub HRbreak {
    local(*text, $blank) = @_;

    print("\n") unless $blank;
    print('-' x $linelen . "\n\n");
    return 1;
}

sub H1break {
    local(*text, $blank) = @_;

    print("\n") unless $blank;
    $text =~ s/^[ \t]+//;
    $text =~ tr/a-z/A-Z/ if $H1cap;
    
    $maxlen = 0;
    while (length($text) > $linelen - 22) {
	$offset = rindex($text, ' ', $linelen - 22);
	$offset = index($text, ' ') if $offset == -1;
	($sub = substr($text, 0, $offset)) =~ s/ +$//;
	$indent = ($linelen - length($sub))/2;
	$maxlen = length($sub) if length($sub) > $maxlen;
	print(' ' x $indent . $sub . "\n");
	substr($text, 0, $offset) = "";
	$text =~ s/^ +//;
    }
    $maxlen = length($text) if length($text) > $maxlen;
    $indent = ($linelen - length($text))/2;
    print(' ' x $indent . "$text\n");
    if ($H1under) {
	$indent = ($linelen - $maxlen)/2;
	print(' ' x $indent . '=' x $maxlen . "\n");
    }
    print("\n");
    $text = "";
    return 1;
}

sub H2break {
    local(*text, $blank) = @_;

    print("\n") unless $blank;
    $text =~ s/^[ \t]+//;
    $text =~ tr/a-z/A-Z/ if $H2cap;
    
    $maxlen = 0;
    while (length($text) > $linelen - 22) {
	$offset = rindex($text, ' ', $linelen - 22);
	$offset = index($text, ' ') if $offset == -1;
	($sub = substr($text, 0, $offset)) =~ s/ +$//;
	$maxlen = length($sub) if length($sub) > $maxlen;
	print($sub . "\n");
	substr($text, 0, $offset) = "";
	$text =~ s/^[ \t]+//;
    }
    $maxlen = length($text) if length($text) > $maxlen;
    print("$text\n");
    $text = "";
    if ($H2under) {
	print('-' x $maxlen . "\n");
	return 0;
    } else {
	print("\n");
	return 1;
    }
}

sub H3break {
    local(*text, $blank) = @_;
    print("\n") unless $blank;

    push(@margins, $margin);
    $margin = 0;
    $blank = &TEXTbreak(*text, $blank, 1);
    $margin = pop(@margins);
    return $blank;
}
