#!/usr/bin/perl

# CATegorisation Tools Archive, version 1.1.
# By Niklas Zechner, Språkbanken, 2021.

package Catta;
use FindBin;
use feature(say);
use Cwd;
use utf8;
binmode(STDOUT, ":utf8");
use open ':std', ':encoding(UTF-8)';
use Time::HiRes 'time';
use Exporter 'import';
our @EXPORT=qw(
cwd here in to moreto out filesin foldersin recfilesin barefile enoughfiles
lb sum mean max min sd ssd corr erf cnd randset
blackspace whatsin tic toc check faint gasp
dedup ynhash sortbyp decsortbyp clipcopy clippaste gaussblur
makefeats makefeatsover makefeatsovert getfeattype getfeats loadprof clearprof whois acctest
comp bae baegraph baesim rank rankgraph importplain importSB classify
trainbyindex trainbyrand
trait tget tgetv twho tset tonly tdel allprofs alltraits alltraitsb alltraitsv allvals
$DIST_TYPE $RANK_TYPE $METHOD %FEAT_TYPES $FEAT_TYPE $FEAT_MAX $TARGET $PROFS
@FEAT_KEY @FEAT_WEIGHT $FEAT_COUNT @FEAT_USED $TRAIN $GOTFEATS $GOTBAE $VERBOSE
@RANK_ALL $POSI $RANK_APRI
$GAUSS_DIST $GAUSS_SD $GAUSS_RES $GAUSS_MARGIN $GAUSS_MIN $GAUSS_MAX $GAUSS_TOT
@gaussx @gaussy @gaussxy
@header @feat @p @q
);

################################################################
# Some constants and default settings
################################################################

$path=$FindBin::Bin;
$,="\n";
$H="\#"; # because some editors get confused by escaped hash signs

$DIST_TYPE='dist_sum';
$RANK_TYPE='rank_wsum';
%FEAT_TYPES=ynhash('word','pos','cgram');
$FEAT_TYPE='word';
$FEAT_MAX=10;
$PROFS=0;
$NERVOUS=1;

package Blank {
	use overload
	bool     => sub { 1 },
	q{""}    => sub { "" },
	fallback => 1;
}
$BLANK = bless({}, Blank::);

################################################################
# General subs for debugging, benchmarking etc.
################################################################

sub blackspace{ # Makes whitespace less white.
	my @x=@_;
	for(@x){
		s/\n/⬇︎/g;
		s/\t/➡︎/g;
		s/\r/⬅︎/g;
		s/ /◇/g;
	}
	if(wantarray){return @x}
	else{return join('●',@x)}
}

sub whatsin{ # Gives a basic idea of what is in an array.
	if(@_){
		my @a=blackspace(@_);
		if(scalar(@a)==1){
			if($a[0] eq ''){say "Empty string"}
			else{say "Scalar: $a[0]"}
		}else{
			say "Array size ".scalar(@a);
			if(scalar(@a)>5){say "First elements:"}
			else{say "Elements:"}
			if(scalar(@a)>0){say "\t".blackspace($a[0])}
			if(scalar(@a)>1){say "\t".blackspace($a[1])}
			if(scalar(@a)>2){say "\t".blackspace($a[2])}
			if(scalar(@a)==4){say "\t".blackspace($a[3])}
			if(scalar(@a)==5){say "\t".blackspace($a[3]); say "\t",blackspace($a[4])}
			if(scalar(@a)>5){say "Last elements:\n\t".blackspace($a[scalar(@a)-2])."\n\t".blackspace($a[scalar(@a)-1])}
		}}else{
			say 'Not defined';
		}
}

sub tic{ # "Starts" the timer.
	$TICTIME=time;
}

sub toc{ # "Stops" the timer.
	say time-$TICTIME.' seconds';
}

sub check{ # Prints the argument(s) and the line number it was called from,
	# but only the first three times it is called from the same line.
	# Useful for debugging; you can print things in a loop
	# without clogging the terminal output.
	
	my ($package, $filename, $line) = caller;
	if($CHECKCOUNT[$line] < 3){
		print "Line $line: ";
		whatsin(@_);
		$CHECKCOUNT[$line]++;
	}
}

sub faint{ # Like die(), but more kid-friendly.
	my $i,$package,$filename,$line;
	do{
		($package,$filename,$line)=caller($i++);
		##say ($package, $filename, $line);
	}while($filename=~/Catta.pm$/);
	if($_[0]){say "Error: $_[0] at line $line of ".barefile($filename).'.'; exit}
	say "Error at line $line of ".barefile($filename).'.'; exit;
}

sub gasp{ # Prints a warning, but remembers not to print it again.
	my $i,$package,$filename,$line;
	do{
		($package,$filename,$line)=caller($i++);
	}while($filename=~/Catta.pm$/);
	
	if($NERVOUS and !$didgasp{$line}){
		
		if($_[0]){say "Warning: $_[0] at line $line of ".barefile($filename).'.'}
		else{say "Warning at line $line of ".barefile($filename).'.'}
		$didgasp{$line}=1;
	}
	
}

################################################################
# File handling routines
################################################################

sub here{ # Sets the current working directory to the same as where the executable is.
	chdir($path);
}

sub in{ # Reads a (text) file.
	# By default, looks for the file in CWD.
	# We can use here() to set CWD to where the executable is.
	# In list context, returns the list of lines, without line breaks.
	# In scalar context, returns the whole text.
	# In void context, sets $_.
	# Ignores lines that start with a #, and empty lines at the end.
	# If given a second argument, reads that many lines (not counting comment lines).
	
	local $_=$_[0] || $_;
	open my $fh, "<:encoding(UTF-8)",$_ or faint "Could not open $_";
	my $i=0;
	my @lines;
	while(<$fh>){
		if(/^$H/){next}
			if($_[1] eq $i++){last} # using "eq" so that undef is not the same as zero
		chomp;
		push @lines,$_;
	}
	close $fh;
	while(@lines and !$lines[@lines-1]){pop @lines} # remove empty lines at end
	
	if(defined(wantarray)){
		if(wantarray){return @lines}
		else{return join "\n",@lines}
	}else{
		$_=join "\n",@lines;
	}
}

sub header{ # Looks for "@" headers in a text file.
	local $_=$_[0] || $_;
	open my $fh, "<:encoding(UTF-8)",$_ or faint "Could not open $_";
	my %out;
	while(<$fh>){
		chomp;
		if(/^\@(\S+)\s*(.*)$/){$out{$1}=$2}
		else{last}
	}
	return %out;
}

sub to{ # Sets the output file, and deletes its contents.
	$TO=$_[0] || $_;
	$TO=~/^((.*)\/)?[^\/\.][^\/]*$/ or die "Weird filename $TO";
	##mkdir $2;
	# the Perl mkdir seems to have trouble with spaces
	if($2){system("mkdir -p \"$2\"")}
	open $fh, ">:encoding(UTF-8)",$TO or die "Could not open $TO";
	print $fh '';
	close $fh;
}

sub moreto{ # Sets the output file, but does not delete its contents.
	$TO=$_[0] || $_;
}

sub out{ # Prints the given text to the pre-set output file,
	# appending it with a newline at the end.
	# Lists are appended with newlines after each item.
	# If no text is given, defaults to $_.
	
	my ($package, $filename, $line) = caller;
	open my $fh, ">>:encoding(UTF-8)",$TO or die "could not open $TO from line $line";
	if($_[0]){print $fh join("\n",@_)}
	else{print $fh $_}
	print $fh "\n";
	close $fh;

}

sub filesin{ # Takes a folder (or more), returns an array of the files there.
	my @files;
	while(my $dir=shift){
		$dir=~s/\/$//;
		opendir(dir,$dir);
		for(readdir(dir)){unless(/^\./){
			$x="$dir/$_";
			unless(-d $x){push @files, $x};
		}}
		close dir;
	}
	return @files;
}

sub foldersin{ # Takes a folder (or more), returns an array of the folders there.
	my @files;
	while(my $dir=shift){
		opendir(dir,$dir);
		for(readdir(dir)){unless(/^\./){
			$x="$dir/$_";
			if(-d $x){push @files, $x};
		}}
		close dir;
	}
	return @files;
}

sub recfilesin{ # Takes a folder (or more), returns an array of the files there, including subfolders.
	# If given files, returns them too.
	my @files;
	for(@_){unless(/^\.|^$/){
		if(-d $_){
			opendir(dir,$_);
			for $x(readdir(dir)){unless($x=~/^\./){
				push @files, recfilesin("$_/$x");
			}}
		}else{
			push @files, $_;
		}
	}}
	return @files;
}

sub barefile{ # Takes a path, returns just the filename.
	if($_[0]){$_=$_[0]}
	/.*\/(.*)$/;
	return $1;
}

sub enoughfiles{ # Checks if folders have a given number of files in them,
	# and returns those folders that had at least that number.
	# Args: number, folders.
	
	my $lim=shift;
	my @out;
	for(@_){
		if(scalar(recfilesin($_))>=$lim){push @out,$_}
	}
	return @out;
}

################################################################
# General mathematical functions
################################################################

sub lb{ # The binary logarithm. The best logarithm.
	return log(shift)/log(2);
}

sub lg{ # The decimal logarithm. Sometimes you have to.
	return log(shift)/log(10);
}

sub sum{
	my $x;
	for(@_){$x+=$_}
	return $x;
}

sub mean{
	my $x;
	for(@_){$x+=$_}
	return $x/@_;
}

sub max{
	my $x=shift;
	for(@_){if($_>$x){$x=$_}}
	return $x;
}

sub min{
	my $x=shift;
	for(@_){if($_<$x){$x=$_}}
	return $x;
}

sub sd{ # Standard deviation, for the entire population.
	my $sum;
	my $avg=mean(@_);
	for(@_){$sum+=($_-$avg)**2}
	return sqrt($sum/scalar(@_));
}

sub ssd{ # Standard deviation, for a sample of the population.
	my($n,$sum,$sumOfSquares);
	for(@_){
		$sum += $_;
		$n++;
		$sumOfSquares += $_**2;
	}
	return sqrt(($sumOfSquares - (($sum * $sum) / $n)) / ($n - 1));
}

sub corr{ # Correlation coefficient of @a and @b (no arguments).
	my $avga=mean(@a);
	my $avgb=mean(@b);
	my $sum;
	for(0..scalar(@a)-1){
		$sum+=($a[$_]-$avga)*($b[$_]-$avgb);
	}
	my $sa=stdev(@a); my $sb=stdev(@b);
	unless($sa*$sb){return 0}
	
	##$temp=$sum/scalar(@a)/$sa/$sb;
	##if($temp eq 'nan'){return -2} # not a very pretty solution
	return $sum/scalar(@a)/$sa/$sb;
}

sub erf{ # The error function.
	$_=1/(1+0.3275911*abs($_[0]));
	$_=1-(((((1.061405429*$_-1.453152027)*$_)+1.421413741)*$_-0.284496736)*$_+0.254829592)*$_*exp(-$_[0]*$_[0]);
	if($_[0]<0){return -$_}else{return $_}
}

sub cnd{ # Cumulative normal distribution.
	# Arguments: x, sd (default: 1), median (default: 0)
	my ($x,$sd,$m)=@_;
	unless($sd){$sd=1}
	return 0.5+0.5*erf(($x-$m)/$sd/sqrt(2));
}

sub randset{ # Returns a random subset of the given list, in random order.
	# Args: Size of subset, set.
	my $c=shift;
	my @in=@_;
	my @out;
	if($c>@in){faint "Can't pick $c out of ".scalar(@in)}
	for(0..$c-1){
		push @out,splice @in,int(rand(@in)),1;
	}
	return @out;
}

################################################################
# Various utility subs
################################################################

sub dedup{ # Takes a list, returns it without duplicates.
	my @out; my %seen;
	for(@_){unless($seen{$_}){
		push @out, $_;
		$seen{$_}=1;
	}}
	return @out;
}

sub ynhash{ # Returns a hash which is 1 for all items present in a given list.
    %a=();
    for(@_){$a{$_}=1}
    return %a;
}

sub sortbyp{ # Sorts @p and @q by @p.
    my @i=sort{$p[$a]<=>$p[$b]} 0..$#p;
    @q=@q[@i];
    @p=@p[@i];
}

sub decsortbyp{ # Sorts @p and @q by @p, in descending order.
    my @i=sort{$p[$b]<=>$p[$a]} 0..$#p;
    @q=@q[@i];
    @p=@p[@i];
}

sub clipcopy{ # Copies input to the clipboard.
	open (clip, "|pbcopy");
	print clip join "\n",@_;
	close clip;
}

sub clippaste{ # Gets contents of the clipboard.
	open (clip, "pbpaste|");
	my $text;
	for(<clip>){$text.=$_}
	close clip;
	return $text;
}

sub gaussblur{ # Creates a blurred graph based on an existing point set.
	# There are two main types – function blur or distribution blur.
	# Function blur acts on a function, smoothing it to get rid of noise.
	# It takes a set of x/y coordinates as input.
	# Distribution blur takes a set of points and creates a function
	# describing the density of points along the axis.
	# It acts on a single axis, but y values can be given as weights.
	
	# Parameters to set:
	# $GAUSS_DIST: 0 for function blur, >0 for distribution blur.
	# Set to 2 for a bounded distribution, that is, if we assume that
	# values outside the given range are zero. For unbounded distributions,
	# you are likely to see a drop near the edges, which may or may not
	# be justified. Function blur is assumed to be bounded.
	# @x: the x-coordinates of the points
	# @y: the y-coordinates (function) or weights (distribution)
	# (so @y is mandatory for function, optional for distribution)
	# $GAUSS_SD: Standard deviation for the gaussian; default is 1.
	# Higher number means more smoothing. Somewhat simplified,
	# peaks and dips with a lower width will be smoothed away.
	# $GAUSS_RES: Resolution, i.e. number of points in output -1.
	# By default (or if set to 0), function blur uses the
	# x-coords of the input points, distribution blur uses 100.
	# $GAUSS_MARGIN: X-space to add outside the points; default is 0.
	# Instead of $GAUSS_MARGIN we can set the min and max x
	# with $GAUSS_MIN and $GAUSS_MAX.
	# For function blur, using margins means extrapolating the curve
	# outside the input range, which is typically not what we want.
	# For $GAUSS_DIST==1, margins are just a matter of how much is displayed.
	# For $GAUSS_DIST==2, margins affect the edge adjustment.
	# $GAUSS_TOT: Integral of the output graph, as a kind of normalisation.
	# For function blur, this is rarely something we want.
	# Leave out (or set to zero) to leave the curve unadjusted.
	# For distribution blur, by default this is equal to the number of points
	# (or the sum of weights, if weighted), so the curve shows the
	# approximate *number* of points (weights) per unit interval.
	# If set to 1, the curve shows the approximate *fraction* of
	# points per unit interval, with the whole thing summing to one.
	# For $GAUSS_DIST==1, this is calculated directly from input weights.
	# Otherwise, the final graph is adjusted by integrating numerically.
	
	# Output:
	# Always sets @gaussx and @gaussy containing the lists of coordinates,
	# and @gaussxy containing the tab-separated pairs.
	# In list context, also returns @gaussxy.
	# In scalar context, returns @gaussxy as a multiline string.
	# To copy directly to the clipboard, for use in Grapher, Nyxgraph or similar,
	# we can do clipcopy(gaussblur).
	
	
	# First we set some constants.
	my ($xdist, $gval, $gtot, $d, $min, $max, $gral, $i, $j);
	my @x=@x;
	my @y=@y;
	unless(@x){die "No \@x values for gaussblur"}
	my $GAUSS_RES=$GAUSS_RES;
	if($GAUSS_DIST and !$GAUSS_RES){$GAUSS_RES=100}
	my $GAUSS_SD=$GAUSS_SD || 1;
	my $c=0.3989422804/$GAUSS_SD; # that is, 1/(sd√(2π))
	if(defined($GAUSS_MIN)){$min=$GAUSS_MIN}
	else{$min=min(@x)-$GAUSS_MARGIN}
	if(defined($GAUSS_MAX)){$max=$GAUSS_MAX}
	else{$max=max(@x)+$GAUSS_MARGIN}
	
	if($GAUSS_RES){
		my $spread=(($max-$min) / $GAUSS_RES); # how far apart the points are
		@gaussx=();
		for(0..$GAUSS_RES){push @gaussx,$min+$_*$spread}
	}else{@gaussx=@x}
	@gaussy=(); # Yup, these are global.
	@gaussxy=();
	
	if(@y){
		unless(scalar(@y)==scalar(@x)){die "Coordinate lists differ in length"}
	}else{
		unless($GAUSS_DIST){die "No y values supplied for function blurring"}
		for(0..@x-1){$y[$_]=1} # this could be optimised
	}
	
	
	# Main calculation
	if($GAUSS_DIST){ # for the distribution case
		for $i(0..@gaussx-1){ # point affected
			
			for $j(0..@x-1){ # point affecting
				$xdist=$gaussx[$i]-$x[$j]; # x distance between points
				$gval=$c*exp(-$xdist**2/2/$GAUSS_SD**2); # height of standard gauss at that distance
				$gaussy[$i]+=$gval*$y[$j];
			}
			if($GAUSS_DIST==2){$gaussy[$i]=$gaussy[$i]/(1-cnd($min-$gaussx[$i],$GAUSS_SD)-cnd($gaussx[$i]-$max,$GAUSS_SD))}
			
		}
	}else{ # and for the function case
		for $i(0..@gaussx-1){ # point affected
			$gtot=0; # potential effect of all points in the interval; avoid dropping at curve ends
			for $j(0..@x-1){ # point affecting
				$xdist=$gaussx[$i]-$x[$j]; # x distance between points
				$gval=$c*exp(-$xdist**2/2/$GAUSS_SD**2); # height of standard gauss at that distance
				$gaussy[$i]+=$gval*$y[$j];
				$gtot+=$gval;
			}
			unless($gtot){ # dodging problems at very small numbers
				if($gaussy[$i]){faint "Strange blurring error"}
				else{next}
			}
			$gaussy[$i]=$gaussy[$i]/$gtot;
		}
	}
	
	# Adjust the height of the distribution, if desired.
	if($GAUSS_TOT){
		if($GAUSS_DIST==1){ # integral is just the sum of y values
			$d=$GAUSS_TOT/sum(@y);
		}else{ # integral needs to be explicitly approximated
		for(0..@gaussx-2){
			$gral+=($gaussx[$_+1]-$gaussx[$_])*($gaussy[$_]+$gaussy[$_+1]);
		}
		$gral=$gral/2; # this is the actual integral
		$d=$GAUSS_TOT/$gral;
		}
		for(0..@gaussy-1){
			$gaussy[$_]*=$d;
		}
	}
	
	# And output.
	for(0..@gaussx-1){
		push @gaussxy,$gaussx[$_]."\t".$gaussy[$_];
	}
	if(wantarray){
		return @gaussxy;
	}elsif(defined(wantarray)){
		return join "\n",@gaussxy;
	}
	
}

################################################################
# Internal subs used by classification tools
################################################################

sub dist_sum{ # Distance measure: Sum of differences.
	# Args: index0, index1
	
	if($_[0]>=$PROFS or $_[1]>=$PROFS){die "Profile not loaded"}
	my $dist;
	for(0..($FEAT_COUNT or $FEAT_MAX or die "Missing feature count")-1){
		$dist+=abs($feat[$_[0]][$_]-$feat[$_[1]][$_]);
	}
	return $dist;
}

sub dist_cos{ # Distance measure: Cosine distance,
	# i.e. 1 - (sum of products) / (geometric mean of sums of squares).
	# Args: index0, index1
	
	if($_[0]>=$PROFS or $_[1]>=$PROFS){die "Profile not loaded"}
	my $max=$FEAT_COUNT||$FEAT_MAX||die "Missing feature count";
	my $ab;
	unless($COSDSSQ[$_[0]]){for(0..$max-1){
		$COSDSSQ[$_[0]]+=$feat[$_[0]][$_]*$feat[$_[0]][$_];
	}}
	unless($COSDSSQ[$_[1]]){for(0..$max-1){
		$COSDSSQ[$_[1]]+=$feat[$_[1]][$_]*$feat[$_[1]][$_];
	}}
	for(0..$max-1){
		$ab+=$feat[$_[0]][$_]*$feat[$_[1]][$_];
	}
	return 1-$ab/sqrt($COSDSSQ[$_[0]]*$COSDSSQ[$_[1]]);
}

sub dist_mutual{ # Distance measure: Inverse sum of products.
	# Args: index0, index1
	
	if($_[0]>=$PROFS or $_[1]>=$PROFS){die "Profile not loaded"}
	my $sum;
	for(0..($FEAT_COUNT or $FEAT_MAX or die "Missing feature count")-1){
		$sum+=$feat[$_[0]][$_]*$feat[$_[1]][$_];
	}
	return 1/$sum;
}

sub rank_wsum{ # Ranking: Weighted sum of values.
	# Args: index
	if($_[0]>=$PROFS){faint "Profile not loaded"}
	my $sum;
	for(0..($FEAT_COUNT or $FEAT_MAX or faint "Missing feature count")-1){
		$sum+=$feat[$_[0]][$_]*$FEAT_WEIGHT[$_];
	}
	if($sum){return 1/$sum}
	else{return 'inf'}
}

################################################################
# Trait handling
################################################################

sub trait{ # Which value does this profile have for this trait?
	# Args: profile(s), trait(s)
	# If given multiple profiles, returns the list of their values.
	# If given multiple traits, returns the list of those values.
	# If given multiple profiles as well as multiple traits,
	# returns the list of the whole bunch, which is probably not what you want.
	
	my @out;
	if(defined($_[0])){@in=@_}else{@in=($_)}
	while($in[0]=~/^\d*$/){push @p,shift @in}
	unless(@p){return}
	while($in[0]=~/^[^\W\d]/){push @t,shift @in}
	if(@in){faint "Strange input for trait()"}
	if(@p>1 and @t>1){gasp "Tabular input for trait()"}
	for $p(@p){for $t(@t){push @out,$trait[$p]{$t}}}
	return @out;
	
	# $trait[profile]{trait} contains the trait value for this trait for this profile
	# (if there is no value, it contains the special $BLANK value)
	# $haver{trait}{profile} is 1 if this profile has this trait
	# $haverv{trait}{value}{profile} is 1 if this profile has this value for this trait
	# $tlist{trait} keeps track of all traits used without value
	# $tlistv{trait} keeps track of all traits used with value
	# Note that %tlist includes ONLY those used without value,
	# but they might overlap (if used both with and without value);
	# meanwhile, %haver contains BOTH valued and non-valued.
	# Trait values can include spaces. Trait labels might, but probably shouldn't.
}

sub tget{ # Which traits are in the given profile(s)?
	# If given multiple profiles, lists only the traits present in all of them.
	# If given no profiles, lists all existing traits.
	# Order is unspecified.
	
	my @out; my $first;
	if(defined($_[0])){
		$first=shift;
	}else{
		return keys %alltraits;
	}
	$first=shift @in;
	unless($first=~/^\d*$/){faint "tget() wanted profile number(s)"}
	unless($first<$PROFS){faint "Profile not loaded"}
	
OTW1:for $t(keys %{$trait[$first]}){
	for(@_){
		unless($_<$PROFS){faint "Profile not loaded"}
		unless(defined($trait[$_]{$t})){next OTW1}
	}
	push @out,$t;
}
	if(defined(wantarray)){return @out}
	else{say @out}
}

sub tgetv{ # Which trait values are in the given profile(s)?
	# Like tget(), but gives the trait and value on the form trait:value.
	# If given no profiles, lists all trait:value pairs used in any profile.
	my @in; my @out; my $first;
	
	if(defined($_[0])){
		$first=shift;
	}else{
		for(keys %haverv){
			push @out,keys %{$_};
		}
		return @out;
	}
	$first=shift @in;
	unless($first=~/^\d*$/){faint "trait_get() wanted profile number(s)"}
	unless($first<$PROFS){faint "Profile not loaded"}
	
OTW2:for $t(keys %{$trait[$first]}){
	for(@in){
		unless($_<$PROFS){faint "Profile $_ not loaded"}
		unless($trait[$_]{$t} eq $trait[$first]{$t}){next OTW2}
	}
	push @out,"$t:".$trait[$first]{$t};
}
	if(wantarray){return @out}
	elsif(defined(wantarray)){return scalar(@out)}
	else{say @out}
}

sub twho{ # Which/how many (of these) profiles have (all) these traits or trait values?
	# Args: optionally -1, optional list of profiles, list of traits/values
	# If no profiles are given, acts on all profiles.
	# An initial -1 flags to NOT act on all profiles, so
	# if there are no other numbers listed, no profiles will be matched.
	# If no traits are given, uses $_.
	# In list context, returns the list of profiles.
	# In scalar context, returns the number or profiles
	# (which as a bool will answer "did any of them have these traits").
	# In void context, prints the list.
	
	my @in; my @out; my @p; my @t; my $first, $dontactonall;
	
	if($_[0]==-1){
		shift;
		$dontactonall=1;
	}
	
	while($_[0]=~/^\d*$/){push @p,shift}
	unless(@p or $dontactonall){@p=(0..$PROFS-1)}
	while($_[0]=~/^\w/){push @t,shift}
	if(@_){faint "Strange input for twho()"}
	unless(@t){
		if(/^[^\W\d]/){@t=($_)}
		else{faint "Strange input for twho()"}
	}
	
	OT3:for $p(@p){
		unless($p<$PROFS){faint "Profile $p not loaded"}
		for(@t){
			if(/^([^:]+):(.*)$/){
				unless($trait[$p]{$1} eq $2){next OT3}
			}else{
				unless($trait[$p]{$_}){next OT3}
			}
		}
		push @out,$p;
	}
	
	if(wantarray){return sort{$a<=>$b} @out}
	elsif(defined(wantarray)){return scalar(@out)}
	else{say sort{$a<=>$b} @out}
}

sub tset{ # Give these profiles these traits or trait values.
	# Args: profiles, traits
	# Traits with values are on the form 'trait:value'.
	# If trait is given with no value, existing values are not removed.

	my @p; my @t;
	while($_[0]=~/^\d*$/){push @p,shift}
	while($_[0]=~/^[^\W\d]/){push @t,shift}
	if(@_){faint "Strange input for tset()"}
	
	for $t(@t){
		if($t=~/^(.*?):\s*(.*)$/){ # if this is a valued trait
			for $p(@p){
				delete $haverv{$1}{$trait[$p]{$1}}{$p};
				$trait[$p]{$1}=$2;
				$haver{$1}{$p}=1;
				$haverv{$1}{$2}{$p}=1;
			}
			$tlistv{$1}=1;
		}else{
			for $p(@p){unless($trait[$p]{$t}){
				$trait[$p]{$t}=$BLANK;
				$haver{$t}{$p}=1;
			}}
			$tlist{$1}=1;
		}
	}
}

sub tonly{ # Give these profiles these traits, remove them from all other profiles.
	my @p; my @t;
	while($_[0]=~/^\d*$/){push @p,shift}
	while($_[0]=~/^[^\W\d]/){push @t,shift}
	if(@_){faint "Strange input for tonly()"}
	
	for $t(@t){
		if($t=~/^(.*?):\s*(.*)$/){
			# Delete all...
			for(keys %{$haverv{$1}{$2}}){
				delete $trait[$_]{$1};
				delete $haver{$1}{$_};
			}
			delete $haverv{$1}{$2};
			# ...and add some back
			for $p(@p){
				$trait[$p]{$1}=$2;
				$haver{$1}{$p}=1;
				$haverv{$1}{$2}{$p}=1;
			}
			$tlistv{$1}=1;
		}else{
			for(keys %{$haver{$t}}){
				delete $haverv{$t}{$trait[$_]{$t}}{$_};
				delete $trait[$_]{$t};
			}
			delete $haver{$t};
			for $p(@p){
				$trait[$p]{$t}=$BLANK;
				$haver{$t}{$p}=1;
			}
			$tlist{$1}=1;
		}
	}
}

sub tdel{ # Remove from these profiles these traits or trait values.
	# Args: optional -1, profiles, traits.
	# If no profiles are given, acts on all profiles.
	# An initial -1 flags to NOT act on all profiles, so
	# if there are no other numbers listed, no profiles will be matched.
	# In the case where no profiles and no -1 are given,
	# trait(s) will also be removed from %tlist and %tlistv.
	
	my $dontactonall;
	if($_[0]==-1){
		shift;
		$dontactonall=1;
	}
	
	my @p; my @t;
	while($_[0]=~/^\d*$/){push @p,shift}
	while($_[0]=~/^\w/){push @t,shift}
	if(@_){faint "Strange input for tdel()"}
	unless(@p or $dontactonall){
		@p=(0..$PROFS-1); # could probably be optimised
		for $t(@t){unless($t=~/:/){delete $tlist{$t};delete $tlistv{$t}}}
		# If it's a valued trait, we don't know if it can be removed from
		# %tlistv, because it only applies to profiles with the same value.
		return;
	}
	
	for $t(@t){
		if($t=~/^(.*?):(.*)$/){ # if this is a valued trait
			for $p(@p){
				if($traitprof{$t}{$p}){ # only those that have the correct value
					delete $trait[$p]{$1};
					delete $haver{$1}{$p};
					delete $haver{$t}{$p};
				}
			}
		}else{
			for $p(@p){
				if($haver{$t}{$p}){ # don't really need to check here, but might be faster
					delete $haver{$t}{$p};
					delete $haver{$t.':'.$trait[$p]{$t}}{$p};
					delete $trait[$p]{$t};
				}
			}
		}
	}
	
}

sub allprofs{ # Returns all profiles, or (in scalar context) the number of profiles.
	if(wantarray){return 0..$PROFS-1}
	else{return $PROFS}
}

sub alltraits{ # Lists all the traits.
	# Optional argument 1 (or any true value) means to check explicitly.
	# Otherwise, currently unused traits may be included.
	# Note that some traits may be used both with and without value;
	# they will not be listed twice.
	
	if($_[0]){
		my @out;
		for(keys %tlist){if(%{$tlist{$_}}){push @out,$_}}
		for(keys %tlistv){if(%{$tlistv{$_}}){push @out,$_}}
		@out=dedup(@out);
		if(wantarray){return @out}
		else{return scalar(@out)}
	}
	
	if(wantarray){return dedup(keys %tlist, keys %tlistv)}
	else{return scalar(dedup(keys %tlist, keys %tlistv))}
}

sub alltraitsb{ # Lists all the non-valued traits (b for boolean, or perhaps binary).
	
	if($_[0]){
		my @out;
		for(keys %tlist){if(%{$tlist{$_}}){push @out,$_}}
		if(wantarray){return @out}
		else{return scalar(@out)}
	}
	
	if(wantarray){return keys %tlist}
	else{return scalar(keys %tlist)}
}

sub alltraitsv{ # Lists all the valued traits.
	
	if($_[0]){
		my @out;
		for(keys %tlistv){if(%{$tlistv{$_}}){push @out,$_}}
		if(wantarray){return @out}
		else{return scalar(@out)}
	}
	
	if(wantarray){return keys %tlistv}
	else{return scalar(keys %tlistv)}
}

sub allvals{ # Lists all the values the given trait(s) take.
	my @out;
	for(@_){push @out,keys %{$tlistv{$_}}}
	if(wantarray){return dedup(@out)}
	else{return scalar(dedup(@out))}
}

################################################################
# The actual classification tools
################################################################

sub makefeats{ # Combines the frequency lists for several profiles.
	# The goal is to find the overall most common features, to be used in the feature set.
	# Args: Files or folder(s) to look in. Checks all TXT files in all subfolders.
	# Outputs to <CWD>/feats_<FEATTYPE>.txt.
	
	if($VERBOSE){say "Creating feature list..."}
	my $filecount, $feats, $chsize; my %feats; my %chsize; my %sum;
	for(recfilesin(@_)){if(/\.txt$/){
		for(in){
			if(/^([^\t\@]*)\t([^\t]*)/){
				$sum{$1}+=$2;
			}elsif(/^\@feats\t([^\t]*)/){
				$feats{$1}=1;
			}elsif(/^\@chsize\t([^\t]*)/){
				$chsize{$1}=1;
			}
		}
		$filecount++;
	}}
	unless($filecount){faint "No TXT files found"}
	if($filecount==1){say "Warning: Only one file used."}
	if(scalar(%feats)>1){say "Warning: Mixed feature types."; $feats='mixed'}
	else{$feats=(%feats)[0]}
	if(scalar(%chsize)>1){say "Warning: Different chunk sizes."; $chsize='mixed'}
	else{$chsize=(%chsize)[0]}
	
	to cwd."/feats_$feats.txt";
	out "\@featlist",
	"\@feats\t$feats",
	"\@chsize\t$chsize",
	"\@chcount\t$filecount";
	
	@p=(); @q=();
	for(keys %sum){
		push @p,$sum{$_}/$filecount;
		push @q,$_;
	}
	decsortbyp;
	for $i(0..@p-1){
		$_.="$q[$i]\t$p[$i]\n";
	}
	out;
}

sub makefeatsover{ # Creates feature files for overrepresented features.
	# Args: first profile files(s), other profile filess
	# Instead of ordering by value, orders by the difference in value between
	# the first given argument and all other profiles.
	# Difference is arithmetic, so one minus the other.
	# Chunks are not weighted by size.
	# Note that to give several features as the "first" set,
	# they have to be put in one folder, and the folder used as argument.
	# Printed chunk count is only for the second set.
	
	my $filecount, $feats, $chsize, $firstcount, $othercount;
	my %feats; my %chsize; my %first; my %other; my %used;
	
	if($VERBOSE){say "Creating overrep-feature list..."}
	
	if($MFOT){
		for(recfilesin(@_)){if(/\.txt$/){
			my $pos=0;
			for(in){
				if(/^([^\t\@][^\t]*)\t([^\t]*)/){
					if($pos){$first{$1}+=$2}else{$other{$1}+=$2}
					$used{$1}=1;
				}elsif(/^\@feats\t([^\t]*)/){
					$feats{$1}=1;
				}elsif(/^\@chsize\t([^\t]*)/){
					$chsize{$1}=1;
				}elsif(/^\@([^\t]+)\t?([^\t]*)/){
					if($POSI eq "$1:$2"){$pos=1}
				}
			}
			if($pos){$firstcount++}else{$othercount++}
		}}
	}
	else{
	unless(@_[1]){faint "Need at least two arguments"}
	
	# First argument
	for(recfilesin(shift @_)){if(/\.txt$/){
		$firstcount++;
		for(in()){
		if(/^([^\t\@][^\t]*)\t([^\t]*)/){
			$first{$1}+=$2;
			$used{$1}=1;
		}elsif(/^\@feats\t([^\t]*)/){
			$feats{$1}=1;
		}elsif(/^\@chsize\t([^\t]*)/){
			$chsize{$1}=1;
		}
	}}}

	# Second argument
	for(recfilesin(@_)){if(/\.txt$/){
		for(in){
			if(/^([^\t\@][^\t]*)\t([^\t]*)/){
				$other{$1}+=$2;
				$used{$1}=1;
			}elsif(/^\@feats\t([^\t]*)/){
				$feats{$1}=1;
			}elsif(/^\@chsize\t([^\t]*)/){
				$chsize{$1}=1;
			}
		}
		$othercount++;
	}}
		
	} # end if $MFOT
	
	unless($firstcount){faint "No files found in first argument"}
	unless($othercount){faint "No files found in subsequent arguments"}

	if(scalar(%feats)>1){say "Warning: Mixed feature types."; $feats='mixed'}
	else{$feats=(%feats)[0]}
	if(scalar(%chsize)>1){say "Warning: Different chunk sizes."; $chsize='mixed'}
	else{$chsize=(%chsize)[0]}
	
	to cwd."/feats_$feats.txt";
	out "\@featlist_overrep",
	"\@feats\t$feats",
	"\@chsize\t$chsize",
	"\@chcount\t$othercount";
	
	@p=(); @q=();
	for(keys %used){
		push @p,$first{$_}/$firstcount-$other{$_}/$othercount;
		push @q,$_;
	}
	decsortbyp;
	for $i(0..@p-1){
		$_.="$q[$i]\t$p[$i]\n";
	}
	out;
	
}

sub makefeatsovert{ # Same as makefeatsover, but positives specified by trait.
	# Args: profiles
	# Uses $POSI, set to trait or trait:value.
	# Since we're reading from files, obviously only traits in files count
	# – you can't add traits later.
	
	my $filecount, $feats, $chsize, $firstcount, $othercount;
	my %feats; my %chsize; my %first; my %other; my %used;
	
	if($POSI){
		local $MFOT=1;
		unless($POSI=~/\:/){local $POSI=$POSI.':'}
		makefeatsover(@_);
	}else{faint "\$POSI should contain trait(s) for makefeatsovert"}
	
}

sub getfeattype{ # Gets the feature type from the given files,
	# instead of setting $FEAT_TYPE manually.
	# Args: Any number of files or folders.
	# Warns if feature types differ between files.
	
	unless(@_){faint "Missing arguments to getfeattype()"}
	my %types; my $missingfeats;
	for(recfilesin(@_)){
		my %header=header;
		unless($header{'feats'}){$missingfeats=1}
		$types{$header{'feats'}}=1;
	}
	if($missingfeats){say "Warning: Some file(s) did not specify feature type"}
	if(scalar(keys %types)==0){
		faint "Strange feature type error";
	}elsif(scalar(keys %types)>1){
		gasp "Feature type mismatch";
		$FEAT_TYPE='mixed';
	}else{
		$FEAT_TYPE=(keys %types)[0];
	}
	
	unless($FEAT_TYPES{$FEAT_TYPE}){say "Warning: Unknown feature type"}
}

sub getfeats{ # Reads the given number of features from the appropriate feats file.
	# Args: (maximum) feature count, feature type
	# By default, uses $FEAT_MAX and $FEAT_TYPE.
	# So you can set them either way.
	# Sets the constants if given as arguments.
	
	if($VERBOSE){say "Getting feature list..."}
	$FEAT_MAX=($_[0] or $FEAT_MAX or faint "No maximum feature count specified");
	$FEAT_TYPE=($_[1] or $FEAT_TYPE or faint "No feature type specified");
	unless($FEAT_TYPES{$FEAT_TYPE}){say "Warning: Unknown feature type"}
	
	open my $fh, "<:encoding(UTF-8)",cwd."/feats_$FEAT_TYPE.txt" or faint "Could not open feature file";
	my $lines=0;
	while(<$fh>){
		chomp;
		if(/^\@/ or /^\s*$/){next}
		/^([^\t]+)\t?([^\t]*)/;
		push @FEAT_KEY,$1;
		push @FEAT_WEIGHT,$2;
		if(++$lines==$FEAT_MAX){last}
	}
	$GOTFEATS=1;
}

sub loadprof{ # Loads profiles from the given files.
	# Optionally, traits can be set for the loaded profiles.
	# Args: files or folders, [1, traits]
	
	if($VERBOSE){say "Loading profiles..."}
	unless($GOTFEATS){say "Warning: No features loaded"}
	unless($_[0]){say "Warning: No files given to loadprof"}
	my %feats; my @files; my @in; my $foundcomments;
	for(0..@FEAT_KEY-1){$ind{$FEAT_KEY[$_]}=$_}
	my $oldprofs=$PROFS;
	
	while($_=shift and $_ != 1){
		push @in,$_;
	}
	
	for(recfilesin(@in)){
		my $i=0;
		my $found=0;
		open my $fh, $_ or faint "Could not open file";
		while(<$fh>){
			if(/^\@(\S+)\s*$/){ # header with no value
				$trait[$PROFS]{$1}=$BLANK;
				$haver{$1}{$PROFS}=1;
				$tlist{$1}=1;
			}
			elsif(/^\@(\S+)\s+(.*)$/){ # header with value
				$trait[$PROFS]{$1}=$2;
				$haver{$1}{$PROFS}=1;
				$haverv{$1}{$2}{$PROFS}=1;
				$tlistv{$1}=1;
				
			}elsif(/^\s*$H/ or /^\s*$/){ # a comment or empty line
				# do nothing
			}elsif(/([^\t]*)\t([0-9\.\-e]*)$/){
				if(defined($ind{$1})){ # a regular line
					if($feat[$PROFS][$ind{$1}]){say "Warning: Repeated feature value"}
					$feat[$PROFS][$ind{$1}]=$2;
					$found++;
					if($found==$FEAT_MAX){next} # quit reading when found all interesting feats
				} # else: non-included feature, do nothing
				
			}else{$foundcomments++}
		}
		$PROFS++;
	}
	if(@_){
		tset($oldprofs..$PROFS-1,@_);
	}
	if($foundcomments){say "Skipped $foundcomments non-conforming lines."}
}

sub clearprof{ # Unloads previously loaded profiles, in case we want to start over.
	@trait=();
	%haver=();
	%haverv=();
	%tlist=();
	%tlistv=();
	@feat=();
	$PROFS=0;
	@COSDSSQ=();
}

sub whois{ # Shows info for a profile (or more than one). For printouts and debugging.
	my @a; my @b;
	if(defined($_[0])){@a=@_}else{@a=($_)}
	for(@a){
		unless(/^\d*$/){gasp "Expected profile index"; next}
		if($trait[$_]{'label'}){push @b,$trait[$_]{'label'}}
		else{push @b,$trait[$_]{'author'}.' ➤ '.$trait[$_]{'title'}.' ➤ '.$trait[$_]{'chnum'}}
	}
	
	if(wantarray){
		return @b;
	}
	if(defined(wantarray)){
		return join "\n",@b;
	}
	say @b;
}

sub classify{ # Finds the most likely class for the given profile(s).
	# If no profiles are given, uses all loaded profiles.
	# Uses the 'train' trait, and optionally $TARGET.
	# If $TARGET is false, returns/prints the profile index instead.
	# Specifically, uses ALL profiles with that trait, unlike acctest().
	# If you include training profiles in the argument, they will
	# be ignored, which might get confusing if you try to pair up
	# input and output profiles.
	
	my @in; my @test; my @class;
	my $method=$METHOD || 'dist_sum';
	if(@_){@in=@_}else{@in=0..$PROFS-1}
	unless(@in){faint "No profiles given"}
	if(scalar(keys %{$haver{'train'}})<2){faint "Not enough training profiles set"} # use twho instead?
	for(@in){
		unless($haver{'train'}{$_}){push @test,$_}
	}
	unless(@test){faint "No test profiles remaining"}
	
	for $test(@test){
		my $bestdist=99999999;
		my $bestprof=-1;
		for $train(keys %{$haver{'train'}}){
			my $d=&$method($train,$test);
			if($d<$bestdist){
				$bestdist=$d;
				$bestprof=$train;
			}
		}
		if($TARGET){push @class,$trait[$bestprof]{$TARGET};}
		else{push @class,$bestprof;}
	}
	if(wantarray){
		return @class;
	}else{
		my $ml=0;
		for(@class){if(length>$ml){$ml=length}}
		$ml+=4;
		for(0..@test-1){
			say sprintf("%-${ml}s",$class[$_]).whois($test[$_]);
		}
	}
	
}

sub acctest{ # Runs an accuracy test on the given profiles, without crossfolding.
	# If no profiles are given, uses all loaded profiles.
	# Uses the 'train' trait.
	
	my %cat; my @train; my @test; my %catcorr; my @p; my $corr, $totcorr;
	my $say=!defined(wantarray) or $VERBOSE;
	unless($TARGET){$TARGET='author'}
	unless($METHOD){$METHOD='dist_sum'}
	if(@_){@p=@_}else{@p=0..$PROFS-1}
	
	for(@p){
		if($debug){say "debug\tprof $_ is ".$trait[$_]{'author'}.' '.$trait[$_]{'title'}.' '.$trait[$_]{'chnum'}}
		if($trait[$_]{'train'}){
			push @train,$_;
		}else{
			push @test,$_;
		}
	}
	unless(@train){faint "No training profiles set"}
	if(@train==1){faint "Only one training profile set"}
	unless(@test){faint "No test profiles remaining"}
	
	
	for $test(@test){
		my $bestdist=99999999;
		my $bestprof=-1;
		for $train(@train){
			my $d=&$METHOD($train,$test);
			if($d<$bestdist){
				$bestdist=$d;
				$bestprof=$train;
			}
		}
		if($trait[$bestprof]{$TARGET} eq $trait[$test]{$TARGET}){
			$corr++;
			$catcorr{$trait[$test]{$TARGET}}++;
		}
		$catcount{$trait[$test]{$TARGET}}++;
	}
	if($say){say "Accuracies for each $TARGET"}
	for(keys %catcorr){
		push @catacc,$catcorr{$_}/$catcount{$_};
		if($say){say "$_\t".($catcorr{$_}/$catcount{$_})}
	}
	$ACCTOT=$corr/@test;
	$ACCMEAN=mean(@catacc);
	if($say){
		say "Total accuracy:\t$ACCTOT\nMean accuracy:\t$ACCMEAN";
	}
	return $ACCTOT;

	
}

sub trainbyindex{ # Sets for each class the n:th profile to be training data.
	# Args: index, profiles.
	# If index is omitted or -1, uses (and then increments) $TRAIN.
	# If profiles are omitted, uses all loaded profiles with the given target trait.
	# Note that you can't omit the index if profiles are given
	# (which is why you can instead set it to -1).
	# Uses $TARGET.
	unless($TARGET){faint "\$TARGET not set"}
	my %seen; my @p; my @chosen; my $index;
	if(@_){
		if($_[0]==-1){$index=$TRAIN++}
		else{$index=$_[0]}
		shift;
	}
	if(@_){@p=sort{$a<=>$b} @_}else{@p=sort{$a<=>$b} keys %{$haver{$TARGET}}}
	for $p(@p){
		unless($haver{$TARGET}{$p}){faint "Profile $p does not have trait $TARGET"}
		if($seen{$trait[$p]{$TARGET}}++ == $index){push @chosen,$p}
	}
	for(keys %seen){
		if($seen{$_}<$index+1){faint "Needed ".($index+1)." profiles with $TARGET:$_"}
	}
	tonly(@chosen,'train');

}

sub trainbyrand{ # Picks for each class a random profile as training data.
	# Args: profiles
	# Uses $TARGET.
	# By default, uses all loaded profiles with the given target.
	unless($TARGET){faint "\$TARGET not set"}
	my @p; my %cat; my @chosen;
	if(@_){@p=@_}else{@p=keys %{$haver{$TARGET}}}
	for $p(@p){
		push @{$cat{$trait[$p]{$TARGET}}},$p
	}
	for(keys %cat){
		push @chosen,$cat{$_}[int(rand(@{$cat{$_}}))];
	}
	tonly(@chosen,'train');
}

sub comptwo{ # Obsolete as public; comp() uses this for two profiles.
	# Prints or returns the distance between two profiles.
	# Args: index0, index1
	# Uses $METHOD, $FEAT_COUNT.
	# If only two profiles are loaded, uses those.
	
	my $method=$METHOD || 'dist_sum';
	
	unless(defined($_[1])){
		if($PROFS==2){return comptwo(0,1)}
		else{faint "No profiles specified for comparison"}
	}
	
	if(defined(wantarray)){
		return &$method(@_);
	}else{
	say 'Distance between';
	whois($_[0]);
	say 'and';
	whois($_[1]);
		say 'is';
	say &$method(@_);
	}
}

sub comp{ # Compares one profile to all the other (given) profiles.
	# If only one profile is given, compares to all other loaded.
	# Args: index0, list of other indices
	# In list context, returns the list of profiles by order of distance.
	# In scalar context, returns the top match.
	# In void context, prints the list.
	# Uses $METHOD, $FEAT_COUNT.
	
	if(scalar(@_)==2 or (scalar(@_)==0 and $PROFS==2)){return comptwo(@_)}
	
	my $first=shift;
	my @other;
	unless(defined($first)){faint "No profiles specified for comparison"}
	my $method=$METHOD || 'dist_sum';
	
	if(@_){@other=@_}
	elsif($PROFS>1){@other=0..$PROFS-1} # we'll skip $first in the main loop
	
	@p=(); @q=();
	for(@other){unless($_ eq $first){
		push @q,$_;
		push @p, &$method($first,$_);
	}}
	sortbyp;
	
	if(wantarray){
		return @q;
	}elsif(defined(wantarray)){
		return $q[0];
	}else{
		say "Distance from ".whois($first);
		for(0..@p-1){
			say $p[$_]."\t".whois($q[$_]);
		}
	}
	
}

sub bae{ # Compares all the profiles, for the book/author experiment.
	# Each pair is categorised as same book, same author, or different.
	# Distributions are written to output files.
	# Input: List of indices. By default, all loaded profiles.
	# Globals used: $METHOD, $DOBLUR, $GAUSS_SD (if $DOBLUR is set)
	
	if($VERBOSE){say "Running author/book experiment..."}
	unless($PROFS){say "Warning: No profiles loaded"}
	
	my @all;  my $cat;
	if(@_){@all=@_;}else{@all=0..$PROFS-1}
	
	for(0..@all-2){
		$i=$all[$_];
		if($VERBOSE){whois($i)}
		for($_+1..@all-1){
			$j=$all[$_];
			
			if($header[$i]{'author'} eq $header[$j]{'author'}){
				if($header[$i]{'title'} eq $header[$j]{'title'}){
					$cat='samebook';
				}else{
					$cat='sameauth';
				}
			}else{$cat='diff'}
			push @$cat,&$METHOD($i,$j);
			
		}
	}
	for('samebook','sameauth','diff'){
		to cwd."/bae_$_.txt";
		out "\@bae\t$_",
			"\@count\t".scalar(@$_);
		out sort @$_;
	}
	$GOTBAE=1;
}

sub baegraph{ # Takes the output from bae(), produces a graph data file for Nyxgraph.
	# Unsurprisingly, you need to run bae() first, but the results are saved to file,
	# so it doesn't have to be in the same session.
	
	my ($ymax, $out);
	
	unless($GOTBAE){
		for $cat('samebook','sameauth','diff'){
			for(in("bae_$cat.txt")){
				if(/^[0-9\.\-e]+$/){push @$cat,$_}
			}
		}
	}
	
	local $GAUSS_SD=$GAUSS_SD;
	my $max=max(@samebook,@sameauth,@diff);
	unless($GAUSS_SD){$GAUSS_SD=($max-$min)/20}
	
	to "bae_graph.txt";
	out "xmin=0",
		"xmax=$max",
		"ymin=0";
	my @cols=('c','b','a');
	# Bit of an odd workaround with $out,
	# since $ymax needs to be printed before the numbers,
	# but can only be calculated after.
	
	$GAUSS_DIST=1;
	$GAUSS_TOT=1;
	for('samebook','sameauth','diff'){
		$out.="\ncol ".(shift @cols)."\nnewconn\n";
		@x=@$_;
		$out.=gaussblur;
		$ymax=max($ymax,@gaussy);
	}
	
	out "ymax=$ymax",
		"textx=200",
		"texty=30",
		"textsize=12",
		"textdy=14",
		"setup",
		"text x = distance",
		"text y = prob. density",
		"col c",
		"text same book",
		"col b",
		"text same author",
		"col a",
		"text different",
		$out;
}

sub baesim{ # "Simulates" the book/author experiment, that is,
	# calculates the expected accuracies for a given number of candidates.
	# Args: Number of candidates.
	# Alternatively, set $BAESIM_CANDS. Defaults to 100.
	# Uses the files created by bae().
	# Uses private globals @samebook, @sameauth, @diff.
	# Returns, in list context, acc for (author+topic, only author, only topic).
	
	my ($low,$mid,$high,$good,$bad,$badc,$prob);
	my @out;
	my $outof=$_[0] || $BAESIM_CANDS || 100;
	
	unless($GOTBAE){
		for $cat('samebook','sameauth','diff'){
			for(in("bae_$cat.txt")){
				if(/^[0-9\.\-e]+$/){push @$cat,$_}
			}
		}
	}
	
	my sub aux{
		my @acc;
		for(@{$_[0]}){
			$low=0;
			$high=@{$_[1]};
			while($low<$high-1){
				$mid=int(($low+$high)/2);
				if(${$_[1]}[$mid]<$_){$low=$mid}else{$high=$mid}
			}
			##$prob=1-($low/(@{$_[1]}-1));
			$prob=1-($low/(@{$_[1]}-1));
			push @acc,$prob**($outof-1);
		}
		if(defined(wantarray)){
			if(wantarray){
				push @out,mean(@acc);
			}else{say "Baesim makes no sense in scalar context"; exit}
		}else{say "$_[2]: ".mean(@acc);}
		
	}
	
	aux('samebook','diff','Same book vs. different (author+topic)');
	aux('sameauth','diff','Same author vs. different (only author)');
	aux('samebook','sameauth','Same book vs. same author (only topic)');
	if(wantarray){return @out}
	
}

sub rank{ # Ranks the given profiles.
	# If no arguments given, ranks all the loaded profiles.
	# Args: list of profile indices
	# In list context, returns the ranked list of profiles.
	# In scalar context, returns the top match.
	# In void context, prints the list.
	# Uses $METHOD, $FEAT_COUNT.
	
	if($VERBOSE){say "Ranking profiles..."}
	unless($METHOD=~/^rank/){say "Warning: Unexpected ranking algorithm"};
	my @profs;
	if(@_){@profs=@_}
	elsif($PROFS>1){@profs=0..$PROFS-1}
	
	@p=(); @q=();
	for(@profs){
		push @q,$_;
		push @p, &$METHOD($_);
	}
	sortbyp;
	
	if(wantarray){ # list context
		return @q;
	}elsif(defined(wantarray)){ # scalar context
		return $q[0];
	}else{ # void context
		say "Ranking:";
		for(0..@p-1){
			say sprintf("%.12f",$p[$_])."\t".whois($q[$_]);
		}
	}
	
}

sub rankgraph{ # Tests a ranking to estimate how likelihood depends on rank value.
	# Args: List of all profiles to use, positive and negative.
	# If omitted, uses @RANK_ALL, or else all loaded profiles.
	# Globals:
	# $POSI is the trait that distinguishes positive profiles – mandatory!
	# $RANK_APRI is the apriori probability.
	# If omitted or 0, not adjusted from the profiles.
	# Also uses $METHOD, $FEAT_COUNT, $GAUSS_SD, and the other gaussian params.
	
	if($VERBOSE){say "Creating ranking graph..."}
	my ($fact, $poscount, $x, $infcount);
	my @all;
	if(@_){@all=@_}
	elsif(@RANK_ALL){@all=@RANK_ALL}
	else{@all=0..$PROFS-1}
	
	unless(@all){faint "No profiles given to rankgraph"}
	unless($POSI){faint "\$POSI should be set to the positive trait"}
	my %posi=ynhash(twho($POSI));

	@x=();
	local $GAUSS_DIST=0;
	my $x;
	for(@all){
		$x=&$METHOD($_);
		if($x eq 'inf'){$infcount++;next}
		push @x,$x;
		push @y,0+$posi{$_};
		if($posi{$_}){$poscount++}
	}
	if($VERBOSE and $infcount){say "$infcount omitted for infinite distance."}
	
	gaussblur;
	
	my @yout;
	if($RANK_APRI){
		my $fact=$RANK_APRI/(1-$RANK_APRI)*(@all-$poscount-$infcount)/$poscount;
		for(@gaussy){
			my $adjrat=$_/(1-$_)*$fact;
			push @yout,$adjrat/($adjrat+1);
		}
		@gaussy=@yout;
	}else{@yout=@gaussy}
	
	my $xmin=min(0,@gaussx);
	my $xmax=max(0,@gaussx);
	my $xlabstep=10**int(lg(($xmax-$xmin)*1.1));
	my $xgridstep=$xlabstep/10;
	to "rank_graph.txt";
	out "xmin=$xmin",
	"xmax=$xmax",
	"ymin=0",
	"ymax=1",
	"textx=250",
	"texty=30",
	"textsize=12",
	"textdy=14",
	"xlabstep=$xlabstep",
	"xgridstep=$xgridstep",
	"ylabstep=0.5",
	"ygridstep=0.1",
	"setup",
	"text x = distance",
	"text y = probability";
	for(@gaussx){
		out "$_\t".shift @yout;
	}
	
}

sub importplain{ # Reads plain text files or markup, creates profiles based on frequencies.
	# Args: List of files/folders.
	# Uses the following globals.
	# $FEAT_TYPE: Which feature type to extract ('word' or 'cgram'); default is 'word'
	# (cgram is not implemented yet)
	# $IMP_CHUNK: Whether to split the text in chunks, and size of chunks.
	#     0 (def) = no chunks, use all text
	#     positive int = use this chunk size
	# $IMP_CHUNKLIM: Limit to number of chunks.
	#     0 (def) = continue as long as there is enough text for another chunk
	#     positive int = stop after this many chunks
	# $IMP_JOIN: Whether to join all input files, or process them separately (def).
	# If both $IMP_JOIN and $IMP_CHUNK, chunks may contain text from several files.
	# $IMP_CASE: 0 (def) to convert everything to lowercase, 1 to keep case
	# $IMP_AUTHOR, $IMP_TITLE: Header values for author identification
	# $IMP_LABEL: Header value for arbitrary classes
	# $IMP_DIR: Folder to put the results in (inside "profiles").
	# By default, tries author, title or label, otherwise picks a random code.
	
	# Not implemented yet:
	# $IMP_SKIPTAGS: 1 to remove SGML/XML tags; default is 0
	# $IMP_SEP: How to separate words.
	#     0 (def) = all whitespace, tags (if $IMP_SKIPTAGS=1), and word boundaries
	#     1 = all whitespace but nothing else
	#     2 = only linebreaks
	# $IMP_GRAMLEN: number of characters for character engrams
	# Possibly something about automatic headers?
	
	unless(@_){say "Warning: Missing file(s) for import."; return}
	unless($IMP_AUTHOR or $IMP_TITLE or $IMP_LABEL){say "Warning: Missing class label."}
	my $chunk=0; my $i, $label;
	my $chunksize=$IMP_CHUNK || 'inf';
	my $chunklim=$IMP_CHUNKLIM || 'inf';
	my $dir=$IMP_DIR || $IMP_LABEL || $IMP_TITLE || $IMP_AUTHOR || int(rand(1000000));
	my sub output;
	if($IMP_LABEL){$label=$IMP_LABEL}
	elsif(!$IMP_TITLE and !$IMP_AUTHOR){$label=$dir}
	mkdir "profiles/$dir";
	
	for(recfilesin(@_)){
		open my $fh, "<:encoding(UTF-8)",$_ or faint "Could not open $_";
		if($i and !$IMP_JOIN){ # ran out of file – reset chunk
			$i=0;
			%count=();
		}
		while(<$fh>){
			if(/^$H/){next}
			chomp;
			unless($IMP_CASE){$_=lc}
			for(split){
				$count{$_}++;
				$i++;
				if($i==$chunksize){
					output();
					$chunk++;
					if($chunk==$chunklim){return}
					$i=0;
					%count=();
				}
			}
		}
		close $fh;
	}
	if($chunksize eq 'inf'){$chunksize=$i; if($i){output()}}
	
	
	sub output{
		to "profiles/$dir/$chunk.txt";
		if($label){out "\@label\t$IMP_LABEL"} # todo: generalise
		if($IMP_AUTHOR){out "\@author\t$IMP_AUTHOR"}
		if($IMP_TITLE){out "\@title\t$IMP_TITLE"}
		if($IMP_YEAR){out "\@year\t$IMP_YEAR"}
		out "\@feats\t$FEAT_TYPE";
		out "\@chsize\t$chunksize";
		out "\@chnum\t$chunk";
		@p=values %count;
		@q=keys %count;
		decsortbyp;
		for $q(@q){
			out "$q\t".$count{$q}/$chunksize;
		}
	}
}
	
sub importSB{ # Reads SB XML files, creates profiles based on frequencies.
	# Args: List of files/folders.
	# Uses the following globals.
	# $FEAT_TYPE: Which feature type to extract ('word', 'pos' or 'cgram'); default is 'word'
	# (cgram not implemented yet)
	# $IMP_CHUNK: Whether to split the text in chunks, and size of chunks.
	#     0 (def) = no chunks, use all text
	#     positive int = use this chunk size
	# $IMP_CHUNKLIM: Limit to number of chunks.
	#     0 (def) = continue as long as there is enough text for another chunk
	#     positive int = stop after this many chunks
	# $IMP_CASE: 0 (def) to convert everything to lowercase, 1 to keep case
	# $IMP_DIR: Folder to put the results in (inside "profiles").
	# By default, name of original file (minus extension).
	
	# Not implemented yet:
	# $IMP_AUTHOR, $IMP_TITLE: Header values for author identification
	# $IMP_LABEL: Header value for arbitrary classes
	# $IMP_GRAMLEN: number of characters for character engrams
	# $IMP_JOIN: Whether to join all texts of the input file,
	# or process them separately (def).
	# If both $IMP_JOIN and $IMP_CHUNK, chunks may contain text from several files.
	# (Not sure joining is useful for SB files.)
	
	
	unless(@_){say "Warning: Missing file(s) for import."; return}
	for(recfilesin(@_)){
		my $infile=0;
		my $words=0;
		/\/([^\/\.]*)\.[^\/]*$/;
		my $dir=$IMP_DIR || $1;
		my $chunksize=$IMP_CHUNK || 'inf';
		my $chunklim=$IMP_CHUNKLIM || 'inf';
		my $author, $title, $year;
		my $c=0; # current chunk index
		my %count;
		
		my sub starttext{
			if(/author="([^\"]*)"/){$author=$1}else{die "Missing author"}
			if(/title="([^\"]*)"/){$title=$1}else{die "Missing title"}
			if(/year=".*?([^\"][^\"][^\"][^\"])"/){$year=$1}else{$year=0}
			if($VERBOSE){say "$author: $title"}
			$c=0;
			$words=0;
			%count=();
		}
		
		my sub endchunk{
			to cwd."/profiles/$dir/$author/$title/$c.txt";
			out "\@author\t$author",
				"\@title\t$title",
				"\@year\t$year",
				"\@feats\t$FEAT_TYPE",
				"\@chsize\t$chunksize",
				"\@chnum\t$c";
			
			@p=values %count;
			@q=keys %count;
			decsortbyp;
			for $q(@q){
				out "$q\t".$count{$q}/$chunksize;
			}
			$words=0;
			%count=();
			$c++;
		}
		
		my sub word{
			if($FEAT_TYPE eq 'word'){
				$count{$_[1]}++;
			}elsif($FEAT_TYPE eq 'pos'){
				$_[0]=~/pos=\"(.*?)\"/;
				$count{$1}++;
			}
			
			if(++$words==$chunksize){
				endchunk;
				$words=0;

			}
		}
		
		mkdir "profiles/$dir";
		open my $fh, "<:encoding(UTF-8)",$_ or faint "Could not open $_";
		while(<$fh>){
			if(/^\s*<text / and not $intext){
				$intext=1;
				starttext;
			}elsif(/<\/text>.*?<text / and $intext){
				starttext;
			}elsif(/<\/text/ and $intext){
				$intext=0;
			}elsif(/^\s*<w (.*?)>(.*?)<\/w>/ and $intext and $c<$chunklim){
				if($IMP_CASE){word($1,$2)}
				else{word($1,lc($2))}
}}}}












1;

