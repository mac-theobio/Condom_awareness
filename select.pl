use strict;
use 5.10.0;
while(<>){
	chomp;
	my @ln = split /,\s*/, $_;
	my @new;

	foreach my $rec (@ln){
		$rec =~ s/^V(.*)/$&, MV$1/;
		$rec =~ s/^S(.*)/$&, SM$1/;
		push @new, $rec;
	}
	say join ", ", @ln;
}
