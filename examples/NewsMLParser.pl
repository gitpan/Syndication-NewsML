#!/usr/bin/perl

package NewsMLParser;

# not finished. It's still a good example of what you can do with the module, though.

use strict;
use NewsML;

MAIN:
{
	my $filename = $ARGV[0] || die "Usage: $0 <filename to parse>\n";
	my $newsml = new Syndication::NewsML($filename);
	my $env = $newsml->getNewsEnvelope;

	print "date and time is ".$env->getDateAndTime->getText."\n";

	print "priority is ".$env->getPriority->getFormalName."\n" if $env->getPriority;

	print "news item count is ".$newsml->getNewsItemCount."\n";

	my $items = $newsml->getNewsItemList;
	my $itemtype;
	my $count = 1;
	foreach my $item (@$items) {
		# get type
		$itemtype = $item->getType;
		print "news item ".$count." (".$itemtype.")\n";

		# get identification
		print "Identification info:\n";
		my $identifier = $item->getIdentification->getNewsIdentifier;
		print " Provider id: ".$identifier->getProviderId->getText."\n";
		print " Date id: ".$identifier->getDateId->getText."\n";
		print " Revision id: ".$identifier->getRevisionId->getText
			. " (update = ". $identifier->getRevisionId->getUpdate
			. ", Previous Revision = ".$identifier->getRevisionId->getPreviousRevision.")"
			. "\n";
		print "Public identifier: ".$identifier->getPublicIdentifier->getText."\n";

		# get management info
		print "Management info:\n";
		my $management = $item->getNewsManagement;
		print " NewsItemType: ".$management->getNewsItemType->getFormalName."\n";
		print " First created: ".$management->getFirstCreated->getText."\n";
		my $tstamp = $management->getFirstCreated->getDatePerl;
		print " First created (Perl): ".$tstamp."\n";
		my ($ss, $mi, $hh, $dd, $mm, $yy) = gmtime($tstamp);
		print " which translates in UTC to ".sprintf("%4d/%02d/%02d %02d:%02d:%02d",1900+$yy,$mm+1,$dd,$hh,$mi,$ss)."\n";
		($ss, $mi, $hh, $dd, $mm, $yy) = localtime($tstamp);
		print " which translates in local timezone to ".sprintf("%4d/%02d/%02d %02d:%02d:%02d",1900+$yy,$mm+1,$dd,$hh,$mi,$ss)."\n";

		print " This revision created: ".$management->getThisRevisionCreated->getText."\n";
		print " Status: ".$management->getStatus->getFormalName."\n";
		# should deal with types of status (embargoed etc)
		print " Urgency: ".$management->getUrgency->getFormalName."\n" if $management->getUrgency;

		if ($itemtype eq "NewsComponent") {
			my $comp = $item->getNewsComponent;

			# parse a news component (the most common type of NewsItem)
			&parseNewsComponent($comp, 0);
		} elsif ($itemtype eq "Update") {
			my $update = $item->getUpdate;
			print "got update\n";
			print " ...\n";
		} elsif ($itemtype eq "TopicSet") {
			my $topicset = $item->getTopicSet;
			print "got topicset\n";
			print " ...\n";
		} else {
			print "no NewsComponent, Update or TopicSet -- strange, but legal\n";
		}
	}
}

sub parseNewsComponent {
	my ($newscomp, $indent) = @_;
	# parse a news component (the most common type of NewsItem)
	print " " x $indent . "News Component:\n";
	print " " x $indent . " Formal Name: ".$newscomp->getRole->getFormalName."\n" if $newscomp->getRole;
	my $equiv = $newscomp->getEquivalentsList;
	print " " x $indent . " Equivalents list: ".$equiv;
	if ($equiv eq "yes") {
		my $basis = $newscomp->getBasisForChoiceList->[0]->getText;
		print " (basis for choice: ".$basis.")";
	}
	print "\n";
	if ($newscomp->getAdministrativeMetadata) {
		print " " x $indent . " Provider: ".$newscomp->getProvider->getDescriptionList->[0]->getText."\n";
		print " " x $indent . " Creator: ".$newscomp->getCreator->getDescriptionList->[0]->getText."\n";
	}
	if ($newscomp->getRightsMetadata) {
		print " " x $indent . " Copyright holder: ".$newscomp->getCopyrightHolder."\n";
	}
	# arbitrary metadata
	if ($newscomp->getMetadataList->[0]) {
		my $meta = $newscomp->getMetadataList->[0];
		print " " x $indent . " Metadata: ".$meta->getMetadataType->getFormalName.": ";
		print $meta->getPropertyList->[0]->getFormalName." = ".$meta->getPropertyList->[0]->getValue."\n";
	}
	# look for child news components
	my $childnewscomps = $newscomp->getNewsComponentList;
	foreach my $childnewscomp (@$childnewscomps) {
		&parseNewsComponent($childnewscomp, $indent + 2);
	}
}
