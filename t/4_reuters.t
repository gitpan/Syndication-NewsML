#!/usr/bin/perl -w
use strict;
use Test;

use Syndication::NewsML;

my $TESTS;
BEGIN { 
#   require "t/TestDetails.pm"; import TestDetails;
   $TESTS = 27;
   plan tests => $TESTS; 
}

MAIN:
{
	my $filename = "t/test_data/reuters-test.xml";
	my $newsml = new Syndication::NewsML($filename);

	# test 1: could open and read file
	ok($newsml);

	my $catalog = $newsml->getCatalog;

	# test 2: catalog exists
	ok(defined($catalog));

	# test 3: number of resources matches what we think
	ok($catalog->getResourceCount, 13);

	my $resource = $catalog->getResourceList->[2];

	# test 4
	ok($resource->getUrlList->[0]->getText, "http://www.reuters.com/ids/vocabularies/ids/destination.xml");

	# test 5
	ok($resource->getDefaultVocabularyForList->[0]->getContext, "NewsService");

	my $newsenvelope = $newsml->getNewsEnvelope;

	# test 6
	ok($newsenvelope->getDateAndTime->getText, "20010907T151305+0000");

	# test 7
	ok($newsenvelope->getNewsServiceList->[0]->getFormalName, "OUWDPLUS");

	# test 8
	ok($newsenvelope->getPriority->getFormalName, "4");

	my $newsitem = $newsml->getNewsItemList->[0];

	my $identifier = $newsitem->getIdentification->getNewsIdentifier;

	# test 9
	ok($identifier->getProviderId->getText, "Reuters.Com");

	# test 10
	ok($identifier->getDateId->getText, "20010907");

	# test 11
	ok($identifier->getNewsItemId->getText, "ID00301");

	# test 12
	ok($identifier->getRevisionId->getPreviousRevision, "0");

	# test 13
	ok($identifier->getRevisionId->getUpdate, "N");

	# test 14
	ok($identifier->getRevisionId->getText, "1");

	# test 15
	ok($identifier->getPublicIdentifier->getText, "urn:newsml:Reuters.Com:20010907:ID00301:1");

	my $newsmanagement = $newsitem->getNewsManagement;

	# test 16
	ok($newsmanagement->getNewsItemType->getFormalName, "News");

	# test 17
	ok($newsmanagement->getFirstCreated->getText, "20010907T151305+0000");

	# test 18 -- Perl time test
	ok($newsmanagement->getFirstCreated->getDatePerl, 999875585);

	# test 19
	ok($newsmanagement->getThisRevisionCreated->getText, "20010907T151305+0000");

	# test 20
	ok($newsmanagement->getStatus->getFormalName, "Usable");

	# test 21
	ok($newsmanagement->getUrgency->getFormalName, "4");

	my $newscomponent = $newsitem->getNewsComponent;

	# test 22
	ok($newscomponent->getEquivalentsList, "no");

	my $resource2 = $newscomponent->getCatalog->getResourceList->[0];

	# test 23
	ok($resource2->getUrlList->[0]->getText, "http://www.reuters.com/ids/vocabularies/topictypes.xml");

	# test 24
	ok($resource2->getDefaultVocabularyForList->[0]->getContext, 'TopicSet/@FormalName');

	my $topicset = $newscomponent->getTopicSetList->[0];

	# test 25
	ok($topicset->getFormalName, "companies");

	my $resource3 = $topicset->getCatalog->getResourceList->[0];

	# test 26
	ok($resource3->getUrlList->[0]->getText, "http://www.reuters.com/ids/vocabularies/nasdaqvocab.xml");

	# test 27
	ok($resource3->getDefaultVocabularyForList->[0]->getContext, 'Topic/TopicType/@FormalName');

}
