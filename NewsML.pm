# $Id: NewsML.pm,v 0.7 2002/01/11 03:20:15 brendan Exp $
# Syndication::NewsML.pm

$VERSION     = sprintf("%d.%02d", q$Revision: 0.7 $ =~ /(\d+)\.(\d+)/);
$VERSION_DATE= sprintf("%s", q$Date: 2002/01/11 03:20:15 $ =~ m# (.*) $# );

$DEBUG = 1;

#
# Syndication::NewsML -- initial parser. Maybe this should be Syndication::NewsML::Parser or something?
# also grabs the first NewsML element to save time, is that a good idea?
# does it mean that you can't grab extra namespace/DTD declarations etc?
#
package Syndication::NewsML;
use Carp;
use XML::DOM;
@ISA = qw( Syndication::NewsML::IdNode Syndication::NewsML::CatalogNode Syndication::NewsML::TopicSetNode );

sub _init {
	my ($self, $filename) = @_;

	$self->{parser} = new XML::DOM::Parser;
	$self->{doc} = $self->{parser}->parsefile($filename);
	$self->{node} = $self->{doc}->getElementsByTagName("NewsML", 0)->item(0);

	$self->{_singleElements}{NewsEnvelope} = REQUIRED;
	$self->{_multiElements}{NewsItem} = ONEORMORE;

	return $self;
}

=pod

=head1 NAME

Syndication::NewsML -- Parser for NewsML documents

=head1 VERSION

Version $Revision: 0.7 $, released $Date: 2002/01/11 03:20:15 $

=head1 SYNOPSIS

 use Syndication::NewsML;

 my $newsml = new Syndication::NewsML("myNewsMLfile.xml");
 my $env = $newsml->getNewsEnvelope;

 my $dateAndTime = $env->getDateAndTime->getText;

 my $items = $newsml->getNewsItemList;
 foreach my $item (@$items) {
   # do something with the news item
 }
 ...

=head1 DESCRIPTION

B<Syndication::NewsML> parses XML files complying to the NewsML specification, created by the International
Press Telecommunications Council (http://www.iptc.org).

NewsML is a standard format for the markup of multimedia news content.
According to the newsml.org website, NewsML is
"An XML-based standard to represent and manage news throughout its lifecycle, including production,
interchange, and consumer use."

NewsML differs from simpler news markup and syndication standards such as RSS (see the XML::RSS module
on your local CPAN) in that RSS files contain B<links> to stories, whereas NewsML can be used to send
links or the story itself, plus any associated information such as images, video or audio files, PDF
documents, or any other type of data.

NewsML also offers much more metadata information than RSS, including links between associated content;
the ability to revoke, update or modify previously sent stories; support for sending the same story in
multiple languages and/or formats; and a method for user-defined metadata known as Topic Sets.

Theoretically you could use RSS to link to articles created in NewsML, although in reality news
providers and syndicators are more likely to use a more robust and traceable syndication transport
protocol such as ICE (see http://www.icestandard.org).

Syndication::NewsML is an object-oriented Perl interface to NewsML documents. It aims to let users manage
and create NewsML documents without any specialised NewsML or XML knowledge.

=head2 Initialization

At the moment the constructor can only take a filename as an argument, as follows:

  my $newsml = new Syndication::NewsML("file-to-parse.xml");

This attaches a parser to the file (using XML::DOM), and returns a reference to the first NewsML
tag. (I may decide that this is a bad idea and change it soon)

=head2 Reading objects

There are five main types of calls:

=over 4

=item *

Return a reference to an array of elements:

  my $topicsets = $newsml->getTopicSetList;

The array can be referenced as @$topicsets, or an individual element can be referenced as $topicsets->[N].

=item *

Return the size of a list of elements:

  my $topicsetcount = $newsml->getTopicSetCount;

=item *

Get an individual element:

  my $catalog = $topicsets->[0]->getCatalog;

=item *

Get an attribute of an element (as text):

  my $href = $catalog->getHref;

=item *

Get the contents of an element (ie the text between the opening and closing tags):

  my $urlnode = $catalog->getResourceList->[0]->getUrlList->[0];
  my $urltext = $urlnode->getText;

=back

Not all of these calls work for all elements: for example, if an element is defined in the NewsML DTD
as having zero or one instances in its parent element, and you try to call getXXXList, B<Syndication::NewsML>
will "croak" an error. (The error handling will be improved in the future so that it won't croak
fatally unless you want that to happen)

The NewsML standard contains some "business rules" also written into the DTD: for example, a NewsItem
may contain nothing, a NewsComponent, one or more Update elements, or a TopicSet. For some of these
rules, the module is smart enough to detect errors and provide a warning. Again, these warnings will
be improved and extended in future versions of this module.

=head2 Documentation for all the classes

Each NewsML element is represented as a class. This means that you can traverse documents as Perl
objects, as seen above.

Full documentation of which classes can be used in which documents is beyond me right now (with over
120 classes to document), so for now you'll have to work with the examples in the B<examples/> and
B<t/> directories to see what's going on. You should be able to get a handle on it fairly quickly.

The real problem is that it's hard to know when to use B<getXXX()> and when to use B<GetXXXList()>
-- that is, when an element can have more than one entry and when it is a singleton. Quite often it
isn't obvious from looking at a NewsML document. For now, two ways to work this out are to try it and see
if you get an error, or to have a copy of the DTD in front of you. Obviously neither of these is
optimal, but documenting all 127 classes just so people can tell this difference is pretty scary as
well, and so much documentation would put lots of people off using the module. So I'll probably come
up with a reference document listing all the classes and methods, rather than docs for each class, in
a future release.  If anyone has any better ideas, please let me know.

=head1 BUGS

None that I know of, but there are probably many. The test suite isn't complete, so not every method
is tested, but the major ones (seem to) work fine. Of course, if you find bugs, I'd be very keen to
hear about them at B<brendan@clueful.com.au>. 

=head1 SEE ALSO

L<XML::DOM>, L<XML::RSS>

=head1 AUTHOR

Brendan Quinn, Clueful Consulting Pty Ltd
(brendan@clueful.com.au)

=head1 COPYRIGHT

Copyright (c) 2001, Brendan Quinn. All Rights Reserved.
This module is free software. It may be used, redistributed
and/or modified under the same terms as Perl itself.

=cut

#
# Syndication::NewsML::DOMUtils -- a few helpful routines
#
package Syndication::NewsML::DOMUtils;
use Carp;

# walk the tree of descendents of $node to look for an attribute $attr with value $value.
# returns the matching node, or undef.
sub findElementByAttribute {
	my ($node, $attr, $value) = @_;
	my $tstattr = $node->getAttributeNode($attr);
	return $node if defined($tstattr) && ($tstattr->getValue eq $value);
	my $iternode;
	if ($node->hasChildNodes) {
		for my $child ($node->getChildNodes) {
			if ($child->getNodeType == XML::DOM::ELEMENT_NODE) {
				$iternode = findElementByAttribute($child, $attr, $value);
			}
			return $iternode if defined($iternode);
		}
	}
	return undef;
}

# return a reference to the NewsML element at the top level of the document.
# will croak if not NewsML element exists in the parent path of the given node.
sub getRootNode {
	my ($node) = @_;
	if (!defined($node)) {
		croak "Invalid document! getRootNode couldn't find a NewsML element in parent path";
	} elsif ($node->getNodeName eq "NewsML") {
		return $node;
	} else {
		return getRootNode($node->getParentNode);
	} 
}

#
# Syndication::NewsML::References -- routines to follow references
# (any ideas for a better name?)
package Syndication::NewsML::References;
use Carp;

# find reference (based on NewsML Toolkit Java version)
# get referenced data from within this document or possibly an external URL.
# parameter useExternal, if true, means we can look outside this document if necessary.
sub findReference {
	my ($node, $reference, $useExternal) = @_;
	# if reference starts with # it's in the local document (or should be)
	if ($reference =~ /^#/) {
		return $node->getElementByDuid(substr($reference, 1));
	} elsif ($useExternal) {
		# use LWP module to get the external document
		use LWP::UserAgent;
		my $ua = new LWP::UserAgent;
		$ua->agent("Syndication::NewsML/0.04" . $ua->agent);
		my $req = new HTTP::Request GET => substr($reference, 1);
		my $response = $ua->request($req);
		if ($response->is_success) {
			return $response->content;
		}
	}
	# document is external but we're not allowed to go outside
	# or an error occured with the retrieval
	# maybe should flag error better than this??
	return undef;
}

#
# Syndication::NewsML::Node -- superclass defining a few functions all these will need
#
package Syndication::NewsML::Node;
use Carp;
@ISA = qw( XML::DOM::Node );

sub new {
	my ($class, $node) = @_;
	my $self = bless {}, $class;

	use constant REQUIRED => 1;
	use constant IMPLIED => 2;
	use constant OPTIONAL => 3;
	use constant ZEROORMORE => 4;
	use constant ONEORMORE => 5;

	$self->{node} = $node;
	$self->{text} = undef;
	$self->{_tagname} = undef;

	# child elements we may want to access
	$self->{_singleElements} = {};
	$self->{_multiElements} = {};
	$self->{_attributes} = {};
	$self->{_hasText} = 0;

	$self->_init($node); # init will vary for different subclasses

	# call _init of ALL parent classes as well
	# thanks to Duncan Cameron <dcameron@bcs.org.uk> for suggesting how to get this to work!
	$_->($self, $node) for ( map {$_->can("_init")||()} @{"${class}::ISA"} );

	return $self;
}

sub _init { } # undef init, subclasses may want to use it

# get the contents of an element as as XML string (wrapper around XML::DOM::Node::toString)
# this *includes* the container tag of the current element.
sub getXML {
    my ($self) = @_;
    $self->{xml} = $self->{node}->toString;
}

# getChildXML is the same as the above but doesn't include the container tag.
sub getChildXML {
    my ($self) = @_;
	my $xmlstring = "";
    for my $child ($self->{node}->getChildNodes()) {
		$xmlstring .= $child->toString();
    }
	$self->{xml} = $xmlstring;
}

# get the text of the element, if any
# now includes get text of all children, including elements, recursively!
sub getText {
    my ($self, $stripwhitespace) = @_;
    croak "Can't use getText on this element" unless $self->{_hasText};
    $self->{text} = "";
    $self->{text} = getTextRecursive($self->{node}, $stripwhitespace);
}

# special "cheat" method to get ALL text in ALL child elements, ignoring any markup tags.
# can use on any element, anywhere (if there's no text, it will just return an empty string
# or all whitespace)
sub getAllText {
    my ($self, $stripwhitespace) = @_;
    $self->{text} = "";
    $self->{text} = getTextRecursive($self->{node}, $stripwhitespace);
}

sub getTextRecursive {
    my ($node, $stripwhitespace) = @_;
    my $textstring;
    for my $child ($node->getChildNodes()) {
        if ( $child->getNodeType == XML::DOM::ELEMENT_NODE ) {
            $textstring .= getTextRecursive($child, $stripwhitespace);
        } else {
            my $tmpstring = $child->getData();
            if ($stripwhitespace && ($stripwhitespace eq "strip")) {
                $tmpstring =~ s/^\s+/ /; #replace with single space -- is this ok?
                $tmpstring =~ s/\s+$/ /; #replace with single space -- is this ok?
            }
            $textstring .= $tmpstring;
        }
    }
    $textstring =~ s/\s+/ /g if $stripwhitespace; #replace with single space -- is this ok?
    return $textstring;
}

# get the tag name of this element
sub getTagName {
	my ($self) = @_;
	$self->{_tagname} = $self->{node}->getTagName;
}

# get the path up to and including this element
sub getPath {
	my ($self) = @_;
	$self->getParentPath($self->{node});
}

# get the path of this node including all parent nodes (called by getPath)
sub getParentPath {
	my ($self, $parent) = @_;
	# have to look two levels up because XML::DOM treats "#document" as a level in the tree
	return $parent->getNodeName if !defined($parent->getParentNode->getParentNode);
	return $self->getParentPath($parent->getParentNode) . "->" . $parent->getNodeName;
}

use vars '$AUTOLOAD';

# Generic routine to extract child elements from node.
# handles "getParamaterName", "getParameterNameList"  and "getParameterNameCount"
sub AUTOLOAD {
	my ($self) = @_;

	if ($AUTOLOAD =~ /DESTROY$/) {
		return;
	}

	# extract attribute name
	$AUTOLOAD =~ /.*::get(\w+)/
		or croak "No such method: $AUTOLOAD";

	print "AUTOLOAD: method is $AUTOLOAD\n" if $DEBUG;
	my $call = $1;
	if ($call =~ /(\w+)Count$/) {

		# handle getXXXCount method
		$var = $1;
		if (!$self->{_multiElements}->{$var}) {
			croak "Can't use getCount on $var";
		}
		my $method = "get".$var."List";
		$self->$method unless defined($self->{$var."Count"});
		return $self->{$var."Count"};
	} elsif ($call =~ /(\w+)List$/) {

		# handle getXXXList method for multi-element tags
		my $elem = $1;

		if (!$self->{_multiElements}->{$elem}) {
			croak "No such method: $AUTOLOAD";
		}

		# return undef if self->node doesn't exist
		return undef unless defined($self->{node});

		my $list = $self->{node}->getElementsByTagName($elem, 0);
		if (!$list && $self->{_multiElements}->{$elem} eq ONEORMORE) {
			croak "Error: required element $elem is missing";
		} 
		# set elemCount while we know what it is
		$self->{$elem."Count"} = $list->getLength;
		my @elementObjects;
		my $elementObject;
		for (my $i = 0; $i < $self->{$elem."Count"}; $i++) {
			$elementObject = "Syndication::NewsML::$elem"->new($list->item($i))
				if defined($list->item($i)); # if item is undef, push an undef to the array
			push(@elementObjects, $elementObject);
		}
		$self->{$elem} = \@elementObjects;
		return $self->{$elem};
	} elsif ($self->{_singleElements}->{$call}) {
		# return undef if self->node doesn't exist
		return undef unless defined($self->{node});

		# handle getXXX method for single-element tags
		my $element = $self->{node}->getElementsByTagName($call, 0);
		if (!$element) {
			if ($self->{_singleElements}->{$call} eq REQUIRED) {
				croak "Error: required element $call is missing";
			} else {
				return undef;
			}
		} 
		$self->{$call} = "Syndication::NewsML::$call"->new($element->item(0))
			if defined($element->item(0));
		return $self->{$call};
	} elsif ($self->{_attributes}->{$call}) {
		# return undef if self->node doesn't exist
		return undef unless defined($self->{node});
		$self->{$call} = $self->{node}->getAttributeNode($call)->getValue;
		if (!$self->{$call} && $self->{_attributes}->{$call} eq REQUIRED) {
			croak "Error: $call attribute is required";
		} 
		return $self->{$call};
	} elsif ($self->{_multiElements}->{$call}) {
		# flag error because multiElement needs to be called with "getBlahList"
		croak "$call can be a multi-element field: must call get".$call."List";
	} else {
		croak "No such method: $AUTOLOAD";
	}
}

#
# Syndication::NewsML::IdNode -- a node with Duid and/or Euid (or neither): most classes will inherit from this
#
package Syndication::NewsML::IdNode;
use Carp;
@ISA = qw( Syndication::NewsML::Node );

sub _init {
	my ($self, $node) = @_;
	$self->{_attributes}->{Duid} = IMPLIED;
	$self->{_attributes}->{Euid} = IMPLIED;
	$self->{localid} = undef;
}

sub getLocalID {
	my ($self) = @_;
	$self->{localid} = $self->getDuid || $self->getEuid;
}

# Euid is an "Element-unique Identifier". Its value must be unique among elements
# of the same element-type and having the same parent element.

# This method retrieves a *sibling method* by its Euid attribute.
# we may need a more generic Euid method later on, possibly using XPath?
sub getElementByEuid {
	my ($self, $searchEuid) = @_;

	# start search at my parent
	Syndication::NewsML::DOMUtils::findElementByAttribute($self->{node}->parentNode,
		"Euid", $searchEuid);
}

# Duid is a "Document-unique Identifier". Its value must be unique within the entire document.
# (thus there is no point starting at a particular node)
sub getElementByDuid {
	my ($self, $searchDuid) = @_;
	my $rootNode = Syndication::NewsML::DOMUtils::getRootNode($self->{node});
	Syndication::NewsML::DOMUtils::findElementByAttribute($rootNode, "Duid", $searchDuid);
}

#
# Syndication::NewsML::DateNode -- superclass defining an extra method for elements
#                             that contain ISO8601 formatted dates
#
package Syndication::NewsML::DateNode;
use Carp;

# convert ISO8601 date/time into Perl internal date/time.
# always returns perl internal date, in UTC timezone.
sub getDatePerl {
	my ($self, $timezone) = @_;
	use Time::Local;
	my $dateISO8601 = $self->getText;
	my ($yyyy, $mm, $dd, $hh, $mi, $ss, $tzsign, $tzhh, $tzmi) = ($dateISO8601 =~ qr/(\d\d\d\d)(\d\d)(\d\d)T?(\d\d)?(\d\d)?(\d\d)?([+-])?(\d\d)?(\d\d)?/);
	my $perltime = timegm($ss, $mi, $hh, $dd, $mm-1, $yyyy);
	if ($tzhh) {
		my $deltasecs = 60 * ($tzsign eq "-") ? -1*($tzhh * 60 + $tzmi) : ($tzhh * 60 + $tzmi);
		$perltime += $deltasecs;
	}
	return $perltime;
}

#
# Syndication::NewsML::CommentNode -- superclass defining what to do in elements
#                             that contain Comment sub-elements
#
package Syndication::NewsML::CommentNode;
use Carp;
@ISA = qw( Syndication::NewsML::Node );

sub _init {
	my ($self, $node) = @_;
	$self->{_multiElements}->{Comment} = ZEROORMORE;
}

#
# Syndication::NewsML::Comment -- the actual comment
#
package Syndication::NewsML::Comment;
use Carp;
@ISA = qw( Syndication::NewsML::IdNode Syndication::NewsML::XmlLangNode );

sub _init {
	my ($self, $node) = @_;
	$self->{_attributes}->{TranslationOf} = IMPLIED;
	$self->{_hasText} = 1;
}

#
# Syndication::NewsML::AssignmentNode -- superclass defining what to do in elements
#                                that contain assignment attributes
#
package Syndication::NewsML::AssignmentNode;
use Carp;
@ISA = qw( Syndication::NewsML::Node );

sub _init {
	my ($self, $node) = @_;
	$self->{_attributes}->{AssignedBy} = IMPLIED;
	$self->{_attributes}->{Importance} = IMPLIED;
	$self->{_attributes}->{Confidence} = IMPLIED;
	$self->{_attributes}->{HowPresent} = IMPLIED;
	$self->{_attributes}->{DateAndTime} = IMPLIED;
	$self->{_multiElements}->{Comment} = ZEROORMORE;
}

#
# Syndication::NewsML::PropertyNode -- superclass defining what to do in elements
#                              that contain Property sub-elements
#
package Syndication::NewsML::PropertyNode;
use Carp;
@ISA = qw( Syndication::NewsML::Node );

sub _init {
	my ($self, $node) = @_;
	$self->{_multiElements}->{Property} = ZEROORMORE;
}

#
# Syndication::NewsML::XmlLangNode -- superclass defining what to do with "xml:lang" attributes
#
package Syndication::NewsML::XmlLangNode;
use Carp;
@ISA = qw( Syndication::NewsML::Node );

sub _init {
	my ($self, $node) = @_;
}

# get xml:lang attribute (can't turn this into an AUTOLOAD because of the colon, dammit!)
sub getXmlLang {
	my ($self) = @_;
	$self->{xmlLang} = $self->{node}->getAttributeNode("xml:lang")->getValue;
}

#
# Syndication::NewsML::FormalNameNode -- superclass defining what to do with "formal name" attributes
#
package Syndication::NewsML::FormalNameNode;
use Carp;
@ISA = qw( Syndication::NewsML::Node );

sub _init {
	my ($self, $node) = @_;
	$self->{_attributes}->{FormalName} = REQUIRED;
	$self->{_attributes}->{Vocabulary} = IMPLIED;
	$self->{_attributes}->{Scheme} = IMPLIED;
}

# get the associated vocabulary for a given FormalName.
# NOTE other nodes (NewsItemId and ProviderId) also have Vocabularies for their Schemes
# but are not FormalNameNodes), I guess we should handle them in the same way??
sub resolveTopicSet {
	my ($self) = @_;
	return Syndication::NewsML::References::findReference($self, $self->getVocabulary);
}

sub resolveTopicSetDescription {
	my ($self) = @_;
	# note that this findReference routine only returns a DOM node, not a NewsML one so we
	# have to use DOM functions to traverse it.
	my $dumbnode =  Syndication::NewsML::References::findReference($self, $self->getVocabulary);
	return $dumbnode->getElementsByTagName("Comment")->[0]->getFirstChild->getNodeValue;
}

sub resolveVocabularyDescription {
	my ($self) = @_;
	# get the topicset referred in the vocabulary of this element
	my $topicset =  Syndication::NewsML::Resources::findResource($self->getVocabulary);
	# find the topic with this FormalName in the given TopicSet
	# NOT FINISHED
}

#
# Syndication::NewsML::CatalogNode -- superclass defining what to do with Catalog sub-elements
#
package Syndication::NewsML::CatalogNode;
use Carp;
@ISA = qw( Syndication::NewsML::Node );

sub _init {
	my ($self, $node) = @_;
	$self->{_singleElements}->{Catalog} = OPTIONAL;
}

#
# Syndication::NewsML::Catalog -- a container for Resource and TopicUse elements
#
package Syndication::NewsML::Catalog;
use Carp;
@ISA = qw ( Syndication::NewsML::IdNode );

sub _init {
	my ($self, $node) = @_;

	# Child elements
	$self->{_multiElements}->{Resource} = ZEROORMORE;
	$self->{_multiElements}->{TopicUse} = ZEROORMORE;
	$self->{_attributes}->{Href} = IMPLIED;
}

#
# Syndication::NewsML::TransmissionId -- 
#
package Syndication::NewsML::TransmissionId;
use Carp;
@ISA = qw( Syndication::NewsML::IdNode );

sub _init {
	my ($self, $node) = @_;
	$self->{_attributes}->{Repeat} = IMPLIED;
	$self->{_hasText} = 1;
}

#
# Syndication::NewsML::Update -- modification to an existing NewsItem
#
package Syndication::NewsML::Update;
use Carp;
@ISA = qw ( Syndication::NewsML::IdNode );

sub _init {
	my ($self, $node) = @_;

	$self->{_multiElements}{InsertBefore} = ZEROORMORE;
	$self->{_multiElements}{InsertAfter} = ZEROORMORE;
	$self->{_multiElements}{Replace} = ZEROORMORE;
	$self->{_multiElements}{Delete} = ZEROORMORE;
}

#
# Syndication::NewsML::Delete -- instruction to delete an element in a NewsItem
#
package Syndication::NewsML::Delete;
use Carp;
@ISA = qw( Syndication::NewsML::IdNode );

sub _init {
	my ($self, $node) = @_;

	$self->{_attributes}{DuidRef} = REQUIRED;
}

#
# Syndication::NewsML::DerivedFrom
#
package Syndication::NewsML::DerivedFrom;
use Carp;
@ISA = qw( Syndication::NewsML::IdNode Syndication::NewsML::CommentNode );

sub _init {
	my ($self, $node) = @_;

	$self->{_attributes}{NewsItem} = IMPLIED;
}

#
# Syndication::NewsML::AssociatedWith -- reference to associated NewsItem
#
package Syndication::NewsML::AssociatedWith;
use Carp;
@ISA = qw( Syndication::NewsML::IdNode Syndication::NewsML::CommentNode Syndication::NewsML::CommentNode );

sub _init {
	my ($self, $node) = @_;

	$self->{_attributes}{NewsItem} = IMPLIED;
}

#
# Syndication::NewsML::UsageRights -- usage rights for a NewsComponent
#
package Syndication::NewsML::UsageRights;
use Carp;
@ISA = qw( Syndication::NewsML::IdNode Syndication::NewsML::AssignmentNode );

sub _init {
	my ($self, $node) = @_;

	$self->{_singleElements}{UsageType} = OPTIONAL;
	$self->{_singleElements}{Geography} = OPTIONAL;
	$self->{_singleElements}{RightsHolder} = OPTIONAL;
	$self->{_singleElements}{Limitations} = OPTIONAL;
	$self->{_singleElements}{StartDate} = OPTIONAL;
	$self->{_singleElements}{EndDate} = OPTIONAL;
}

#
# Syndication::NewsML::UsageType -- type of usage to which the rights apply
#
package Syndication::NewsML::UsageType;
use Carp;
@ISA = qw( Syndication::NewsML::IdNode Syndication::NewsML::AssignmentNode Syndication::NewsML::XmlLangNode );

sub _init {
	my ($self, $node) = @_;
	$self->{_hasText} = 1;
}

#
# Syndication::NewsML::TopicUse -- indication of where topic is used in the document
#
package Syndication::NewsML::TopicUse;
use Carp;
@ISA = qw( Syndication::NewsML::IdNode );

sub _init {
	my ($self, $node) = @_;

	$self->{_attributes}{Topic} = REQUIRED;
	$self->{_attributes}{Context} = IMPLIED;
}

#
# Syndication::NewsML::Resource
#
package Syndication::NewsML::Resource;
use Carp;
@ISA = qw( Syndication::NewsML::IdNode );

sub _init {
	my ($self, $node) = @_;

	$self->{_singleElements}{Urn} = OPTIONAL;
	$self->{_multiElements}{Url} = ZEROORMORE;
	$self->{_multiElements}{DefaultVocabularyFor} = ZEROORMORE;
}

#
# Syndication::NewsML::Url -- a URL that can be used to locate a resource
#
package Syndication::NewsML::Url;
use Carp;
@ISA = qw( Syndication::NewsML::IdNode );

sub _init {
	my ($self, $node) = @_;
	$self->{_hasText} = 1;
}

#
# Syndication::NewsML::Urn
# A URN that provides a global identifier for a resource. This will typically (but
# not necessarily) be a NewsML URN as described in the comment to PublicIdentifier.
#
package Syndication::NewsML::Urn;
use Carp;
@ISA = qw( Syndication::NewsML::IdNode );

sub _init {
	my ($self, $node) = @_;
	$self->{_hasText} = 1;
}

#
# Syndication::NewsML::TopicSetNode -- superclass defining what to do with TopicSet fields
#
package Syndication::NewsML::TopicSetNode;
use Carp;
@ISA = qw( Syndication::NewsML::Node );

sub _init {
	my ($self, $node) = @_;
	$self->{_multiElements}{TopicSet} = ZEROORMORE;
}

#
# Syndication::NewsML::TopicSetRef -- reference to another TopicSet somewhere
#
package Syndication::NewsML::TopicSetRef;
use Carp;

@ISA = qw( Syndication::NewsML::IdNode Syndication::NewsML::CommentNode );

sub _init {
	my ($self, $node) = @_;
	$self->{_attributes}{TopicSet} = IMPLIED;
}

#
# Syndication::NewsML::DataNode -- superclass defining what to do with DataContent and Encoding fields
#
package Syndication::NewsML::DataNode;
use Carp;
@ISA = qw( Syndication::NewsML::Node );

sub _init {
	my ($self, $node) = @_;
	$self->{_singleElements}{Encoding} = OPTIONAL;
	$self->{_singleElements}{DataContent} = OPTIONAL;
}

#
# Syndication::NewsML::TopicNode -- superclass defining what to do with Topic fields
#
package Syndication::NewsML::TopicNode;
use Carp;
@ISA = qw( Syndication::NewsML::Node );

sub _init {
	my ($self, $node) = @_;
	$self->{_multiElements}{Topic} = ZEROORMORE;
}

#
# Syndication::NewsML::TopicSet -- a container for Topics
#
package Syndication::NewsML::TopicSet;
use Carp;
@ISA = qw( Syndication::NewsML::IdNode Syndication::NewsML::CommentNode Syndication::NewsML::CatalogNode
           Syndication::NewsML::TopicNode Syndication::NewsML::FormalNameNode );

sub _init {
	my ($self, $node) = @_;

	$self->{_multiElements}{TopicSetRef} = ZEROORMORE;
}

#
# Syndication::NewsML::NewsEnvelope
#

package Syndication::NewsML::NewsEnvelope;
use Carp;
@ISA = qw ( Syndication::NewsML::IdNode );

sub _init {
	my ($self, $node) = @_;
	croak "Error! A NewsML document must contain one and only one NewsEnvelope!" unless defined($node);
	$self->{_singleElements}{DateAndTime} = REQUIRED;
	$self->{_singleElements}{TransmissionId} = OPTIONAL;
	$self->{_singleElements}{SentFrom} = OPTIONAL;
	$self->{_singleElements}{SentTo} = OPTIONAL;
	$self->{_singleElements}{Priority} = OPTIONAL;
	$self->{_multiElements}{NewsService} = ZEROORMORE;
	$self->{_multiElements}{NewsProduct} = ZEROORMORE;
}

#
# Syndication::NewsML::NewsItem
#

package Syndication::NewsML::NewsItem;
use Carp;
@ISA = qw ( Syndication::NewsML::IdNode Syndication::NewsML::CatalogNode Syndication::NewsML::XmlLangNode );

sub _init {
	my ($self, $node) = @_;
	croak "Error! A NewsML document must contain at least one NewsItem!" unless defined($node);

	$self->{_singleElements}{Identification} = REQUIRED;
	$self->{_singleElements}{NewsManagement} = REQUIRED;
	$self->{_singleElements}{NewsComponent} = OPTIONAL;
	$self->{_singleElements}{TopicSet} = OPTIONAL;
	$self->{_multiElements}{Update} = ZEROORMORE;
}

# wow! a real method, not an autoload! :-)

# getType -- returns "NewsComponent", "Update", "TopicSet" or undef (none of the above)
sub getType {
	my ($self) = @_;
	return $self->{type} if $self->{type};
	# else have to check myself
	if ($self->{node}->getElementsByTagName("NewsComponent", 0)->item(0)) {
		return $self->{type} = "NewsComponent";
	} elsif ($self->{node}->getElementsByTagName("Update", 0)->item(0)) {
		return $self->{type} = "Update";
	} elsif ($self->{node}->getElementsByTagName("TopicSet", 0)->item(0)) {
		return $self->{type} = "TopicSet";
	}
	return undef;
}

#
# Syndication::NewsML::NewsComponent
#

package Syndication::NewsML::NewsComponent;
use Carp;
@ISA = qw ( Syndication::NewsML::IdNode Syndication::NewsML::CatalogNode Syndication::NewsML::CommentNode
	Syndication::NewsML::TopicSetNode Syndication::NewsML::XmlLangNode );

sub _init {
	my ($self, $node) = @_;

	$self->{_singleElements}{Role} = OPTIONAL;
	$self->{_singleElements}{NewsLines} = OPTIONAL;
	$self->{_singleElements}{AdministrativeMetadata} = OPTIONAL;
	$self->{_singleElements}{RightsMetadata} = OPTIONAL;
	$self->{_singleElements}{DescriptiveMetadata} = OPTIONAL;
	$self->{_multiElements}{Metadata} = ZEROORMORE;
	$self->{_multiElements}{BasisForChoice} = ZEROORMORE;
	$self->{_multiElements}{NewsItem} = ZEROORMORE;
	$self->{_multiElements}{NewsItemRef} = ZEROORMORE;
	$self->{_multiElements}{NewsComponent} = ZEROORMORE;
	$self->{_multiElements}{ContentItem} = ZEROORMORE;
}

# may be a nicer/more generic way of doing this, but this will do for now
sub getEssential {
	my ($self) = @_;
	my $ess = $self->{node}->getAttributeNode("Essential");
	$self->{"Essential"} = $ess ? $ess->getValue : 'no';
}

# may be a nicer/more generic way of doing this, but this will do for now
sub getEquivalentsList {
	my ($self) = @_;
	my $equiv = $self->{node}->getAttributeNode("EquivalentsList");
	$self->{"EquivalentsList"} = $equiv ? $equiv->getValue : 'no';
}

# should really do some sanity checking because a NewsComponent can't contain more than one type of
# NewsItem/NewsItemRef, NewsComponent, or ContentItem

## Metadata helpers -- so we don't have to delve too deep for not much reason

# Administrative Metadata
sub getFileName {
	my ($self) = @_;
	$self->{"FileName"} = $self->getAdministrativeMetadata->getFileName->getText;
}

sub getSystemIdentifier {
	my ($self) = @_;
	$self->{"SystemIdentifier"} = $self->getAdministrativeMetadata->getSystemIdentifier->getText;
}

# these two both return Topic objects
sub getProvider {
	my ($self) = @_;
	$self->{"Provider"} = $self->getAdministrativeMetadata->getProvider->getParty->resolveTopicRef;
}

sub getCreator {
	my ($self) = @_;
	$self->{"Creator"} = $self->getAdministrativeMetadata->getCreator->getParty->resolveTopicRef;
}

# source and contributor also exist in AdministrativeMetadata, but they both can be multiples
# and source can have an extra attr (NewsItem) so let's leave them alone for now

# Rights Metadata
# should this return text or (an array of) Topic object(s)?
sub getCopyrightHolder {
	my ($self) = @_;
	my @copyholders;
	foreach my $copyright (@{$self->getRightsMetadata->getCopyrightList}) {
		my $text = $copyright->getCopyrightHolder->getText;
		# ignore text if it's just whitespace
		push(@copyholders, $text) if $text !~ /^\s*$/;
		foreach my $origin (@{$copyright->getCopyrightHolder->getOriginList}) {
			# hard coding [0] here probably isn't good, but when do you have multiple Descriptions?
			push(@copyholders, $origin->resolveTopicRef->getDescriptionList->[0]->getText);
		}
	}
	$self->{"CopyrightHolder"} = \@copyholders;
	return wantarray ? @copyholders : join(',', @copyholders);
}

sub getCopyrightDate {
	my ($self) = @_;
	my @copydates;
	foreach my $copyright (@{$self->getRightsMetadata->getCopyrightList}) {
		my $text = $copyright->getCopyrightDate->getText;
		# ignore text if it's just whitespace
		push(@copydates, $text) if $text !~ /^\s*$/;
		foreach my $origin (@{$copyright->getCopyrightDate->getOriginList}) {
			# hard coding [0] here probably isn't good, but when do you have multiple Descriptions?
			push(@copydates, $origin->resolveTopicRef->getDescriptionList->[0]->getText);
		}
	}
	$self->{"CopyrightDate"} = \@copydates;
	return wantarray ? @copydates : join(',', @copydates);
}

# descriptive metadata
sub getLanguage {
	my ($self) = @_;
	$self->{"Language"} = $self->getDescriptiveMetadata->getLanguageList->[0]->getFormalName;
}

#
# Syndication::NewsML::NewsManagement
#

package Syndication::NewsML::NewsManagement;
use Carp;
@ISA = qw ( Syndication::NewsML::IdNode Syndication::NewsML::PropertyNode );

sub _init {
	my ($self, $node) = @_;

	$self->{_singleElements}{NewsItemType} = REQUIRED;
	$self->{_singleElements}{FirstCreated} = REQUIRED;
	$self->{_singleElements}{ThisRevisionCreated} = REQUIRED;
	$self->{_singleElements}{Status} = REQUIRED;
	$self->{_singleElements}{StatusWillChange} = OPTIONAL;
	$self->{_singleElements}{Urgency} = OPTIONAL;
	$self->{_singleElements}{RevisionHistory} = OPTIONAL;
	$self->{_multiElements}{DerivedFrom} = ZEROORMORE;
	$self->{_multiElements}{AssociatedWith} = ZEROORMORE;
	$self->{_multiElements}{Instruction} = ZEROORMORE;
}

#
# Syndication::NewsML::ContentItem
#

package Syndication::NewsML::ContentItem;
use Carp;
@ISA = qw ( Syndication::NewsML::IdNode Syndication::NewsML::CatalogNode Syndication::NewsML::CommentNode Syndication::NewsML::DataNode );
sub _init {
	my ($self, $node) = @_;
	$self->{_singleElements}{MediaType} = OPTIONAL;
	$self->{_singleElements}{Format} = OPTIONAL;
	$self->{_singleElements}{MimeType} = OPTIONAL;
	$self->{_singleElements}{Notation} = OPTIONAL;
	$self->{_singleElements}{Characteristics} = OPTIONAL;
	$self->{_attributes}{Href} = IMPLIED;
}

#
# Syndication::NewsML::RevisionHistory -- pointer to a file containing the revision history of a NewsItem
#

package Syndication::NewsML::RevisionHistory;
use Carp;
@ISA = qw ( Syndication::NewsML::IdNode );

sub _init {
	my ($self, $node) = @_;

	$self->{_attributes}{Href} = REQUIRED;
}

#
# Syndication::NewsML::TopicOccurrence -- this topic appears in the NewsComponent
#

package Syndication::NewsML::TopicOccurrence;
use Carp;
@ISA = qw ( Syndication::NewsML::IdNode Syndication::NewsML::AssignmentNode );

sub _init {
	my ($self, $node) = @_;
	$self->{_attributes}{Topic} = IMPLIED;
}

#
# Syndication::NewsML::MediaType -- media type of a ContentItem
#

package Syndication::NewsML::MediaType;
use Carp;
@ISA = qw ( Syndication::NewsML::IdNode Syndication::NewsML::FormalNameNode );

sub _init {
	my ($self, $node) = @_;
}

#
# Syndication::NewsML::Format -- format of a ContentItem
#

package Syndication::NewsML::Format;
use Carp;
@ISA = qw ( Syndication::NewsML::IdNode Syndication::NewsML::FormalNameNode );

sub _init {
	my ($self, $node) = @_;
}

#
# Syndication::NewsML::MimeType -- MIME type of a ContentItem
#

package Syndication::NewsML::MimeType;
use Carp;
@ISA = qw ( Syndication::NewsML::IdNode Syndication::NewsML::FormalNameNode );

sub _init {
	my ($self, $node) = @_;
}

#
# Syndication::NewsML::Notation -- Notation of a ContentItem
#

package Syndication::NewsML::Notation;
use Carp;
@ISA = qw ( Syndication::NewsML::IdNode Syndication::NewsML::FormalNameNode );

sub _init {
	my ($self, $node) = @_;
}

#
# Syndication::NewsML::LabelType -- a user-defined type of Label
#

package Syndication::NewsML::LabelType;
use Carp;
@ISA = qw ( Syndication::NewsML::IdNode Syndication::NewsML::FormalNameNode );

sub _init {
	my ($self, $node) = @_;
}

#
# Syndication::NewsML::Urgency -- urgency of a NewsItem
#

package Syndication::NewsML::Urgency;
use Carp;
@ISA = qw ( Syndication::NewsML::IdNode Syndication::NewsML::FormalNameNode );

sub _init {
	my ($self, $node) = @_;
}

#
# Syndication::NewsML::FutureStatus -- future status of a NewsItem
#

package Syndication::NewsML::FutureStatus;
use Carp;
@ISA = qw ( Syndication::NewsML::IdNode Syndication::NewsML::FormalNameNode );

sub _init {
	my ($self, $node) = @_;
}

#
# Syndication::NewsML::NewsItemType -- type of a NewsItem
#

package Syndication::NewsML::NewsItemType;
use Carp;
@ISA = qw ( Syndication::NewsML::IdNode Syndication::NewsML::FormalNameNode );

sub _init {
	my ($self, $node) = @_;
}

#
# Syndication::NewsML::NewsLineType -- type of a NewsLine
#

package Syndication::NewsML::NewsLineType;
use Carp;
@ISA = qw ( Syndication::NewsML::IdNode Syndication::NewsML::FormalNameNode );

sub _init {
	my ($self, $node) = @_;
}

#
# Syndication::NewsML::NewsProduct -- product to which these news items belong
#

package Syndication::NewsML::NewsProduct;
use Carp;
@ISA = qw ( Syndication::NewsML::IdNode Syndication::NewsML::FormalNameNode );

sub _init {
	my ($self, $node) = @_;
}

#
# Syndication::NewsML::NewsService -- service to which these news items belong
#

package Syndication::NewsML::NewsService;
use Carp;
@ISA = qw ( Syndication::NewsML::IdNode Syndication::NewsML::FormalNameNode );

sub _init {
	my ($self, $node) = @_;
}

#
# Syndication::NewsML::Priority -- priority notation of this NewsItem
#

package Syndication::NewsML::Priority;
use Carp;
@ISA = qw ( Syndication::NewsML::IdNode Syndication::NewsML::FormalNameNode );

sub _init {
	my ($self, $node) = @_;
}

#
# Syndication::NewsML::Role -- role this NewsComponent plays within its parent
#

package Syndication::NewsML::Role;
use Carp;
@ISA = qw ( Syndication::NewsML::IdNode Syndication::NewsML::FormalNameNode );

sub _init {
	my ($self, $node) = @_;
}

#
# Syndication::NewsML::Status -- status of a NewsItem
#

package Syndication::NewsML::Status;
use Carp;
@ISA = qw ( Syndication::NewsML::IdNode Syndication::NewsML::FormalNameNode );

sub _init {
	my ($self, $node) = @_;
}

#
# Syndication::NewsML::SubjectCode -- container for Subject codes
#

package Syndication::NewsML::SubjectCode;
use Carp;
@ISA = qw ( Syndication::NewsML::IdNode Syndication::NewsML::AssignmentNode );

sub _init {
	my ($self, $node) = @_;

	$self->{_multiElements}{Subject} = ZEROORMORE;
	$self->{_multiElements}{SubjectMatter} = ZEROORMORE;
	$self->{_multiElements}{SubjectDetail} = ZEROORMORE;
	$self->{_multiElements}{SubjectQualifier} = ZEROORMORE;
}

#
# Syndication::NewsML::Subject -- subject of a NewsItem
#

package Syndication::NewsML::Subject;
use Carp;
@ISA = qw ( Syndication::NewsML::IdNode Syndication::NewsML::FormalNameNode Syndication::NewsML::AssignmentNode );

sub _init {
	my ($self, $node) = @_;
}

#
# Syndication::NewsML::SubjectDetail -- subject detail (?) of a NewsItem
#

package Syndication::NewsML::SubjectDetail;
use Carp;
@ISA = qw ( Syndication::NewsML::IdNode Syndication::NewsML::FormalNameNode Syndication::NewsML::AssignmentNode );

sub _init {
	my ($self, $node) = @_;
}

#
# Syndication::NewsML::SubjectMatter -- subject matter (?) of a NewsItem
#

package Syndication::NewsML::SubjectMatter;
use Carp;
@ISA = qw ( Syndication::NewsML::IdNode Syndication::NewsML::FormalNameNode Syndication::NewsML::AssignmentNode );

sub _init {
	my ($self, $node) = @_;
}

#
# Syndication::NewsML::SubjectQualifier -- subject qualifier (?) of a NewsItem
#

package Syndication::NewsML::SubjectQualifier;
use Carp;
@ISA = qw ( Syndication::NewsML::IdNode Syndication::NewsML::FormalNameNode Syndication::NewsML::AssignmentNode );

sub _init {
	my ($self, $node) = @_;
}

#
# Syndication::NewsML::Relevance -- relevance of a NewsItem to a given target audience
#

package Syndication::NewsML::Relevance;
use Carp;
@ISA = qw ( Syndication::NewsML::IdNode Syndication::NewsML::FormalNameNode Syndication::NewsML::AssignmentNode );

sub _init {
	my ($self, $node) = @_;
}

#
# Syndication::NewsML::Genre -- genre of a NewsComponent
#

package Syndication::NewsML::Genre;
use Carp;
@ISA = qw ( Syndication::NewsML::IdNode Syndication::NewsML::FormalNameNode Syndication::NewsML::AssignmentNode );

sub _init {
	my ($self, $node) = @_;
}

#
# Syndication::NewsML::Language -- a language used in a content item
#

package Syndication::NewsML::Language;
use Carp;
@ISA = qw ( Syndication::NewsML::IdNode Syndication::NewsML::FormalNameNode Syndication::NewsML::AssignmentNode );

sub _init {
	my ($self, $node) = @_;
}

#
# Syndication::NewsML::Limitations -- terms and conditions of usage rights
#

package Syndication::NewsML::Limitations;
use Carp;
@ISA = qw ( Syndication::NewsML::IdNode Syndication::NewsML::XmlLangNode Syndication::NewsML::AssignmentNode Syndication::NewsML::OriginNode );

sub _init {
	my ($self, $node) = @_;
	$self->{_hasText} = 1;
}

#
# Syndication::NewsML::Characteristics -- physical characteristics of a ContentItem
#

package Syndication::NewsML::Characteristics;
use Carp;
@ISA = qw ( Syndication::NewsML::IdNode Syndication::NewsML::PropertyNode);

sub _init {
	my ($self, $node) = @_;
	$self->{_singleElements}{SizeInBytes} = OPTIONAL;
}

#
# Syndication::NewsML::SizeInBytes -- size of a ContentItem (within Characteristics)
#

package Syndication::NewsML::SizeInBytes;
use Carp;
@ISA = qw ( Syndication::NewsML::IdNode );

sub _init {
	my ($self, $node) = @_;
	$self->{_hasText} = 1;
}

#
# Syndication::NewsML::SystemIdentifier -- system ID for a NewsItem
#

package Syndication::NewsML::SystemIdentifier;
use Carp;
@ISA = qw ( Syndication::NewsML::IdNode );

sub _init {
	my ($self, $node) = @_;
	$self->{_hasText} = 1;
}

#
# Syndication::NewsML::ThisRevisionCreated -- date (and possibly time)
#

package Syndication::NewsML::ThisRevisionCreated;
use Carp;
@ISA = qw ( Syndication::NewsML::IdNode Syndication::NewsML::DateNode );

sub _init {
	my ($self, $node) = @_;
	$self->{_hasText} = 1;
}

#
# Syndication::NewsML::MetadataType -- media type of a ContentItem
#

package Syndication::NewsML::MetadataType;
use Carp;
@ISA = qw ( Syndication::NewsML::IdNode Syndication::NewsML::FormalNameNode );

sub _init {
	my ($self, $node) = @_;
	$self->{_hasText} = 1;
}

# Syndication::NewsML::Encoding -- the actual encoding
#
package Syndication::NewsML::Encoding;
use Carp;
@ISA = qw( Syndication::NewsML::IdNode Syndication::NewsML::DataNode );

sub _init {
	my ($self, $node) = @_;
	$self->{_attributes}{Notation} = REQUIRED;
}

# Syndication::NewsML::DataContent -- the actual datacontent
#
package Syndication::NewsML::DataContent;
use Carp;
@ISA = qw( Syndication::NewsML::IdNode );

sub _init {
	my ($self, $node) = @_;
	$self->{_hasText} = 1;
}

# stuff to do with parties (yeah!) (oh, not that kind of party)

# Syndication::NewsML::PartyNode -- superclass defining what to do in elements
#                           that contain Party sub-elements
#
package Syndication::NewsML::PartyNode;
use Carp;
@ISA = qw ( Syndication::NewsML::CommentNode ); # %party entity can have comment as well

sub _init {
	my ($self, $node) = @_;
	$self->{_singleElements}{Party} = ONEORMORE;
}

# Syndication::NewsML::Party -- the actual party
#
package Syndication::NewsML::Party;
use Carp;
@ISA = qw( Syndication::NewsML::IdNode Syndication::NewsML::FormalNameNode );

sub _init {
	my ($self, $node) = @_;
	$self->{_attributes}{Topic} = IMPLIED;
}

sub resolveTopicRef {
	my ($self) = @_;
	my $refnode = Syndication::NewsML::References::findReference($self, $self->getTopic, 0);
	return new Syndication::NewsML::Topic($refnode);
}

# Syndication::NewsML::Contributor
#
package Syndication::NewsML::Contributor;
use Carp;
@ISA = qw( Syndication::NewsML::IdNode Syndication::NewsML::PartyNode );

sub _init {
	my ($self, $node) = @_;
}

# Syndication::NewsML::Creator
#
package Syndication::NewsML::Creator;
use Carp;
@ISA = qw( Syndication::NewsML::IdNode Syndication::NewsML::PartyNode );

sub _init {
	my ($self, $node) = @_;
}

# Syndication::NewsML::Provider
#
package Syndication::NewsML::Provider;
use Carp;
@ISA = qw( Syndication::NewsML::IdNode Syndication::NewsML::PartyNode );

sub _init {
	my ($self, $node) = @_;
}

# Syndication::NewsML::SentFrom
#
package Syndication::NewsML::SentFrom;
use Carp;
@ISA = qw( Syndication::NewsML::IdNode Syndication::NewsML::PartyNode );

sub _init {
	my ($self, $node) = @_;
}

# Syndication::NewsML::SentTo
#
package Syndication::NewsML::SentTo;
use Carp;
@ISA = qw( Syndication::NewsML::IdNode Syndication::NewsML::PartyNode );

sub _init {
	my ($self, $node) = @_;
}

# Syndication::NewsML::Source
#
package Syndication::NewsML::Source;
use Carp;
@ISA = qw( Syndication::NewsML::IdNode Syndication::NewsML::PartyNode );

sub _init {
	my ($self, $node) = @_;
	$self->{_attributes}{NewsItem} = IMPLIED;
}

# Syndication::NewsML::Topic

# Syndication::NewsML::Topic -- "information about a thing" according to the DTD ;-)
#
package Syndication::NewsML::Topic;
use Carp;
@ISA = qw( Syndication::NewsML::IdNode Syndication::NewsML::CommentNode Syndication::NewsML::CatalogNode Syndication::NewsML::PropertyNode);

sub _init {
	my ($self, $node) = @_;
	$self->{_multiElements}{TopicType} = ONEORMORE;
	$self->{_multiElements}{Description} = ZEROORMORE;
	$self->{_multiElements}{FormalName} = ZEROORMORE;
	$self->{_attributes}{Details} = IMPLIED;
}

# Syndication::NewsML::TopicType -- type of a topic (amazing huh?)
#

package Syndication::NewsML::TopicType;
use Carp;
@ISA = qw ( Syndication::NewsML::IdNode Syndication::NewsML::FormalNameNode );

sub _init {
	my ($self, $node) = @_;
}

# Syndication::NewsML::Description -- formal name as an element, not an attribute, for Topics
#

package Syndication::NewsML::Description;
use Carp;
@ISA = qw ( Syndication::NewsML::IdNode Syndication::NewsML::XmlLangNode );

sub _init {
	my ($self, $node) = @_;
	$self->{_attributes}{Variant} = IMPLIED;
	$self->{_hasText} = 1;
}

# Syndication::NewsML::FormalName -- formal name as an element, not an attribute, for Topics
#
package Syndication::NewsML::FormalName;
use Carp;
@ISA = qw( Syndication::NewsML::IdNode );

sub _init {
	my ($self, $node) = @_;
	$self->{_attributes}{Scheme} = IMPLIED;
	$self->{_hasText} = 1;
}

# Syndication::NewsML::DefaultVocabularyFor
#
package Syndication::NewsML::DefaultVocabularyFor;
use Carp;
@ISA = qw( Syndication::NewsML::IdNode );

sub _init {
	my ($self, $node) = @_;
	$self->{_attributes}{Context} = REQUIRED;
	$self->{_attributes}{Scheme} = IMPLIED;
}

# Syndication::NewsML::NameLabel -- label to help users identify a NewsItem
#
package Syndication::NewsML::NameLabel;
use Carp;
@ISA = qw( Syndication::NewsML::IdNode );

sub _init {
	my ($self, $node) = @_;
	$self->{_hasText} = 1;
}

# Syndication::NewsML::NewsItemId -- identifier for a NewsItem (combination of NewsItemId and DateId must
#                            be unique amongst all NewsItems from this provider)
#
package Syndication::NewsML::NewsItemId;
use Carp;
# subclass Node instead of IdNode as this doesn't have a %localid
@ISA = qw( Syndication::NewsML::Node );

sub _init {
	my ($self, $node) = @_;
	$self->{_attributes}{Vocabulary} = IMPLIED;
	$self->{_attributes}{Scheme} = IMPLIED;
	$self->{_hasText} = 1;
}

# Syndication::NewsML::NewsItemRef -- reference to another NewsItem somewhere
#
package Syndication::NewsML::NewsItemRef;
use Carp;

# actually this may need more than just CommentNode as it can have zero or more comments...
@ISA = qw( Syndication::NewsML::IdNode Syndication::NewsML::CommentNode );

sub _init {
	my ($self, $node) = @_;
	$self->{_attributes}{NewsItem} = IMPLIED;
}

# Syndication::NewsML::NewsLine -- line of arbitrary text
#
package Syndication::NewsML::NewsLine;
use Carp;

@ISA = qw( Syndication::NewsML::IdNode );

sub _init {
	my ($self, $node) = @_;
	$self->{_singleElements}{NewsLineType} = REQUIRED;
	$self->{_multiElements}{NewsLineText} = ONEORMORE;
}

# Syndication::NewsML::NewsLines -- container for lines of news in a NewsComponent
#
package Syndication::NewsML::NewsLines;
use Carp;

@ISA = qw( Syndication::NewsML::IdNode );

sub _init {
	my ($self, $node) = @_;
	$self->{_multiElements}{HeadLine} = ZEROORMORE;
	$self->{_multiElements}{SubHeadLine} = ZEROORMORE;
	$self->{_multiElements}{ByLine} = ZEROORMORE;
	$self->{_multiElements}{DateLine} = ZEROORMORE;
	$self->{_multiElements}{CreditLine} = ZEROORMORE;
	$self->{_multiElements}{CopyrightLine} = ZEROORMORE;
	$self->{_multiElements}{RightsLine} = ZEROORMORE;
	$self->{_multiElements}{SeriesLine} = ZEROORMORE;
	$self->{_multiElements}{SlugLine} = ZEROORMORE;
	$self->{_multiElements}{KeywordLine} = ZEROORMORE;
	$self->{_multiElements}{NewsLine} = ZEROORMORE;
}

# Syndication::NewsML::AdministrativeMetadata -- the "provenance" of a NewsComponent
#
package Syndication::NewsML::AdministrativeMetadata;
use Carp;

@ISA = qw( Syndication::NewsML::IdNode Syndication::NewsML::CatalogNode Syndication::NewsML::PropertyNode );

sub _init {
	my ($self, $node) = @_;
	$self->{_singleElements}{FileName} = OPTIONAL;
	$self->{_singleElements}{SystemIdentifier} = OPTIONAL;
	$self->{_singleElements}{Provider} = OPTIONAL;
	$self->{_singleElements}{Creator} = OPTIONAL;
	$self->{_multiElements}{Source} = ZEROORMORE;
	$self->{_multiElements}{Contributor} = ZEROORMORE;
}

# Syndication::NewsML::DescriptiveMetadata -- describes the content of a NewsComponent
#
package Syndication::NewsML::DescriptiveMetadata;
use Carp;

@ISA = qw( Syndication::NewsML::IdNode Syndication::NewsML::CatalogNode Syndication::NewsML::PropertyNode Syndication::NewsML::AssignmentNode );

sub _init {
	my ($self, $node) = @_;
	$self->{_singleElements}{Genre} = OPTIONAL;
	$self->{_multiElements}{Language} = ZEROORMORE;
	$self->{_multiElements}{SubjectCode} = ZEROORMORE;
	$self->{_multiElements}{OfInterestTo} = ZEROORMORE;
	$self->{_multiElements}{TopicOccurrence} = ZEROORMORE;
}

# Syndication::NewsML::Metadata -- user-defined type of metadata
#
package Syndication::NewsML::Metadata;
use Carp;

@ISA = qw( Syndication::NewsML::IdNode Syndication::NewsML::CatalogNode Syndication::NewsML::PropertyNode );

sub _init {
	my ($self, $node) = @_;
	$self->{_singleElements}{MetadataType} = REQUIRED;
}

# Syndication::NewsML::RightsMetadata -- user-defined type of metadata
#
package Syndication::NewsML::RightsMetadata;
use Carp;

@ISA = qw( Syndication::NewsML::IdNode Syndication::NewsML::CatalogNode Syndication::NewsML::PropertyNode Syndication::NewsML::AssignmentNode );

sub _init {
	my ($self, $node) = @_;
	$self->{_multiElements}{Copyright} = ZEROORMORE;
	$self->{_multiElements}{UsageRights} = ZEROORMORE;
}

# Syndication::NewsML::BasisForChoice -- XPATH info to help choose between ContentItems
#
package Syndication::NewsML::BasisForChoice;
use Carp;
@ISA = qw( Syndication::NewsML::IdNode );

sub _init {
	my ($self, $node) = @_;
	$self->{_attributes}{Rank} = IMPLIED;
	$self->{_hasText} = 1;
}

# Syndication::NewsML::OriginNode -- superclass for handling weird Origin things
#
package Syndication::NewsML::OriginNode;
use Carp;
@ISA = qw( Syndication::NewsML::Node );

sub _init {
	my ($self, $node) = @_;
	$self->{_multiElements}{Origin} = ZEROORMORE;
}

# Syndication::NewsML::Origin
#
package Syndication::NewsML::Origin;
use Carp;
@ISA = qw( Syndication::NewsML::IdNode Syndication::NewsML::OriginNode Syndication::NewsML::AssignmentNode );

sub _init {
	my ($self, $node) = @_;
	$self->{_attributes}{Href} = IMPLIED;
}

sub resolveTopicRef {
	my ($self) = @_;
	my $refnode = Syndication::NewsML::References::findReference($self, $self->getHref, 0);
	return new Syndication::NewsML::Topic($refnode);
}

# Syndication::NewsML::ByLine -- author/creator in natural language
#
package Syndication::NewsML::ByLine;
use Carp;
@ISA = qw( Syndication::NewsML::IdNode Syndication::NewsML::XmlLangNode Syndication::NewsML::OriginNode );

sub _init {
	my ($self, $node) = @_;
	$self->{_hasText} = 1;
}

# Syndication::NewsML::Copyright
#
package Syndication::NewsML::Copyright;
use Carp;
@ISA = qw( Syndication::NewsML::IdNode Syndication::NewsML::CommentNode Syndication::NewsML::AssignmentNode );

sub _init {
	my ($self, $node) = @_;
	$self->{_singleElements}{CopyrightHolder} = REQUIRED;
	$self->{_singleElements}{CopyrightDate} = REQUIRED;
}

# Syndication::NewsML::CopyrightDate
#
package Syndication::NewsML::CopyrightDate;
use Carp;
@ISA = qw( Syndication::NewsML::IdNode Syndication::NewsML::XmlLangNode Syndication::NewsML::OriginNode );

sub _init {
	my ($self, $node) = @_;
	$self->{_hasText} = 1;
}

# Syndication::NewsML::CopyrightHolder
#
package Syndication::NewsML::CopyrightHolder;
use Carp;
@ISA = qw( Syndication::NewsML::IdNode Syndication::NewsML::XmlLangNode Syndication::NewsML::OriginNode );

sub _init {
	my ($self, $node) = @_;
	$self->{_hasText} = 1;
}

# Syndication::NewsML::CopyrightLine
#
package Syndication::NewsML::CopyrightLine;
use Carp;
@ISA = qw( Syndication::NewsML::IdNode Syndication::NewsML::XmlLangNode Syndication::NewsML::OriginNode );

sub _init {
	my ($self, $node) = @_;
	$self->{_hasText} = 1;
}

# Syndication::NewsML::CreditLine
#
package Syndication::NewsML::CreditLine;
use Carp;
@ISA = qw( Syndication::NewsML::IdNode Syndication::NewsML::XmlLangNode Syndication::NewsML::OriginNode );

sub _init {
	my ($self, $node) = @_;
	$self->{_hasText} = 1;
}

# Syndication::NewsML::DateAndTime
#
package Syndication::NewsML::DateAndTime;
use Carp;
@ISA = qw( Syndication::NewsML::IdNode Syndication::NewsML::DateNode );

sub _init {
	my ($self, $node) = @_;
	$self->{_hasText} = 1;
}

# Syndication::NewsML::DateId
#
package Syndication::NewsML::DateId;
use Carp;
@ISA = qw( Syndication::NewsML::IdNode Syndication::NewsML::DateNode );

sub _init {
	my ($self, $node) = @_;
	$self->{_hasText} = 1;
}

# Syndication::NewsML::DateLabel
#
package Syndication::NewsML::DateLabel;
use Carp;
@ISA = qw( Syndication::NewsML::IdNode );

sub _init {
	my ($self, $node) = @_;
	$self->{_hasText} = 1;
}

# Syndication::NewsML::DateLine
#
package Syndication::NewsML::DateLine;
use Carp;
@ISA = qw( Syndication::NewsML::IdNode Syndication::NewsML::XmlLangNode Syndication::NewsML::OriginNode );

sub _init {
	my ($self, $node) = @_;
	$self->{_hasText} = 1;
}

# Syndication::NewsML::EndDate
#
package Syndication::NewsML::EndDate;
use Carp;
@ISA = qw( Syndication::NewsML::IdNode Syndication::NewsML::XmlLangNode Syndication::NewsML::AssignmentNode Syndication::NewsML::OriginNode );

sub _init {
	my ($self, $node) = @_;
	$self->{_hasText} = 1;
}

# Syndication::NewsML::StartDate
#
package Syndication::NewsML::StartDate;
use Carp;
@ISA = qw( Syndication::NewsML::IdNode Syndication::NewsML::XmlLangNode Syndication::NewsML::AssignmentNode Syndication::NewsML::OriginNode );

sub _init {
	my ($self, $node) = @_;
	$self->{_hasText} = 1;
}

# Syndication::NewsML::FileName
#
package Syndication::NewsML::FileName;
use Carp;
@ISA = qw( Syndication::NewsML::IdNode );

sub _init {
	my ($self, $node) = @_;
	$self->{_hasText} = 1;
}

# Syndication::NewsML::FirstCreated
#
package Syndication::NewsML::FirstCreated;
use Carp;
@ISA = qw( Syndication::NewsML::IdNode Syndication::NewsML::DateNode );

sub _init {
	my ($self, $node) = @_;
	$self->{_hasText} = 1;
}

# Syndication::NewsML::Geography
#
package Syndication::NewsML::Geography;
use Carp;
@ISA = qw( Syndication::NewsML::IdNode Syndication::NewsML::XmlLangNode Syndication::NewsML::AssignmentNode Syndication::NewsML::OriginNode );

sub _init {
	my ($self, $node) = @_;
	$self->{_hasText} = 1;
}

# Syndication::NewsML::HeadLine
#
package Syndication::NewsML::HeadLine;
use Carp;
@ISA = qw( Syndication::NewsML::IdNode Syndication::NewsML::XmlLangNode Syndication::NewsML::OriginNode );

sub _init {
	my ($self, $node) = @_;
	$self->{_hasText} = 1;
}

# Syndication::NewsML::KeywordLine
#
package Syndication::NewsML::KeywordLine;
use Carp;
@ISA = qw( Syndication::NewsML::IdNode Syndication::NewsML::XmlLangNode Syndication::NewsML::OriginNode );

sub _init {
	my ($self, $node) = @_;
	$self->{_hasText} = 1;
}

# Syndication::NewsML::NewsLineText
#
package Syndication::NewsML::NewsLineText;
use Carp;
@ISA = qw( Syndication::NewsML::IdNode Syndication::NewsML::XmlLangNode Syndication::NewsML::OriginNode );

sub _init {
	my ($self, $node) = @_;
	$self->{_hasText} = 1;
}

# Syndication::NewsML::RightsHolder
#
package Syndication::NewsML::RightsHolder;
use Carp;
@ISA = qw( Syndication::NewsML::IdNode Syndication::NewsML::XmlLangNode Syndication::NewsML::AssignmentNode Syndication::NewsML::OriginNode );

sub _init {
	my ($self, $node) = @_;
	$self->{_hasText} = 1;
}

# Syndication::NewsML::RightsLine
#
package Syndication::NewsML::RightsLine;
use Carp;
@ISA = qw( Syndication::NewsML::IdNode Syndication::NewsML::XmlLangNode Syndication::NewsML::OriginNode );

sub _init {
	my ($self, $node) = @_;
	$self->{_hasText} = 1;
}

# Syndication::NewsML::SeriesLine
#
package Syndication::NewsML::SeriesLine;
use Carp;
@ISA = qw( Syndication::NewsML::IdNode Syndication::NewsML::XmlLangNode Syndication::NewsML::OriginNode );

sub _init {
	my ($self, $node) = @_;
	$self->{_hasText} = 1;
}

# Syndication::NewsML::SlugLine
#
package Syndication::NewsML::SlugLine;
use Carp;
@ISA = qw( Syndication::NewsML::IdNode Syndication::NewsML::XmlLangNode Syndication::NewsML::OriginNode );

sub _init {
	my ($self, $node) = @_;
	$self->{_hasText} = 1;
}

# Syndication::NewsML::SubHeadLine
#
package Syndication::NewsML::SubHeadLine;
use Carp;
@ISA = qw( Syndication::NewsML::IdNode Syndication::NewsML::XmlLangNode Syndication::NewsML::OriginNode );

sub _init {
	my ($self, $node) = @_;
	$self->{_hasText} = 1;
}

# Syndication::NewsML::Label
#
package Syndication::NewsML::Label;
use Carp;
@ISA = qw( Syndication::NewsML::IdNode );

sub _init {
	my ($self, $node) = @_;
	$self->{_singleElements}{LabelType} = REQUIRED;
	$self->{_singleElements}{LabelText} = REQUIRED;
}

# Syndication::NewsML::LabelText
#
package Syndication::NewsML::LabelText;
use Carp;
@ISA = qw( Syndication::NewsML::IdNode );

sub _init {
	my ($self, $node) = @_;
	$self->{_hasText} = 1;
}

# Syndication::NewsML::ProviderId -- should be a domain name apparently
#
package Syndication::NewsML::ProviderId;
use Carp;
@ISA = qw( Syndication::NewsML::IdNode );

sub _init {
	my ($self, $node) = @_;
	$self->{_attributes}{Vocabulary} = IMPLIED;
	$self->{_hasText} = 1;
}

# Syndication::NewsML::PublicIdentifier
#
package Syndication::NewsML::PublicIdentifier;
use Carp;
@ISA = qw( Syndication::NewsML::IdNode );

sub _init {
	my ($self, $node) = @_;
	$self->{_hasText} = 1;
}

# Syndication::NewsML::NewsIdentifier
#
package Syndication::NewsML::NewsIdentifier;
use Carp;
@ISA = qw( Syndication::NewsML::IdNode );

sub _init {
	my ($self, $node) = @_;
	$self->{_singleElements}{ProviderId} = REQUIRED;
	$self->{_singleElements}{DateId} = REQUIRED;
	$self->{_singleElements}{NewsItemId} = REQUIRED;
	$self->{_singleElements}{RevisionId} = REQUIRED;
	$self->{_singleElements}{PublicIdentifier} = REQUIRED;
	$self->{_hasText} = 1;
}

# Syndication::NewsML::RevisionId -- integer representing division
#
package Syndication::NewsML::RevisionId;
use Carp;
@ISA = qw( Syndication::NewsML::IdNode );

sub _init {
	my ($self, $node) = @_;
	$self->{_attributes}{PreviousRevision} = REQUIRED;
	$self->{_attributes}{Update} = REQUIRED;
	$self->{_hasText} = 1;
}

# Syndication::NewsML::InsertAfter -- content to insert after a designated element
#
package Syndication::NewsML::InsertAfter;
use Carp;
@ISA = qw( Syndication::NewsML::IdNode );

sub _init {
	my ($self, $node) = @_;
	$self->{_attributes}{DuidRef} = REQUIRED;
	$self->{_hasText} = 1;
}

# Syndication::NewsML::InsertBefore -- content to insert before a designated element
#
package Syndication::NewsML::InsertBefore;
use Carp;
@ISA = qw( Syndication::NewsML::IdNode );

sub _init {
	my ($self, $node) = @_;
	$self->{_attributes}{DuidRef} = REQUIRED;
	$self->{_hasText} = 1;
}

# Syndication::NewsML::Replace -- content to replace a designated element
#
package Syndication::NewsML::Replace;
use Carp;
@ISA = qw( Syndication::NewsML::IdNode );

sub _init {
	my ($self, $node) = @_;
	$self->{_attributes}{DuidRef} = REQUIRED;
	$self->{_hasText} = 1;
}

# Syndication::NewsML::Property
#
package Syndication::NewsML::Property;
use Carp;
@ISA = qw( Syndication::NewsML::IdNode Syndication::NewsML::PropertyNode Syndication::NewsML::FormalNameNode Syndication::NewsML::AssignmentNode );

sub _init {
	my ($self, $node) = @_;
	$self->{_attributes}{Value} = IMPLIED;
	$self->{_attributes}{ValueRef} = IMPLIED;
	$self->{_attributes}{AllowedValues} = IMPLIED;
}

# Syndication::NewsML::OfInterestTo
#
package Syndication::NewsML::OfInterestTo;
use Carp;
@ISA = qw( Syndication::NewsML::IdNode Syndication::NewsML::FormalNameNode Syndication::NewsML::AssignmentNode );

sub _init {
	my ($self, $node) = @_;
	$self->{_singleElements}{Relevance} = OPTIONAL;
}

# Syndication::NewsML::RevisionStatus
#
package Syndication::NewsML::RevisionStatus;
use Carp;
@ISA = qw( Syndication::NewsML::IdNode );

sub _init {
	my ($self, $node) = @_;
	$self->{_singleElements}{Status} = REQUIRED;
	$self->{_attributes}{Revision} = IMPLIED;
}

# Syndication::NewsML::StatusWillChange
#
package Syndication::NewsML::StatusWillChange;
use Carp;
@ISA = qw( Syndication::NewsML::IdNode );

sub _init {
	my ($self, $node) = @_;
	$self->{_singleElements}{FutureStatus} = REQUIRED;
	$self->{_singleElements}{DateAndTime} = REQUIRED;
}

# Syndication::NewsML::Identification
#
package Syndication::NewsML::Identification;
use Carp;
@ISA = qw( Syndication::NewsML::IdNode );

sub _init {
	my ($self, $node) = @_;
	$self->{_singleElements}{NewsIdentifier} = REQUIRED;
	$self->{_singleElements}{NameLabel} = OPTIONAL;
	$self->{_singleElements}{DateLabel} = OPTIONAL;
	$self->{_multiElements}{Label} = ZEROORMORE;
}

#
# Syndication::NewsML::Instruction
#
package Syndication::NewsML::Instruction;
use Carp;
@ISA = qw( Syndication::NewsML::IdNode Syndication::NewsML::FormalNameNode );

sub _init {
	my ($self, $node) = @_;
	$self->{_multiElements}{RevisionStatus} = ZEROORMORE;
}
