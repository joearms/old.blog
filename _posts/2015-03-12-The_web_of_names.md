---
layout: post
title: The web of names, hashes and UUIDs
tags: web names hashes uuid sha1 erlang
year: 2015
month: 3
day: 12
published: true
---

Subtitle: A step towards cleaning up the mess we're in.

How do we keep track of documents that change?

This is a sub-problem to the more general problem ``how do we keep
track of things'' which in it's turn involves solving the problem
``how do we name things'' and this is a sub problem to the problem of
cleaning up [the mess we're
in](https://www.youtube.com/watch?v=lKXe3HUG2l4).

I'll start with the problem of naming things.

# Naming things

Naming things is one of the toughest problems in computer science and
philosophy.  As soon as we name something there is an implied context
- take away the context, or use the name in a different context and we
are lost.

My son is called Thomas - in the context of our family ``Thomas'' means
``my son'' and uniquely identifies him. In the context of my workplace
Thomas means one of 646 different people (and yes I ran a search to
see). Google comes up with 1.58 Billion Thomases - so the name Thomas
is both very useful (in the context of my family) and useless (in the
wider context).

How could we give a precise name to Thomas? - that's easy (in theory)
- we scan Thomas's Genome and compute the SHA1 checksum of the genome
(and we'd need some fancy error correction algorithm since I guess two
scans of the genome would not produce bit identical results).

# Naming thing by their content

Inside computers things are stored in files, this raises two
very tricky problems:

* How should we name the files?
* In which directory should the files be stored?

The easiest way to name a file is by its content.
All we do is compute the [SHA1](http://en.wikipedia.org/wiki/SHA-1)
checksum of the content of the file - and bingo we have
a unique filename. Then we can stick all the files in the same
directory. This solves both our problems.

We've also created a new and totally different problem - ``How do I
find the filename of a file that contains some data I'm interested
in''.

The latter problem (finding the file) is solved by indexing the
contents of the file The file can contain hashtags of certain names
that have meaning, and we can search for these.

Once we have the SHA1 name of a file we can safely request this file
from any server and don't need to bother with security.

Fpr example, if I request a file with SHA1
cf23df2207d99a74fbe169e3eaa035e623b65d94 from a server then I can
check that the data I got back was correct by computing its SHA1
checksum.

This mechanism is immune to a [Man in the Middle
Attack](http://en.wikipedia.org/wiki/Man-in-the-middle_attack) so
regular unencrypted socket transport can be used.  If a man in the
middle changes the content of the file then the SHA1 checksums will
not tally and the client requesting the data will know that the data
is corrupt and cannot be trusted.

Given that we have a SHA1 checksum of some data, how can we find the
address of a server that hosts this file. Well this problem was solved
a long time ago. Just put the data into a
[DHT](http://en.wikipedia.org/wiki/Distributed_hash_table)
(Distributed Hash Table) such as
[Kademlia](http://en.wikipedia.org/wiki/Kademlia) or
[Chord](http://en.wikipedia.org/wiki/Chord_(peer-to-peer\))
and let it work its magic. Bit torrent
clients and servers have routinely using this technique for the last
ten years or more.

# The Web of Hashes

To enable a network for serving immutable content I'd like to see the
emergence of what I have called ``the web of hashes'' so we'd see web
address like:

> sha1://cf23df2207d99a74fbe169e3eaa035e623b65d94

Instead of addresses like:

> http://some.host.some.country/some.name


# What about content that Changes?

SHA1 checksums are fine for content that is immutable (doesn't change)
- but what about a file whose content changes with time?

To solve this I propose adding 
[UUIDs](http://en.wikipedia.org/wiki/Universally_unique_identifier) to
files.  UUIDs can be generated locally without using a centralized
server.

So Erlang files might start something like:
     
     -module(xyz).
     -uuid("de305d54-75b4-431b-adb2-eb6b9e546013")
   
  and HTML: 

     <meta name="uuid" content="de305d54-75b4-431b-adb2-eb6b9e546013">

Once a file has a UUID it can be renamed, edited, cloned etc. As
long as you don't remove the UUID then we can track the location
with a DHT. After a while the contents of files with the same UUID
will diverge and at that time we can think of changing the UUIDs

I think this would be a rather good mechanism. Imagine the following:

+ We create file and make sure it contains a UUID.
+ The file gets modified, or renamed. At this stage their is still only one file
  with this UUID.
+ We copy the file to a new file. Now we have two files with the same UUID. Each can be
  independently edited renamed or copied. By keeping a database of UUID to filename
  mappings we can easily track down all modifications of the file. At a certain stage
  we might look at all the files containing the same UUID and decide that some of the
  modifications are so large as to warrent a name change, in this case we add a new UUID
  to the
  file - and we can also add a `parent:UUID` tag in the file saying that this file
  was derived from an earlier file with this UUID. Using the parent tag we'd be able
  to track the evolution of a file.
+ If we send the file to a friend or post it to the net, copies, renamings
  and editing will
  happen - but we will still be able to track these if the original UUID is not changed.

# The Web of UUIDs

Now we create the web of UUIDs. As for the web of hashes I'd like to
see a web of UUIDs, so we could request data for a resource with identifier:

> uuid://de305d54-75b4-431b-adb2-eb6b9e546013 

This time we might get many different replies, since there might be
multiple copies of the file.  What should a request like the above
reply? Possible a list of SHA1's -- I'm not sure here.

# The Web of Names

Today we have a web of names. Things like

   http://some.name.of.a.server.someplace/some.name.of.a.file

But we don't have either a web of hashes or a web of UUIDs

I think we need all three.

# Why three webs?

+ The web of names is convenient and easy to use
+ The web of UUIDs allows us to track content that changes with time
+ The web of hashes (SHA1) allows total precision in managing content

# What's the point of all of this?

You might ask where I'm going with this? - If you've watched my lecture
[the mess we're
in](https://www.youtube.com/watch?v=lKXe3HUG2l4).
you'll get the answer. The Web and computer software is in a total mess.
It has evolved faster than our ability to understand what we are doing.

> Once there was not enough software, then it was about right, and now there's too much

It's easy to understand how the total amount of software in existence
increases, this is a law of nature - entropy always increases. The
amount of software increases because files get copied, edited
cloned and modified.

We need mechanisms to reverse this process. By adding UUIDs to files
we can track down all copies and modification to a file, and possible
__reduce__ the number of files by throwing away bad modifications
that make no sense and by keeping the best of all the modifications.

This is part of my __reversing entropy__ plan - that hopefully will
clear up the mess we're in.
 

 