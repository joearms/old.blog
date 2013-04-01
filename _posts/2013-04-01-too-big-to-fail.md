---
layout: post
title: Too big to fail
tags: banks failure
year: 2013
month: 4
day: 1
published: true
---

<img src="http://learnyousomeerlang.com/static/img/rube.png"/>	

Image from <a href="http://nostarch.com/erlang">Learn You Some Erlang for Great Good!</a>
     

I'm proud to announce a new extension to Erlang.
After twelve years of discussion, we've added the <b>too_big_to_fail</b>
flag to processes.

Here a usage example:

{% highlight erlang %}
test() ->
    spawn(fun() ->
		  process_flag(too_big_to_fail, true)
	  end).
{% endhighlight %}
<p></p>

Semantics
=========

Too big to fail processes behave like regular processes until they get
too big and memory congestion occurs.  If a memory allocation error is
triggered when a <b>too_big_to_fail</b> process needs more memory, then a
random smaller process is killed, and the system reattempts memory
allocation for the <b>too_big_to_fail</b> process.

An interesting situation can occur if the too big to fail process
has killed all other processes and still cannot get enough memory.

In this case the node running the process tries to memory steal from other nodes.

Memory Stealing
===============

If a too big to fail process runs out of memory after killing all the
smaller processes on the current node, it can spawn a clone of itself on
other nodes and, these can allocate additional memory.

This is what happens:

* A <b>too_big_to_fail_process</b> runs out of memory on node one
* A ``helper'' process is allocated on node two
* The helper node allocates as much memory as it can
* node one reduces it's memory footprint, using the additional memory made by stealing memory from node two.
* This if recursively repeated. So If node two runs out of memory, it tries to steal
memory from node three, and so.

If all <b>too_big_to_fail_nodes</b> run out of memory, then they request
additional memory from the memory allocator of last resort.

The Memory Allocator of Last Resort
===================================

If all the <b>too_big_to_fail</b> processes have failed, and run out of memory
then they can make a request to the ``Memory Allacator of Last Resort.''. The
memory allocator of last resort has the ability to print new memory. Exactly how it
does this and what the consequences are is little understood. Fortunately if new memory
is consumed slower than it is printed everything returns to normal. But if
new memory is consumed faster that it can be printed then the system will crash and
a reboot is required.

Release Schedule
================

<b>too_big_to_fail_processes</b> will be released in OTP-R18B. The
semantics of the memory allocator of last resort are still being
studied and will be the subject of a forthcoming EEP.

