---
layout: post
title: Red and Green Callbacks
tags: io erlang javascript
year: 2013
month: 3
day: 26
published: true
summary: testing my blog
---

There are two types of callback, I'll call them ``Red'' and
``Green''. Red callbacks are evil and interrupt the flow of control in
your program. Green callbacks are beautiful and do not interrupt the
flow of control in your program.  Javascript has red callbacks and
Erlang has green callbacks.

To explain this I have to back-off and talk about I/O.

Concurrent I/O in Erlang
========================

How does Erlang do concurrent I/O? It's easy.
Suppose we have three processes **A**, **B** and **C** executing in 
parallel. I'll represent this as follows:


       A  ||  B ||  C

This is supposed to represent three processes **A**, **B** and **C**
which are doing I/O operations.

Process A does something like:

     A: --- read --- read --- write --- read --- write ---

B does:

     B: ---- write --- write --- read --- read --- write ---

and so on. The dashed lines represent sequential computations. The
Erlang code for process **A** is exactly like the diagram:

{% highlight erlang %}
a() ->
    ...
    X = read(),
    ...
    Y = read(), 
    ...  
    write(...),
    ...
{% endhighlight %}

And likewise for **B** and **C**.

There is actually no **read()** statement in Erlang. Erlang has
<a href="http://www.erlang.org/course/concurrent_programming.html#select">selective receive</a> 
 so we actually define **read()** something like this:

{% highlight erlang %}
read() ->
    receive
        Pattern1 -> 	
           ...
        Pattern2 -> 
           ...
    end.
{% endhighlight %}

The details of exactly how selective receive works are not important
to this discussion, so I'll hop over the details for now.

The important point to note is that in the code for the process
**A** we write:

{% highlight erlang %}
    ...
    X = read()
    ...
{% endhighlight %}

and that our process suspends (or you might say _blocks_) at the read
function until the read request is satisfied.  So our code ``looks
like'' we're doing a _synchronous blocking read_.

_Looks like_ was in quotes, because it's not actually a blocking read,
it's really an asynchronous read which does not block any other Erlang
processes.

This is really nice, since the beginning of time, programs have waited
for read requests to be satisfied, and then having read their data,
move on.

Enter Concurrency
=================

Erlang I/O is magic with a purpose. When we have two parallel processes,
a **read()** request in process **A** will apparently block in **A**,
but this will not effect the code in any other parallel process (say **B**).

So **A** and **B** can both be written _as if they were sequential processes_.

Now suppose we didn't have a decent underlying concurrency model.
Suppose everything we did had to be squeezed into a single thread of
execution. Suppose we did a read (which blocked) while there was other
stuff waiting to be done. Oh dear, our programming model would be clearer,
but we'd use up valuable CPU cycles.

Now in some languages (and I'm looking at you Javascript) there are no
processes, and no threads. This is not strictly accurate, there is one
thread and everything has to be multiplexed into this thread. To write
code with reads in Javascript you have to use red callbacks and invent
you own concurrency abstractions.


Red Callbacks
=============

In Javascript you really don't want to do a blocking synchronous read
in the main thread (and remember there is only one thread), so instead
you have to setup a callback which triggers when the read completes. I call this
a _red callback_. You write something like this:


{% highlight javascript %}
    var done  = function(x){ ... do something with x ..};
    var error = function(x){ .... x ...}
    read(Something, {onSuccess:done, onError:error});
    ...
    ... more code ...
{% endhighlight %}

Code like this melts my brain.

When the program is executing somewhere inside ``_more code_'', 
the read completes, and I'm time warped back in
time to the code in **done**, then back to wherever I came from in ``_more
code_'' when **done** completes. I find this very difficult to understand. 

It's actually worse, every Javascript programmer who has a concurrent
problem to solve must **invent their own concurrency model**.  The
problem is that they don't know that this is what they are
doing. Every time a Javascript programmer writes a line of code that
says ``on this do that'' they are actually inventing a new concurrency
model, and they haven't a clue how the code will interleave when it
executes.

(I actually have a love-hate relationship with Javascript, most parts
I love but I hate the concurrency model- that might sound funny since
Javascript has no concurrency model - so there's nothing to hate :-)

What's even more difficult to understand is errors. Errors in
multi-threaded callback code with shared memory is something that would
give me an extremely large headache.

Green Callbacks
===============

Just to make life more confusing, we use a lot of callbacks in
Erlang.  I'll call these ``green callbacks.'' So callbacks are not
necessarily bad.  In Erlang we can see precisely where, in the context
of a process, a callback is evaluated, so we don't get the problem of
figuring out how the callbacks interleave.

Here's a example of a green callback in Erlang:

{% highlight erlang %}
    loop(F) ->
        receive
            {new_callback, F1} ->
                loop(F1);
            Msg ->
                F(Msg),
                loop(F)
        end.
{% endhighlight %}

When the processes running this code receives a message **Msg** it
evaluates the function **F(Msg)**.  There is no uncertainty about
this, we know _exactly_ when the callback is triggered.  It is
triggered immediately after receiving the message **Msg**.

This wee code fragment is doubly beautiful, if you send the process a message
**{new_callback, F1}** then it will change its behavior using the
new callback on the next invocation.

I don't know how you would write this in Javascript. I've written
quite a lot of jQuery and know how to setup and remove callbacks. But
what happens if an event is triggered in the time interval between
removing an event handler and adding a new one. I have no idea, and
life is too short to find out.
 
How Erlang I/O really works
============================

We don't actually send messages to a process. We send messages to a
process mailbox. Each process has a mailbox, when we send a message to
a process it is put in the mailbox (if the postal guy can find the
processes).

Imagine Erlang processes are houses with mailboxes. Sending a
message means you hand your message to the mail guy, whose job is
to do two things: deliver the mail in the destination mailbox and bang
on the door to say ``new mail.''

A process can be doing something or asleep. Just like a house owner,
they can be awake doing something or asleep. If they are asleep, then
when the postman comes and bangs on the door they wander over to the
mailbox to check if anything interesting has arrived.

If they are doing something, and have finished a job, they might just
wander over to the mailbox to see if any new mail has arrived when
they were doing other things.

That's how Erlang messaging works. Each house (process) has a life of
its own.  The mail guy delivers the mail, and the house owners go
check the mailboxes now and then when they feel like it.




 
 



