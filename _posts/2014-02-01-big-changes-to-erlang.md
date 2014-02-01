---
layout: post
title: Big changes to Erlang
tags: maps associate arrays
year: 2014
month: 2
day: 1
published: true
---

Yesterday release candidate 1 of version R17 of Erlang
was released.

This was a major event. Version R17 has some changes to Erlang that
significantly improve the language. These are the biggest changes
since the introduction of higher order functions and list
comprehensions.

Erlang now has maps and named arguments in funs.

We've been talking about maps for over twelve years, but now they are
here to stay.

Why the long wait? - we wanted maps to be a replacement for records
and to be as efficient as records, and its not blindingly obvious how
to do so.

In the remainder of this article I'll explain some of the new features in
Erlang version R17.

# Erlang now has maps

Records are dead - long live maps !

Maps are associative collections of key-value pairs.  In Perl and Ruby
they are called hashes, in C++ and Java the are called maps in Lua
tables, and Python calls them dictionaries.  We call them maps.

We can create a new map by writing:

    A = #{key1 => Val1, key2 => Val2, ...}

And we can pattern match a map by writing :

    #{key1 := Pattern1, key2 := Pattern2, ...} = VarContainingAMap

Updating a map is done as follows:

     NewX = X#{ key1 => Val1, ... KeyN := ValN, ...}

The update operators are either "=>" or ":=" but what they do is
subtly different.

<b>Key => Val Introduces a NEW key</b>

This should be used whenever you know that you want to add a new key
to a map or when you are unsure if the key exists in the old map or not.

<b>Key := Val Updates an existing Key</b>

The key must be present in the old map.
If Key is not in the old map Erlang will complain loudly.

Using two operations instead of one has several advantages:

<i>A spelling mistake cannot accidentally introduce a new key </i>


Suppose we want to update a key called age, and suppose that there
were was only one operator ":" that both updated an old element in the
map or created a new element in the map.  In this case we might write:


<pre>
birthday(#{age : N} = Person) ->
    Person#{arg : N+1}
</pre>

So calling birthday(#{person:bill, age:12}) would 
return #{person:bill, age:12, arg:13}

A new element (arg) has been introduced into the map, but it has the
wrong key, and it will live in the map for a long time.  We meant to
write age but by mistake we wrote arg.  This is how things work in
Javascript, a simple misspelling of a tag name will create a bad
element in an object, and the consequences of this probably won't be
discovered until a lot later.

This is why Erlang has two operators, Key => Val always adds a new
element to the map, but Key := Val updates an existing element and
shrieks with protest and immediately crashes the program if you try to
update an element that does not exist in the map.

<i>Different maps can share key descriptors</i>

Updating existing keys in the map has another unexpected consequence.
If the compiler sees a line of code like this:
    
<pre>
    X1 = X#{key1 := Val1, key2 := Val2, ...}
</pre>

When all the update operators are ":=" then it know that the new
object X1 has the same keys as X. This means we can store the a
descriptor of X (ie what the keys the object has) and the values are
in different places, and that the new object X1 can share the keys
used by X.

If we now build a large list of objects, all with the same set of
keys, then they can share a single key descriptor. This means that
asymptotically maps will be as efficient in terms of storage as
records.

Credit for this idea comes from Richard O'Keefe who pointed
this out years ago. I don't think any other programming language does this,
but I might be wrong.

<i>What are the keys in maps?</i>

The keys in maps can be any ground term - which is great. We argued
over this for years.

Here's an example:

<pre>
> Z = #{ {age,fred} => 12, {age, bill} => 97, 
         {color, red} => {rgb,255,0,0}}.
</pre>

Then I can pattern match out any of the arguments like this:

<pre>
>    #{ {color,red} := X1} = Z.
</pre>


which binds X1 to {rgb,255,0,0}

So now we have maps - the keys can be any ground term. We can handle
large lists of maps that all have the same keys in a space efficient manner
and we can guard against accidentally introducing bad keys into the map
in a sensible way that will not cause problems in the future.

# Names in Funs

You can use a name inside a fun before the name has been defined. This
makes it rather difficult to define things like factorial in a fun.

Why is this?

You might think that the natural way to define factorial inside a fun
would be to say:

<pre>
   Fac = fun(0) -> 1; (N) -> N*Fac(N-1) end
</pre>

The problem here is that inside the fun (ie between the <b>fun</b> and
<b>end</b> symbols) the variable Fac has not yet been defined, it's
only defined outside the scope of the <b>begin</b> .. <b>end</b>
construct.

There is a way round this, we add an additional argument to the
internal function that contains the name of the function to be called
so we write the inner part of the factorial function as

<pre>
     fun(F, 0) -> 1;
        (F, N) -> N*F(F, N-1)
     end.
</pre>

and pass the function to be called as an additonal argument to the function.
Now everything is defined. If we say:

<pre>
    G = fun(F, 0) -> 1;
        (F, N) -> N*F(F, N-1)
     end.
</pre>

Then G(G,X) will computer factorial X.

But we want to hide this horrible function G, so we write:

<pre>
    Fac = fun(X) ->
            G = fun(_, 0) -> 1;
                   (Fac, N) -> N*Fac(Fac, N-1)
                end,
            G(G, X)
          end.
</pre>

 After this feat of intellectual masturbation is over you have defined
the factorial function. We can even type this monstrously horrible
expression into the shell and test it:

<pre>
1> F = fun(X) ->
1>       G = fun(_, 0) -> 1;
1>              (Fac, N) -> N*Fac(Fac, N-1)
1>           end,
1>       G(G, X)
1>     end.
#Fun<erl_eval.6.71889879>
2> F(10).
3628800
</pre>

And goodness gracious - it works.

This trick is well known to old-style functional programmers, they
waffle on about Y combinators and eat this stuff for breakfast, but
it's the kind of stuff that gives functional programming a bad
name. Try explaining this to first year students who had a heavy night
out at the pub the evening before.

But there is an easier way. Allow names in the function definition before
they are fully defined.

So now we have a better way .. and there should be a drum role here.

Here's the new way (in the shell).

<pre>
   1> F = fun Fact(0) -> 1; 
              Fact(N) -> N * Fact(N - 1) 
          end.  §
   #Fun<erl_eval.30.71889879>
   2> F(10).
   3628800
</pre>

Which is a zillion times better than the old way.






