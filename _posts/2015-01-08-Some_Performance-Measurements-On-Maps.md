---
layout: post
title: Some Performance Measurements On Maps
tags: erlang maps performance
year: 2015
month: 01
day: 5
published: true
summary: Performance measurements are difficult
---

On Jan 8 Harit Hamnshu asked [this question](
http://erlang.org/pipermail/erlang-questions/2015-January/082455.html)
on the erlang mailing list.

To quote from the mail:

    Write a function map_search_pred(Map, Pred) that returns the first element
    {Key,Value} in the map for which Pred(Key, Value) is true.

    My attempt, looks like

    map_search_pred(Map, Pred)  ->
      M = [{Key, Value} || {Key, Value} <- maps:to_list(Map),
                                           Pred(Key, Value) =:= true],
      case length(M) of
        0 -> {};
        _ -> lists:nth(1, M)
      end.


I thought _That's an interesting question_ and secondly _the answer
is obvious, just iterate over the map and terminate when the predicate
is satisfied_. But it turns out that this is impossible.

> I thought I'd write a little program to test if my intuition was
correct. Here's what happened:

# Thought 1: It's easy

This was my first thought. This is *always* my first thought _I'm an optimist_.
I also wonder why the heck does anybody worry about performance.

> I was
brought up in the days of MHz clocks so back in the day I did worry about this
but when GHz clocks came I stopped worrying.

# Thought 2: I have to *measure* this.

I know my intuition about performance is always wrong - so it's
_measure, measure, measure_.

At this stage I wrote some code to make a large map:

    make(0, M, _) -> M;
    make(K, M, I) -> make(K-1,maps:put(I,I+1,M),I+2).

  Call this with `make(1000,#{},1)` and I get a map with 1000 elements.
  
Then I added an element at the ``beginning'' and ``end'' of then map
and made a few measurements.

> and yes I know that there is no much thing as the **begining**
> and **end** of a map. So ``beginning'' means  _at that position
> in the map that I expect to be visted first when I traverse the map._

Then I wrote some code to generate a list and measure
the times to locate an element when it was at the beginning and end of
the map. Something like this:

```
test(K) ->
    Map = make(K, #{}, 1),
    Map1 = Map#{0 => 0},
    Map2 = Map#{123456789 => 123456789},
    List1 = maps:to_list(Map1),
    List2 = maps:to_list(Map2),
    J1 = timer:tc(?MODULE, map_search_pred,[Map1,fun(X,Y) -> X =:= Y end]),
    J2 = timer:tc(?MODULE, map_search_pred,[Map2,fun(X,Y) -> X =:= Y end]),
    ...
```    

  I'd expected the timer results `J1` and `J2` to be the same but
there weren't. Since the entire list gets traversed in both cases, the
times should be roughly the same. But they weren't.

**I've append the entire program at the end of this posting.**

> The results of this first test puzzled me at first. But hang on, I
>  know that a)
> **performance measurement is difficult** b) **My intuition
>  about performance is always wrong**.

My use of `timer:tc` just gives me a ``quick first idea'' as to what's going on.

What's really going on is complicated by the garbage collection and ``warming'' the
  cache.

For fun I just swapped the order of the statements `J1 = ...` and `J2 =
...` in the program and re-ran it. This shouldn't change anything. The
*semantics* of the program are unchanged by this reordering - *but*
the timings change  a lot.

As I said - ``**performance measurement is difficult**''


#   Thought 3:

The ``obvious'' way to terminate abruptly from a loop
  is to use `catch/throw` (_that's why we have `catch/throw`_).

I defined:
   
    J3 = timer:tc(?MODULE, map_search_pred1,
                  [Map2, 
           	   fun(X, Y) when X =:= Y -> throw({X,Y});
		      (_,_) -> false
		   end]),

And:

    map_search_pred1(A, B) ->
      (catch map_search_pred(A,B)).

This still converts the entire map to a list, but exits early from the
list comprehension.

And yes it was faster ``most of the time'' - ie in several runs it *was*
   faster but sometimes it was slower:

Here's a typical run with a 20,000 element map:

```
 {length,20000},
 {maps_at_start,{6479,{0,0}}},
 {maps_at_end,{2327,{123456789,123456789}}},   <- 1) should be the same maps_at_start
 {maps_thrown,{1814,{123456789,123456789}}},   <- 2) it is faster
 {maps_foldl,{2486,{123456789,123456789}}},
 {lists_start,{2,{0,0}}},
 {lists_end,{1221,{123456789,123456789}}}}
```

This shows the name of the test and the result of calling `timer:tc` on the test case.

So now I have a program that works and I can make measurements with.

#  Thought 4:

> The notion of ``order'' in a map is unclear. I know that maps are
> printed in lexical order, but I don't know if they are stored
> internally in the same order as which they are printed.  
> And they might perform differently if the maps have
> a small or large number of elements.

# Thought 5:

The notion of iterating over the elements of a map seems strange to
me. I always think of lists as being ``things that you iterate over''
especially when there are large numbers of things.

Probably I'm being old-fashioned here. I guess I should think of
iterating over collections, but to me a collection is just a fancy
name for a list.

> and lists in Erlang aren't even `lists`. They are really  `stacks` -
> but that's another story, and changing the name would not be a good
> idea.

# Thought 6:

For short maps measuring the time is pointless - access times will be
very fast and it will be difficult to measure the differences in time
for different implementations. And for large collections I should be
using lists anyway.

# Thought 7:

> There **should be** a maps iterator.

Why don't I just walk over the map element at a
time until I get what I want. I need a `first`, and `next` operator
defined over a map.

I consult the manual page for `maps.erl`, looking for `first` and `next`.

# Thought 8:

The only maps iterator I can find is `maps:fold`.

I should be able to ``throw my way out'' of `maps:fold`.

Then I read the code:
    
`maps:fold` is defined like this:

     fold(Fun, In it, Map) when is_function(Fun,3), is_map(Map) ->
        lists:fold(fun({K,V},A) -> Fun(K,V,A) end,In it,maps:to_list(Map)).

So the map is converted to a list anyway, **this is horrific**.

> All of which means there is no alternative to converting the
map to list first and then walking down the list to get what you want.

Finding the first element in a list that satisfies a predicate is
really easy:

```
find_first([H|T], Pred) ->
   case Pred(H) of
      true  -> H;
      false -> find_first(T, Pred)
   end;
find_first([], _) ->
   {}.
```

My intuition says this will be fastest:

I measure:

```
> p1:test(20000).
 {length,20000},
 {maps_at_start,{6215,{0,0}}},
 {maps_at_end,{2253,{123456789,123456789}}},
 {maps_thrown,{1827,{123456789,123456789}}},
 {maps_fold,{2698,{123456789,123456789}}},
 {lists_start,{3,{0,0}}},
 {lists_end,{1229,{123456789,123456789}}}}
```

** This time I was right**. When the element is at the start of the
list it's very quick. When the element is at the end of the list it's
faster than all the implementations using maps.  Also it makes sence
to talk about an element being ``at the start of a list' or ``at the
end.''

I've appended my test program. As I said earlier, swapping the lines `J1=...`
and `J2=...` and running the program will convince you that performance
estimation is tricky.

> actually micro benchmarks (like this) are pretty difficult to interpret.
at the end of the day you have to ask ``is my application fast enough.''
Most projects fail before the applications are complete so this question
never gets answered]

# Here's the test program

{% highlight erlang %}
-module(maps_timings).
-compile(export_all).

map_search_pred(Map, Pred)  ->
    M = [{Key, Value} || {Key, Value} <- maps:to_list(Map), Pred(Key, Value) =:= true],
    case length(M) of
	0 -> {};
	_ -> lists:nth(1, M)
    end.

map_search_pred1(A, B) ->
    (catch map_search_pred(A,B)).

test(K) ->
    Map = make(K, #{}, 1),
    Map1 = Map#{0 => 0},
    Map2 = Map#{123456789 => 123456789},
    List1 = maps:to_list(Map1),
    List2 = maps:to_list(Map2),
    J1 = timer:tc(?MODULE, map_search_pred,[Map1,fun(X,Y) -> X =:= Y end]),
    J2 = timer:tc(?MODULE, map_search_pred,[Map2,fun(X,Y) -> X =:= Y end]),
    J3 = timer:tc(?MODULE, map_search_pred1, [Map2, 
					      fun(X, Y) when X =:= Y -> throw({X,Y});
						 (_,_) -> false
					      end]),
    J4 = timer:tc(?MODULE, faster, [Map2, 
				    fun(X, Y, _) when X =:= Y -> throw({X,Y});
				       (_,_, A) -> A
				    end]),
    J5 = timer:tc(?MODULE, find_first, [List1, fun({X, Y}) -> X =:= Y end]),
    J6 = timer:tc(?MODULE, find_first, [List2, fun({X, Y}) -> X =:= Y end]),
    
    {
     {length,K},
     {maps_at_start,J1}, 
     {maps_at_end,J2}, {maps_thrown,J3}, {maps_foldl,J4}, 
     {lists_start,J5},
     {lists_end,J6}}.
     
make(0, M, _) -> M;
make(K, M, I) -> make(K-1,maps:put(I,I+1,M),I+2).

faster(Map, Fun) -> 
    case catch (maps:fold(Fun, [], Map)) of
	[] -> {};
	Other -> Other
    end.

find_first([H|T], Pred) ->
    case Pred(H) of
	true  -> H;
	false -> find_first(T, Pred)
    end;
find_first([], _) ->
    {}.
{% endhighlight %}


	    
					    

    
    

    
