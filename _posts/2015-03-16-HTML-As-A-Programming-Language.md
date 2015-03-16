---
layout: post
title: HTML as a programming language
tags: HTML programming erlang
year: 2015
month: 3
day: 16
published: true
---

Subtitle: Unifying HTML with Erlang.

HTML is a __layout__ language not a __programming language.__ 
With a few simple additions we can restructure HTML to make it into a
programming language.

# Step 1 - Adding structure to HTML

HTML has a flat structure. In order to turn HTML into a programming
language we have break the HTML file into named sections.

This is easy. The first figure shows some HTML before and after 
the addition of a sectioning statement.

<img src='/images/html1.png'/>

The statements `@function{lorem}` and `@function{grumpy}` break the HTML 
file into named sections. We'll see later how each named section 
is compiled into a single Erlang function.

# Step 2 - Calling HTML

Now that we have split the original HTML file into sections we can
``call'' the sections from other sections of HTML. Here's an example
where we call code in `one.html` from `two.html`.

<img src='/images/html2.png'/>

The syntax `<? one:lorem(In) ?>` means replace the `<?...?>` construct
with the contents of the section `lorem` in `one.html`.

When we _evaluate_ `two:main(Env)` we get:

<img src='/images/html2a.png'/>

# What's inside the funny brackets?

What's inside the `<? ... ?>` brackets -- Well since I like Erlang,
I'll let you into a secret, it's just plain Erlang code. No need to
invent yet another templating language when Erlang is at hand.

So to include 10 paragraphs of filler text I'd write:


<img src='/images/html3.png'/>

# Everything is just a function

The nice thing about the transformation to Erlang is that
all HTML fragments become Erlang functions after transformation.


The transformation of HTML into Erlang is as follows:

<img src='/images/html4.png'/>

The input file `one.html` is transformed into an Erlang module `one.erl`.
Each HTML fragment is transformed into a single Erlang function with the
same name as the fragment name.

HTML code is transformed into an Erlang map of the form `#{html =>
"..."}` Erlang code is transformed into a sequence of forms each of
which returns a map.

Finally all the maps are combined
and a module is created with the same name as the HTML page.

# Adding JS/CSS/ERlang

The next step is to extend this so we can deal with Erlang, Javascript
and CSS in a uniform manner:

Here's an example:

<img src='/images/html5.png'/>

We don't need to do anything special with Javascript and CSS. The
parameter passing mechanism works as expected so we can pass parameters
into CSS. So for example the parameter `"red"` gets bound to the variable
`In` and expanded in the body of the CSS definition. The added function
`to_string(X)` converts its argument to a map.

Erlang functions are written following a `@erlang` annotation, as in
the example. All we have to remember is that functions that are called
from HTML functions must return an object of type `#{html => ...}`

# Efficiency

I haven't mention efficiency. My plan is to __make if right then make
it fast__. Right now this is just a proof of concept. On the other
hand I expect this to be efficient. Once transformed and loaded HTML
functions become compiled Erlang code which is pretty fast. So I
expect this to be blindingly efficient - the file system never gets
touched, everything is compiled and efficient management of concurrent
session is built into the virtual machine.

# About

As I said this is a proof of concept- I wrote this article to
accompany [a talk I'm
giving](http://www.meetup.com/ErlangSF/events/220955245/) in a couple
of weeks time.

The code has not yet been published - I have a prototype that works and
I'll publish it when it's sufficiently mature.

# What else is in the map?

So far the examples show a return value of `#{html => "..."}`. Two more keys
are planned.

`#{headers => [{Key,Value}]}` adds some headers to the map. `#{once => [Name]}`
adds a fragment `Name` to the map (this is equivalent to calling
`<? Name(In) ?>` but it's only done once. This is so we include
CSS and JS code once only. If several fragments use the same CSS styles
we only want the styles to be included once.  

