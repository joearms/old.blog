---
layout: post
title: A Week with Elixir
tags: elixir
published: true
---

About a week ago I started looking at [Elixir](http://elixir-lang.org). 
Elixir had been one of
those things that was vaguely aware of but had not yet time to look at
in any detail.

This all changed when I discovered the announcement that Dave Thomas
was publishing [Programming
Elixir](http://pragprog.com/book/elixir/programming-elixir).  Dave
Thomas edited my Erlang book and did great work in introducing Ruby,
so when Dave gets excited about something then this is a sure sign
that something interesting is in the wind.

Dave was excited about Elixir, in the introduction to his new book he
says:

<pre>
I came across Ruby in 1998 because I was an avid 
reader of comp.lang.misc (ask your parents). I 
downloaded it, compiled it, and fell in love. 
As with any time you fall in love,
it’s difficult to explain why. 
It just worked the way I work, 
and it had enough depth to keep me interested.

Fast forward 15 years. All that time I’d been 
looking for something new that gave me the same feeling.

I came across Elixir a while back, but for some 
reason never got stuck in. But a few months ago I 
was chatting with Corey Haines. I was
bemoaning the fact that I wanted to find a 
way to show people functional programming concepts 
without the kind of academic trappings those books 
seem to attract. He told me to look again at Elixir. I
did, and I felt the same way I felt when I first saw Ruby.  
</pre>

I know the feeling. Gut feeling  precedes logic. I
know when things are right, I don't know how or why I know, but the
explanation of why things are right often comes weeks or years
later. Malcolm Gladwell in his book
[Blink: The Power of Thinking Without Thinking](http://www.amazon.com/Blink-Power-Thinking-Without/dp/0316010669/ref=sr_1_1?s=books&ie=UTF8&qid=1369995752&sr=1-1&keywords=blink)
talks about this.  Experts in a particular field can often instantly
know that something is right, but they can't explain why.

When I saw that Dave had his eye ``on the ball'' I wanted to
know why.

Surprise number two, Simon St. Laurent was also writing a book on
Elixir. Simon did a good job with [Introducing
Erlang](http://www.amazon.com/Introducing-Erlang-Simon-St-Laurent/dp/1449331769)
... and we'd exchanged several mail, so something was in the air. With
both Pragmatic Press and O'Reilly racing to get into  Elixir I knew
something was happening on the Erlang VM, and I didn't know about
this. Boy am I out of touch.

I mailed Dave and Simon and they kindly sent me copies of their books
so I could start learning ...  Thanks guys ...

## I downloaded elixir last week and started playing ...

It didn't take long, but pretty soon my gut feeling kicked in. This is
good shit.  The funny thing is that Erlang and Elixir are the same
thing under the surface.  They ``feel'' the same to me. In fact this
is literally true, they both compile to instructions for the
EVM (Erlang Virtual Machine) - actually nobody call this the EVM They
just say the "Beam" VM but I thing we should start calling this the
EVM to distinguish it for the JVM.

Why do Erlang and Elixir have the same ``semantics''. The reason has
to do with the underlying machine. The garbage collection behavior,
the non-shared concurrency model, the underlying error handling and
code loading mechanism are identical. They must be identical they run
on the same VM. This is also why things like Scala and Akka will never
be like Erlang. Scala and Akka run on the JVM so under the covers,
things like garbage collection and code loading are fundamentally
different. 

What Elixir brings to the table is a complete different surface
syntax, inspired by Ruby. What you might call a ``non scary'' syntax,
and a load of extra goodies.

Erlang's syntax derived from Prolog and was heavily influenced by
smalltalk, CSP and the functional programming. Elixir is heavily
influenced by Erlang and Ruby.  From Erlang it bring pattern matching,
higher order functions and the entire process and error handling ``let
it crash'' philosophy. From Ruby it brings sigils, shortcut syntaxes, It
also adds a few goodies of its own, the |> pipe operator, reminiscent
of Prologs DCGs and Haskell monads (though less complicated, more like
the good old unix pipe operator) and the macro quote and unquote
operators, which come from the lisp quasiquote and comma operators.

Elixir also brings a new underlying AST to the table, instead of the
Erlang AST where everything form has it's own representation, the
Elixir AST has a far more uniform representation, which makes
meta-programming far easier.

The implementation is surprisingly solid, though some things don't work as I
expected. String interpolation (which is a great idea) works in a
hit-and-miss manner.

I'd thought that:

    IO.puts "...#{x}..."

evaluated x and injected a pretty-printed representation of x into the
string.  But it only works for some simpler forms of x and not all x.

Since you can call any Erlang function from Elixir this was easy to fix.

IO.puts "...#{pp(x)}..." always works. I just defined pp(x) as

	def pp(x) do 
		:io_lib.format("~p", [x])
		|> :lists.flatten
		|> :erlang.list_to_binary
	end

In Erlang this would be:

    pp(X) ->
      list_to_binary(lists_flatten(li_lib:format("~p),[X])))

which is ``obviously'' equivalent to the Elixir version. Actually the
Elixir version is easier to read. The |> operator in the above means
pipe the output of io_lib:format into lists:flatten and then
list_to_binary. Just like the good 'ol Unix pipe operator.

Elixir breaks a few Erlang holy cows - variables can be re-bound in
sequences.  This is actually ok, the resulting forms can still be
normalized into a static-single-assignment (SSA) form. While this is OK in
sequences it would totally verboten-nicht-do-not-do-it in loop
constructs. But this is fine, Elixir has no loops, only
recursion. Actually it could not have loops with mutable variables
since this would be impossible to compile into anything remotely
sensible in the EVM. SSA variables in sequences are fine, the EVM knows
how to optimize these. Loops no, so Elixir doesn't go there. It would even boil
nicely down into LLVM assembler - but that's another story too long to tell here.

# The Three Laws of Programming Language Design

* What you get right, nobody mentions it.
* What you get wrong, people bitch about.
* What is difficult to understand you have to explain to people over and over again.

Some language get some things so right that nobody ever bothers to
mention them, they are right, they are beautiful, they are easy to
understand.

The wrong stuff is a bitch. You boobed, but you are forgiven if the
good stuff outweighs the bad. This is the stuff you want to remove
later, but you can't because of backwards compatibility and some nitwit has
written a zillion lines of code using all the bad stuff.

The difficult to understand stuff is a real bummer. You have to
explain it over and over again until you're sick, and some people never
get it, you have to write hundred of mails and thousands of words
explaining over and over again why this stuff means and why it is so.
For a language designer, or author, this is a pain in the bottom.

I'm going to mention a few things that I think fall into these three
categories.

Before I start I'll just say that Elixir has got a heck of lot of
things right, and the good things far outweigh the bad things.

The nice thing about Elixir is that it's not too late to fix the bad
things. This can only be done before zillions of line of code get
written and before trillions of programmers start using it - so
there's only a few days to fix this.

No versions in the source files
===============================

XML files always start

<pre>
  &lt;?xml version="1.0"?&gt;
</pre>

This is great. Reading the first line of an XML file is like
listening to the opening bars of Rachmaninoff's third piano
concerto. A sublime experience. All praise to the XML designers,
hallowed be their names, give these guys some Turing prizes.

Putting the language version in all source files is essential. Why is
this?

Early Erlang did not have list comprehensions. Suppose that we give a
modern Erlang module to an old Erlang compiler and ask it to compile
it. The modern code has list comprehensions, but the old compiler
doesn't know about list comprehensions so the old compiler thinks this
is a syntax error.

If a **version3**  Erlang compiler is given a file that starts:
  
    -version(5,0).

Then it should say

    ** auuuuugggghhhhhh **

       Oh bother and blast, I am mere version 3 compiler
       and cannot see into the future.
       
       You have given me a version 5 program. This means 
       my time on earth has come.

       You will have to kill me. You will uninstall me, 
       and install a version five compiler. I will be
       no more. I will cease to exist.

       Goodbye old friend.

       I have a headache. I'm going to have a rest...
    **

It's the first law of data design:

     All data that might change in the future should be 
     tagged with a version number.

and a module *is* data.

Funs and defs are not the same
==============================

When I started writing ``Programming Erlang'' Dave Thomas wondered why
you couldn't type function in the shell.

If a module contains this:

    fac(0) when N > 0 -> 1;
    fac(N)            -> N* fac(N-1).

You can't just cut and paste this into the shell and get the same result.
Dave asked why and said that this was stupid.

In Lisp and so on you can. Dave said something like ``this is going to
confuse people'' - he was right and it does confuse people. There must
be hundreds to thousands of messages on forums asking about this.

I have explained why so many times that my hair has gone grey, it's
true my hair is now grey because of this.

It's because there is a bug in Erlang.

Modules in Erlang are sequences of **FORMS**.

The Erlang shell evaluates a sequence of **EXPRESSIONS**.

In Erlang **FORMS** are not **EXPRESSIONS**.


    double(X) -> 2*X.            in an Erlang module is a FORM

    Double = fun(X) -> 2*X end.  in the shell is an EXPRESSION

The two are **not** the same. This bit of silliness has been Erlang forever
but we didn't notice it and we learned to live with it.

In an Elixir Module you can write

    def triple(x) do
       3 * x;
    end

My bet is that thousands of programmers will cut and paste this from their text editor
into the shell and it will say

    ex> def double(x) do 2*x; end
    ** (SyntaxError) iex:66: cannot invoke def outside module

If you don't fix this you'll spend the next 20 years explain why - just like we did in Erlang.

BTW the fix is really really easy. I made erl2 as an experiment to fix
this. It can't be fixed in Erlang (backwards compatibility) so I did it
in [erl2](https://github.com/joearms/erl2). This needs a very small
change to erl_eval and a few tweaks in the parser.

Basically FORMS are not expressions, so I added a keyword **def**

     Var = def fac(0) -> ; fac(N) -> N*fac(N-1) end.

This is __defined__ to be an expression with a side effect. Since it's
an expression I can evaluate in the shell, remember the shell can
only evaluate expressions.

The side effect is to create a function called shell:fac/1 (just as if
it were defined in a module).

So really

    iex> double = fn(x) -> 2 * x end;

    iex> def douuble(x) do 2*x end;

Should be identical, and **both** should define a function with name **Shell.double**

Make this change and avoid grey hair.

Funs have an extra dot in the name
==================================

    iex> f = fn(x) -> 2 * xend
    #Function<erl_eval.6.17052888>
    iex> f.(10)
    20

In school I learned to call functions by writing **f(10)** not **f.(10)** -
this is "really" a function with a name like **Shell.f(10)** (it's a
function defined in the shell) The shell part is __implicit__ so it should
just be called **f(10)**.

If you leave it like this expect to spend the next twenty years of
your life explaining why.  Expect thousands of mails in hundreds of
forums.

The send operator
=================

    Process <- Message

What's on earth is? Do you realize how difficult it will be to convert
form occam-pi to Elixir.

You're going to lose the occam-pi community here. The send operator
should be **!**, like this:

    Process ! Message

Next week my brain will have gone soggy and my neutral network be
reprogrammed so that I ``see'' <- as **!** - this is not about thinking
it's about reprogramming the base of my spine. The send operator is
not in my brain, it's in the the base of my spine. My brains thinks
``send a message to a process'' and sends a message to my fingertips,
my spine adds the **!** then by brain does **backspace erase <-**.

It's a syntax thing. We all love to hate syntax. But on a scale of
badness where 10 is ``really really bad'' and 1 is well ``ok I might
get used to this'' its about a 3.

This is going to make it really difficult for programmers in Occam-pi
to convert to Elixir, by the simple act of changing **<-** to **!** will cause
hoards of occam-pi programmers would weep for joy rush into the
streets crying ``horray, horray, what a good day'' and immediately
convert to Elixir. Old men will tell of this in times to come, and
there will be much rejoicing and celebration in the land.

The Pipe operator
=================

This is one of those things that is really really good, and really
really easy to understand so nobody will give you credit for
it. That's life.

This is the recessive monadic gene of Prolog. The gene was dominant in
Prolog recessive in Erlang (son-of-prolog) but re-emerged in
Elixir (son-of-son-of-prolog).

**x |> y** means call **x** then take the output of **x** and add it as an extra
argument to **y** in the first argument position.

So

     x(1,2) |> y(a,b,c) 

Means

     newvar = x(1,2);
     y(newvar,a,b,c);

This is **very** useful. Suppose we want to capitalize an atom that is
turn the atom **'abc'** into **'Abc'**. There is no function to capitalize an
atom in Elixir but there is a function to capitalize a string. So we
have to first convert the atom to a string. So in Erlang we'd write

     capitalize_atom(X) ->
        list_to_atom(binary_to_list(capitalize_binary(list_to_binary(atom_to_list(X))))).

Which is horrible. We could also write

     capitalize_atom(X) ->
         V1 = atom_to_list(X),
         V2 = list_to_binary(V1),
         V3 = capitalize_binary(V2),
         V4 = binary_to_list(V3),
         binary_to_atom(V4).

Which is even worse - yucky code. How many times have I written code like
this? this sucks big time.

With the **|>** operator this becomes:

        X |> atom_to_list |> list_to_binary |> capitalize_binary 
          |> binary_to_list |> binary_to_atom

Why did I call this the recessive gene.

Erlang was derived from Prolog, and Elixir is derived from Erlang.

Prolog had DCGs so 
  
      foo --> a,b,c.

Got expanded into

      foo(In, Out) :- a(In, V1), b(V1,V2), c(V2, Out).

This is essentially the same idea. We create an additional extra
hidden argument that threads its way in and out of a sequence of
function calls. It's kind-of what a monad does in Haskell, but keep this
a secret.

Prolog had DCGs, Erlang didn't, Elixir has the pipe operator.

Elixir has sigils
==================

Sigils are great - love 'em. We should add these to Erlang.

A string is a programming language abstraction. Programming languages
have string literals, usually sequence of characters surrounded by
double quotes.  So when you see a line of code like:

    x = "a string"

the compiler turns this into some internal representation of the
string literal, this internal representation has some associated
semantics.

In Erlang

     X = "abc" 

means ``X is a list of integer representing the ASCII codes for a, b
and c''

But we can choose any meaning we want. In Elixir **x = "abc"** means
**x** is a UTF8 encoded binary. By adding a character in-front of the
leading quote we can change the meaning of the string literal so that
in Erlang:

     X = r"...."

could be defined to mean ``a compiled regular expression'' (ie is
equivalent to **X = re:compile("....")** - given that we know the meaning
of a string we can interpret the contents in different ways. We might
like to write

     A = "Joe",
     B = s"Hello #{A}".

So that B would be **"Hello Joe"** - here the s sigil would change the
interpretation of the string literal to mean **"substitute variables in"**.

Elixir does this in a very nice way, defining many different sigils.

In Elixir the sigil syntax is different, it's:

    %C{.....}

Their C is a single character followed by a pair of matching brackets
**{}** or **[]**.


Sigils are great. Erlang could have had these 15 years ago, and they can 
be added now without breaking backwards compatibility.

Docstrings
==========

Docstrings are great love 'em.

But I have a minor quibble. Please put the docstring **inside** the function definition

You say
 
    @doc """
     ...
    """
    
    def foo do
       ...
    end

Put it *inside* the function like this:

     def foo do
       @doc """
       ...
       """
     end

Otherwise you'll get``detached comment'' things will get mucked up
when you edit programs. Comments will get separated from the functions
to which they apply.

In Erlang I haven't a clue if a comment belongs to the next function
or the previous one or the module. If a comment refers to a function
it should be INSIDE the function not outside.

defmacro quote and unquote
===========================

Love 'em. This is parse transforms done right. The joy is that you
don't need to know the abstract syntax. Quote and unquote do the magic
for you.

This is one of those things that is right - it's great but
intrinsically difficult to explain. This is like monads in Haskell -
Yup, monads are really easy to understand that's why there are
hundreds of articles explaining how easy they are.

Elixir macros are really easy - quote is just quasiquote in lisp and
unqote is the list comma operator - so that was easy :-)

Extra punctuation
=================

This works:

    iex> lc x inlist [1,2,3], do: 2*x
    [2,4,6]

This doesn't:

    iex> lc x inlist [1,2,3] do: 2*x 
    ** (SyntaxError) iex:3: syntax error before: do

The extra comma after the list confuses me.


Whitespace Strangeness
======================

    iex> lc x inlist [1,2,3], do : 2*x
    ** (SyntaxError) iex:2: invalid token: : 2*x

Oops it should be ``do:'' and not ``do :''

To me white-space is white-space. Inside string literals I cannot mess
with white-space. Outside a string literal I like to add white space
wherever I feel like for formatting reason, so I can make the code
look pretty.

But I can't do this in Elixir - I don't like this.

Closures done right - Hip hip hooray
====================================


Closures in Elixir (fn's) are really just closures in Erlang (fun's)

fn's have the nice property of capturing the present value of any
variables that are in their scope (ie we can create immutable
closures) this is incredibly useful. This is something that JavaScript
gets very wrong. Here's an example in JavaScript and Elixir so you can
see the difference:

    js> a = 5;
    5
    js> f = function(x) { return x+a }; 
    function (x){return x+a}
    js> f(10)
    15
    js> a = 100
    100
    js> f(10)
    110

Oh dear we broke the function f. We define a function f, start using
it. Redefine a and this has the side effect of breaking f. One of the
good things about functional programming is that it makes it easy to
reason about programs. If f(10) evaluates to 15 then it should evaluate
to 15 forever, you should not be able to remotely break it.

What about Elixir? This gets closures right:

    iex> a = 5  
    5
    iex> f = fn(x) -> x + a end
    #Function<erl_eval.6.17052888>
    iex> f.(10)
    15
    iex> a = 100
    100
    iex> f.(10)
    15

Proper closures should only contain pointers into immutable
data (which is the case in Erlang) - no pointers into mutable data. If
a closure contains a pointer into mutable data and you change the data
later you break the closure. This means you can't parallelize your
program and even sequential code can contain weird errors.

In a conventional language creating proper closures would be very
expensive since it would require deep copying of all the variables
that are captured in the environment, but this is not the case in
Erlang or Elixir, since data once written is immutable. All you can do
later is refer to it. Internally this is through a pointer
(which the programmer never sees) and the garbage collector removes
all data that can never be referenced since nothing points to it.

We can have closures in the shell, but we can't have closures in a
module.

If I can say

    a = 10;
    f = fn(x) -> a + x end;

in the shell

why can't I say

    a = 10;
    def f(x) do
       a + x
    end

in a module. This is completely fixable I did this in an experiment
in erlang2.

Finally
======

This has been my first week with Elixir, and I'm pretty excited.

Elixir has a non-scary syntax and combines the good features of Ruby
and Erlang.  It's not Erlang and it's not Ruby and it has ideas of
it's own.

It's a new language, but books are being written as the language is
being developed. The first Erlang book came 7 years after Erlang was
invented, and the first popular book 14 years later. 21 years is too
long to wait for a decent book.

Dave loves Elixir, I think it's pretty cool, I think we're going to have fun
together.

Erlang powers things like Whats-App and crucial parts of half the
worlds mobile phone networks. It's going to be great fun to see what
will happen when the technology becomes less scary and the next wave
of enthusiasts joins the party.

This was written in haste and while excited. So probably has a few typos.
Feel free to push corrections.

 

