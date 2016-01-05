---
layout: post
title: Fun with Swift
tags: swift gui cocoa
year: 2016
month: 1
day: 2
published: true
---

I don't know where to start this. It started off as my post turkey
festive season relaxation and just gets longer and longer.

I thought to myself ``I'll try and make a GUI in Swift using only
emacs and the Swift REPL'' - I Googled a bit (as one does these days)
and found an amazing bit of code in [Swift from the command
line](https://forums.developer.apple.com/thread/5137). 
``Jens'' gave the following code example:

<pre>
{% include_relative swift/browser.swift %}
</pre>

{% highlight swift %}
{% include_relative swift/browser.swift %}
{% endhighlight %}

Which when I stuck in a file called `browser.swift` and gave the shell command
`swift browser.swift'  up popped the following window:

<img src='/images/browser.png'/>

I later found very similar code with more explanation at
http://practicalswift.com/.

My immediate reaction was ``Golly -- Swift might just be usable'' and
I was hooked.  I'd also read that Swift was a functional language
which again piqued my interest.

In the next week or so I found that Swift was certainly not a pure
functional language but that it was whole lot better than
Objective-C. Above all it had a REPL and I could program outside Xcode
(which I hate). I can use my dear and very old friend emacs whose
commands are located somewhere near the base of my spine.

# How we programmed GUIs in the good 'ol days

Once upon a time GUIs were windows containing a few buttons and some
text. When you clicked the buttons the text changed. We were easily amused
in the old days.

Writing a program to create a GUI was really easy.  I'd write
some code that looked something like this:

<pre>
window = make_window(Width, Height, Title)
b1 = make_button(window, X, Y, Width, Ht, Label)
e1 = make_entry(window, X, Y, Width, Ht)
l1 = make_label(window, X, Y, Width, Ht)
b1.onclick = function
                X = read(e1)
                write(l1, X)
             end
</pre>

I'd use my favorite text editor, stick this in a file `my_gui.bas` and
say the magic words:

    > basic my_gui.bas

Then the wife, kids, cats, dogs and the entire neighborhood would
rejoice a GUI would appear as if by magic.

I'd hardly call this programming, all we had to do was figure out the
positions and labels on the objects and type them into a computer.

As time went on tools were built to ``assist'' writing such programs
as if it were too difficult to figure out the coordinates of where
those pesky little buttons should go.  Figuring out the coordinates
could be done in a drag and drop interface and we could offer the user
a vast library of widgets that could be used - not just buttons and
text entries, but sliders and date selection widgets and goodness-knows-what-they-do
selectors.

The languages to program GUIs became more complicated, and the number
of functions available to manipulate the widgets became so large that
nobody could ever remember them all.

So we made integrated IDEs that hid thousands of commands from the
users and which offered hundred of widgets and could be used to build
amazing shiny things.

But the tools became larger and larger to the point of unusability and
the simple ways of making a GUI which a beginner programmer could
knock up in a few minutes were lost.

I'm an old-style programmer. I like to understand *every* line of code
I write.  I like to write my code in files and study the results.

> This is my attempt to make an old-style GUI in Swift.

All the code for the GUI must be in a single Swift file which I'll
launch from the terminal. I'll use only the Apple Swift compiler, Emacs
and Make.

I'm a beginner at Swift and Cocoa - I've asked a number of questions
in the article and I hope that some of you who read this can provide
answers to these questions. All the source is on-line and so you can
send me pull requests or add comments at the end of the article.

Another goal is to shorten and/or clarify the programs. So if you can
remove a line of code which you feel is irrelevant, or improve the
explanation please tell me.

Brevity of expression is not a primary goal - since I'd favor slightly
more verbose code if it is clearer than extremely terse code. The goal
is clear obviously correct code and code patterns that are easy to
scale and reproduce.

To help you navigate you can jump straight to the code or read the
entire article from start to finish:

# Part 1 - primitive functions

+ [experiment 0 - Not really a window](#experiment0)
+ [experiment 1 - A minimal window](#experiment1)
+ [experiment 2 - A window with specific size and a title ](#experiment2)
+ [experiment 3 - A window with a button ](#experiment3)
+ [experiment 4 - A window with a text field](#experiment4)
+ [experiment 5 - Connecting the button to a text field](#experiment5)
+ [experiment 6 - A window with an image](#experiment6)

# Part 2 - Abstractions

+ [Parameter naming gobbledygook](#parameters)
+ [Putting it all together](#combined)

# Part 3 - Philosophy

+ [Swift as a functional language](#swift_fpl)
+ [Why I use text and not XCode](#why_i_use_text)
+ [References](#references)

The programs have been tested on OS X Yosemite version 10.10.5 with
Xcode version 7.2 and Swift version 2.1.

Each program is in a single file
called `experiment1.swift`, `experiment2.swift` and so on.

The source can be found [here](https://github.com/joearms/joearms.github.com/_posts/swift).


# Running and compiling the examples

To run the an example in a terminal give the command

    swift experiment<K>.swift

To compile `example<K>` give the command:

    switftc -o experiment<K> experiment<K>.swift

This will generate an executable called `experiment<K>`

# Part 1 - Primitive Functions

# <a name="experiment0"></a> Experiment 0 - How not to make a window

If we start Xcode, do `File->New->Project` and select `Cocoa Application`
then set the language to `Swift` then Xcode will create a file called
`ApplicationDelegate.swift` with the following content:

<pre>
{% include_relative swift/AppDelegate.swift %}
</pre>

This file cannot be run outside Xcode - here's what happens if we try to
run this directly in a terminal:

```
swift AppDelegate.swift
AppDelegate.swift:11:1: error: 'NSApplicationMain' attribute cannot be used in a module that contains top-level code
@NSApplicationMain
^
AppDelegate.swift:1:1: note: top-level code defined in this source file
//
^
```

So this doesn't work - but after a little Googling and some small
edits we can modify this so that we can create a window in the shell, this is show in the next
section.

# <a name="experiment1"></a> Experiment 1 - A simple window

<img src='/images/experiment1.png'/>

This window is created with the following code - which is
derived from `AppDelegate.swift`.

<pre>
{% include_relative swift/experiment1.swift %}
</pre>

To run the program store the above in a file `experiment1.swift` and run
the program in a terminal with the following command:

    $ swift experiment1.swift

# <a name="experiment2"></a> Experiment 2 - A window with a title and specific size  

Now I'll make a larger window and add a title:

<img src='/images/experiment2.png'/>

Here I've make the window a specific size a title and some controls
to the title bar:

<pre>
{% include_relative swift/experiment2.swift %}
</pre>

# Adding controls

Now we can make a simple window. The next step is to add some controls.
I've added one control in each experiment.

# <a name="experiment3"></a> Experiment 3 - A window with a button

<img src='/images/experiment3.png'/>

Here's a window with a button in it - note that nothing happens when
we click the button - in experiment 5 I'll add a click action to the
button.

The code is as follows:

<pre>
{% include_relative swift/experiment3.swift %}
</pre>

*Exercise:* Try Googling `ThickSquareBezelStyle` you should be able to
find all the available button styles. Try making small edits to the
program to see what happens.

# <a name="experiment4"></a> experiment 4 - A window with a text field

<img src='/images/experiment4.png'/>

This is actually a `TextView` where I've set the background color to the window
background color.

*Exercise:* Make a better or alternative version.	

<pre>
{% include_relative swift/experiment4.swift %}
</pre>

# <a name="experiment5"></a> Experiment 5 - Connecting the button to the text field


<img src='/images/experiment5.png'/>
<pre>
{% include_relative swift/experiment5.swift %}
</pre>

This code is a bit of a mess, it had to be carefully constructed so
that the variable `text` was defined *before* the button callback
function `myAction` (since `text` is referred to in the body of the
`myAction` function).

This is a tad tricky and would indeed be impossible if the relationships
between the controls could not be ordered (for example, it would be
impossible to create two buttons `b1` and `b2` which when clicked
change the labels of the ``other'' button) - I'll solve this problem
later by allowing the click action on a button to be defined *after*
the all the controls have been created.

# <a name="experiment6"></a>Experiment 6 - A window with an image

<img src='/images/experiment6.png'/>

The image is from Sydney Padua's most
excellent book [The Thrilling Adventures of Lovelace and Babbage](http://sydneypadua.com/2dgoggles/)



<pre>
{% include_relative swift/experiment6.swift %}
</pre>


# Part 2 - Abstractions

The goal of this part is to make a simple set of functions that hides
most of the detail of making windows and adding controls to them.

The end result is going to be a  program fragment that looks like this:

<pre>
let entry1  = make_entry(window, (200, 80, 180, 30), "1")
let text1   = make_text(window, (20, 80, 180, 30), "Hello from me")
let text2   = make_text(window, (20, 120, 180, 30), "Another field")
let button1 = make_button(window, (120, 40, 80, 30), "Click")
     
// make a click function
let f1 = {() -> Bool in
            text1.string = "Callback worked"
            print(entry1.textStorage!.string)
            text2.string = entry1.textStorage!.string
            return true} //

button1.onclick = f1
</pre>

All the details of making the controls will be hidden in functions
like `make_text`, `make_button` and so ``wiring up'' the controls will
be done by hooking callback functions on the buttons.

To do this needs some trickery, so in the sections that follow I'll
first explain the techniques and then refactor the code in the Part 1
into a form that fits my purpose.

Firt I'll look at parameter passing in Swift.

# <a name="parameters"></a> Parameter naming gobbledygook

> Swift parameter passing is totally unobvious.

I had expected that the ``obvious'' way to declare and call a function
would be something like this:

<pre>
{% include_relative swift/funcs1.swift %}
</pre>

But oh dear:

<pre>
swift funcs1.swift
funcs1.swift:5:18: error: missing argument labels 'y:z:' in call
print("add=", add(1,2,3))
                 ^
                    y:  z: 
</pre>

A little research revealed that the correct way to call
`add` was to use the totally unobvious syntax:

<pre>
{% include_relative swift/funcs2.swift %}
</pre>

Inconsistent *moi*? - the first parameter name is omitted. All the other parameters
must be present in the same order as the definition. Great shades of objective C!

This horrible syntax works:

<pre>
> swift funcs2.swift
add = 6
</pre>

Better though again unobvious is to prefix each argument in the function definition with
underscore `_` like this:

<pre>
{% include_relative swift/funcs3.swift %}
</pre>

Now at least we can call the function in the same way as we'd do in
just about every other programming language under the sun.

# A button callback function

To make a callback function for a button we have to step back and then understand how
closures work, and then we can make  a callback function.

I'll start by defining a simple class with an instance variable:

<pre>
{% include_relative swift/classes1.swift %}
</pre>

This should look familiar to you if you've programmed in an OO language.

Running this we see the following:

<pre>
> swift classes1.swift
fred
joe
</pre>

Now I'll do the same things with a functional argument:

<pre>
{% include_relative swift/closures1.swift %}
</pre>

Running this:

<pre>
true
hello I'm a callback and i = 10
true
hello I'm a callback and i = 20
true
</pre>

> Which to my mind is horrendous. The closure `f` does not capture the value of
`i` at the time when `f` is defined, changing `i` *after* `f` was defined
make nonsense of the idea of a closure. 

If we are extremely careful we *can* use this mechanism to add a callback facility to
buttons. We'll do this and make a custom class, by inheriting the properties
of `NSButton` and adding our own callback routine:

<pre>
class MyButton: NSButton {
    var onclick: () -> Bool = {() -> Bool in true}

    func myclick(sender: AnyObject) {
        self.onclick()
    }
}
</pre>

and to make a button I call `make_button`

<pre>
func make_button(window: NSWindow,
                 _ size:(x:Int, y:Int, width:Int, ht:Int),
                 _ title:String
) -> MyButton {
    let button = MyButton()
    button.frame = NSMakeRect(CGFloat(size.x),     CGFloat(size.y),
                              CGFloat(size.width), CGFloat(size.ht))
    
    button.title = title
    button.bezelStyle =  .ThickSquareBezelStyle
    button.target = button
    button.action = "myclick:"
    window.contentView!.addSubview(button)
    return button
}
</pre>

Now we can make some objects and add an `onclick` callback to the buttons (just like
jquery) - the pseudo code to do this will look something like this:

<pre>
    ... create a window ...
    text1  = make_text(window, Size, Text)
    entry1 = make_entry(window, Size, Default)
    button1 = make_button(window, Size, Text)
    F = ... a function involving text1, entry1, ... etc.
    button1.onclick = F
</pre>

# <a name="combined"></a> Putting it all together

Now let's put it all together:

<img src='/images/combined1.png'/>

The code is rather long - but I was able to pull out most of
the mess into simple reusable functionbs.

> The code to create the window and add controls was easy
and Xcode was not used.

<pre>
{% include_relative swift/combined1.swift %}
</pre>

# <a href="open"></a> Open Problems

I had hoped to write:

      let window = make_window(400, 200, "My Title")
      let entry1 = make_text(window, ...)

But for some reason this does not work, so I have to move the call to `make_window`
to the initialisation part of the class. I haven't a clue why.

# <a name="swift_fpl"></a> Swift as a functional language

The bad

+ Verbose syntax - types are declared rather than inferred
+ Mutable data types
+ Weird mix of Classes, Structs and functions
+ No concurrency model

The good

+ It has a REPL
+ I can write apps *outside* XCode

I *like* the syntax of exceptions if the function X can raise an exception
then one must qualify call with ! ? or use an explicit try syntax

In Erlang term the use of ! and ? is easy to explain

Imagine an Erlang function f(X) that returns {ok, Val} when `Val = f(X)` or
otherwise `{eror, Why}` if the function could not compute a value for some argument `X`

The Swift compiler convention if implemented in Erlang would require `f(X)` to be
evaluated within a `catch` statement, like this:

     case (catch f(X)) of
     	  {ok, Val} ->
	      ...;
	  {error, Why} ->
	      ...
     end

otherwise an error would be indicted.

If we were absolutely sure that `{ok, Val}` would be returned we'd write:

     Val = f!(X)

Which is equivalent to the Erlang

     {ok, Val} = f(X)


The ? convention unwraps `{ok, Val}` into `Val` or `{error, Why}` into `nil` in Swift.

# <a name="comments"></a> Comments on Swift

I've been programming Swift for a couple of weeks now. Do I like it?

Swift is widely marketed as a functional language - so it's
interesting to see how well it shapes up as a functional programming
language - or at least to see how compares to other FPLs that I am
familiar with.

Sequential FPLs gain a lot of power from the mechanisms they offer.
The offer (in varying degrees) type systems that (claim to) prevent
(some run-time) errors. Pattern matching syntaxes  make the
programs very short. Immutable data structures simplifies
reasoning about the programming and debugging. Higher order functions
add to power of the language be treating functions as first
class data.

Where Swift shines it is in the integratiion with the underlying Objective C
frameworks on the Mac. To this extent Swift programming is a much
more attractive proposition than programming in C or Objective-C but
but this is not because Swift is a good language rather that C and
objective-C are bad languages for writing user-space application in.

The C family of languages (C, C++, Objective-C) are fine for writing
operating system but not for writing the majority of user-space
applications - here things like Python or visual-basic are far better.

Swift is a good replacement for
contexts in which Objective-C would be used.

If you're coming form Erlang/Haskell world you'll think ``Swift is
verbose and a bit of a mess`` but if you're coming from Objective-C
you'll think ``Swift is concise and elegant''

In Swift I really miss pattern matching and I dislike having to
excessivly declare types.

The lack of a decent concurrency model in Cocoa and Objective-C is
reflected in the Swift code - which although it works is pretty
horrible.

# <a name="why_i_use_text"></a> Textual vs Interactive interfaces

I've said that I don't like Xcode so I've added some reasons why.

Two common ways to create programs are:

1) We create a text file using a text editor.

2) We create a program by clicking on buttons and dragging objects in
an IDE (Integrated Development Environment) (for example, Xcode).

In the first method we don't usually have to tell the user how to
create a text file with given content. It suffices to give a listing
of the file and assume that the user can create the file using an
editor of their choice. It is totally irrelevant *how* the file is
created the only thing that matters is that the content of the file is
correct.

In order to understand the program only the content of the file must
be understood. We can examine it line by line, asking if we understand
what the lines of code mean.

Using an IDE is horribly different - describing how to interact with a
an IDE is very difficulty to do in text. Usually we have to *show* how to
do this typically with a YouTube video, or in a mixture of text and images.

An excellent example of the difficulty of describing how to do something
is can be found in [Notes from a Swift Developer](http://swiftrien.blogspot.se/2015/11/swift-example-binding-nstableview-to.html) -- in this example the author uses
a mixture of text and screenshots to describe how to build an application.
Text alone does not work.

The application described in the link above has a simple layout which could be *easily*
described in text (for example as an html table)

Unfortunately when we try to reproduce what we've seen in a video or
follow a description that is a mixture of text and images, we find that
the description almost invariably describes a different version of the
IDE than the one we have available.

Textual descriptions of the form ``and now click on the doggle control
icon'' are pretty useless if you haven't got a clue what the doggle
control icon looks like.

Worse - when the design process in an interface builder is finished -
all the correct buttons have been clicked the resulting ``state'' of
the system (ie the program) is not available in a textual format so we
cannot ask, line-by-line what the individual statement in the
description mean.

As you might gather - I hate IDEs like Xcode and Eclipse - I like to
totally understand every line of code I wrote - my method of
understanding code is always the same and independent of
language. Find an example program that works then reduce lines until I
can reduce no more making a minimal example that works - then
understand every line.

This is a slow process - but in the long run faster than clicking at
random in a IDE until your program works or Googling like crazy to see
if somebody else has a canned solution to your problem.

I get the impression that developers think that developing in a IDE is
somehow ``quicker'' than developing with no such tools. As far as I can see this
is false. Once I have a working program in a given directory structure
I can make a clone of this in a single terminal command (`cp -R ...`) and then
I'm off in my trusty editor.

Following instructions like

+ enter this ... into a file called ...
+ type the command ... into the shell

are easy to obey and pass the ``telephone test'' (ie can we describe exactly what to do
over a telephone) - no images or videos are needed to ``show'' the users what to do.

Once upon a time we could describe how to do something using text only.
Then we used text and images. Now it needs videos.

Asking questions about text was easy - ``what did you mean in paragraph 4''
now we'd have to ask ``what did you do 41.6-41.8 seconds into your video''
watching somebody doing something in Xcode 200.7 in a video and preforming slow
motion playbacks of selected sections is not my idea of programming.

 

# References

I've found several documents which helped me - none of them solves the problem
I'm try to solve but they have all provided clues to the solution:

+ http://czak.pl/2015/09/23/single-file-cocoa-app-with-swift.html
+ https://github.com/Eonil/CocoaProgrammaticHowtoCollection
+ https://forums.developer.apple.com/thread/5137
+ http://stackoverflow.com/questions/26609778/nsopenpanel-in-swift-how-to-open
+ https://objectivec2swift.com/#/converter/code
+ http://practicalswift.com/
+ https://github.com/tylergaw/js-osx-app-examples
+ http://www.raywenderlich.com/82046/introduction-to-os-x-tutorial-core-controls-and-swift-part-1
+ http://commandlinefanatic.com/cgi-bin/showarticle.cgi?article=art024
+ http://mediautopia.weebly.com/swift-1-intro.html
+ http://dev.iachieved.it/iachievedit/using-swift-as-a-scripting-language/
+ http://commandlinefanatic.com/cgi-bin/showarticle.cgi?article=art024
+ http://www.knowstack.com/swift-programming-an-introduction/

# Help and tips wanted

Does anynody know:

+ How can I create two or more windows? - I seem to only be able to create a single window
+ How can I make a socket client?
+ How can I make a socket server?

Solutions should be single Swift files that can be run from the terminal.



# The future

Articles like this are never finished only started ...






