---
layout: post
title: Why Markdown Sucks
tags: markdown
year: 2016
month: 3
day: 21
published: true
---

>"There are only two kinds of languages: the ones people complain about and the ones nobody uses." -- Bjarne Stroustrup

Markdown really sucks, I'll grant you that it's useful, there's no
doubt about that but the lack of a standard, and the fact that no two
markdown convertors do the same thing is terrible.

# Changes to markdown regularily break my content

The last time I updated my blog, I did my usual, `git poke me in the eye` and
then got a threatening mail back from github. Here it is:

~~~
The page build completed successfully, but returned the following warning:

You are currently using the 'rdiscount' Markdown engine, which will
not be supported on GitHub Pages after May 1st. At that time, your
site will use 'kramdown' for markdown rendering instead. To suppress
this warning, remove the 'markdown' setting in your site's
'_config.yml' file and confirm your site renders as expected. For more
information, see
https://help.github.com/articles/updating-your-markdown-processor-to-kramdown.

GitHub Pages was recently upgraded to Jekyll 3.0. It may help to confirm you're using the correct dependencies:

  https://github.com/blog/2100-github-pages-now-faster-and-simpler-with-jekyll-3-0

For information on troubleshooting Jekyll see:

  https://help.github.com/articles/troubleshooting-jekyll-builds

If you have any questions you can contact us by replying to this email.
~~~

I made the changes suggested in the email and the next time I made a change to
my blog I got no build warnings, "good," I thought.

> But then I discovered that about a third of my old blog entries were
incorrectly formatted ...

So having taken great trouble to make the content look nice, github decided
to make a non backwards compatible changes that muck up my old pages.
 
> I was for exactly this reason that I left my [old blog
  site](http://armstrongonsoftware.blogspot.se/) and moved to
  github. Google messed around with the formatting of my content so I
  moved to Jekyll and Github.

# You've just broken a lot of my old content

I did as the mail suggested. Brilliant, but it broke the formatting of
a lot of my old blogs posts. Here are two non-backwards compatible
changes:

+ Smart quotes (a la LaTeX) no longer work and are not a kramdown option
+ Fenced code blocks now are limited by tildes and not backquotes. Which breaks most of my old code examples

So what am I supposed to do? - go back and edit all my old blog posts?
 
# Markdown renders any old crap without producing error messages

Editing my old pages is more difficult than it seems, since neither
the old dialect of markdown `rdiscount` nor the new `kramdown` have a
formal grammar and I haven't a clue what output they will produce,
since it is not formally specified.

Abrupt and non-backwards compatible changes to a text formatting
system are extremely irritating, text that was formatted nicely and
that worked in the past will in the future look bad - and who get the
blame. Me probably.

If this is a problem in a the perspective of a few months (I say a few
months since github seems to want to change it's formatting system
every few months and break my old content) what will happen in the
future?

If people in a few hundred years from now want to see what their
ancestors wrote, what will they find, a mess of badly formatted crap?
  
# How to make future proof markdown

I am reluctant to say this, but there is a future proof way of making
markdown - that's to write documents directly in XML with a strict DTD
and then **generate** markdown from the XML - that way whatever
incompatibilities the latest markdown de jour has to offer can be
accommodated with a tweak to the code generator.

# So if I hate markdown what do I like?

+ Pencil and paper is best. Boots instantly - very flexible.

+ LaTeX is better than markdown by far, but it has a fundamental
flaw. The computer decides where the text goes, not me - I want to be
in command.

+ PDF is great - I am in command but generating it is a pain.

+ XSL-FO is wonderful, and I love Apache FOP. But XSLT is not my
favorite programming language.

For high quality text, I choose XML markup with my own custom DTD,
then an Erlang program to transform this to XSL-FO using an Apache FOP
backend. This is more-or-less how all the documentation on erlang.org
is made.

Having strict parse trees for all the documentation, is a
pre-condition for automatically manipulating the documentation and
integrating it into various forms.

For low-quality text (like blogs) and emails, markdown is fine - but
please don't change the formatting rules after you've written dozens
of large documents.

 








