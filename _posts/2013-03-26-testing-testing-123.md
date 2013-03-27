---
layout: post
title: Testing testing 123
tags: jekyll bootstrap github disqus
year: 2013
month: 3
day: 26
published: true
summary: testing my blog
---

This is a test of Jekyll. If it works I'll move all my blogs to github.

The is a <a href="http://cobyism.com/jekyll/docs/home/">guide</a> to Jekyll. Also
reading <a href="http://paulstamatiou.com/how-to-wordpress-to-jekyll">How to convert
wordpress to Jekyll</a> clarified a few issues.

Code highlighting test
======================

Can we highlight Erlang code?

{% highlight erlang %}
start([Config]) ->
    File = (atom_to_list(Config)),
    io:format("ws start:~p~n",[File]),
    Conf = read_config(File),
    start_link(Conf).
{% endhighlight %}

Yes!


Quotes test
===========

Quotes seem to work properly. I can make &ldquo;curly quotes&rdquo; and
``regualar quotes''.

Stuff that is broken
====================

* Colored quoting of text
* My tweet button is wrong

URL Autolinking does not work
==========

URL autolinking seems to be broken. The next line is not turned into 
a link:

http://help.github.com/articles/github-flavored-markdown.html

But the following *is* a link:

<a href="http://help.github.com/articles/github-flavored-markdown.html">a link</a>


Comments
========

I have used Disqus for this. It seems to work nicely.
If anybody knows how to make a tweet link or why URLs are not autolinked please
add a comment.

Tests
=====

Page.title = {{page.title}}

Page.url = {{page.url}}

The link below is missing the twitter icon. Anybody know what's wrong?

<a href="https://twitter.com/share" class="twitter-share-button" data-size="medium"
data-url="http://joearms.github.com/2013/03/27/promoting-erlang.html">
tweet</a>

<script>!function(d,s,id){var js,fjs=d.getElementsByTagName(s)[0];if(!d.getElementById(id)){js=d.createElement(s);js.id=id;js.src="//platform.twitter.com/widgets.js";fjs.parentNode.insertBefore(js,fjs);}}(document,"script","twitter-wjs");</script>







