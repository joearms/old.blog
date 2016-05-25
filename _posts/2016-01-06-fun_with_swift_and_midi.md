---
layout: post
title: Fun with Swift and Midi
tags: swift midi synth
year: 2016
month: 1
day: 6
published: true
---

Seems like I can control core audio directly from the command line in Swift.
no horrid Xcode is involved.

> One quick example seems to be worth a thousand words, so here's a
version of `PlaySoftMidi.cpp` converted to Swift.

*PlaySoftMidi* has
been in the Mac Developer library since 2007 and is just about the
only program I can understand that *uses* the CoreAudio stuff.

If you have swift2.1 on your machine save `PlaySoftMidi.swift` in a file and
give the command:

    swift PlaySoftMidi.swift

And the program will play samples from the inbuilt synthesizer.

Here are the programs:

+ [PLaySoftMidi.cpp](https://developer.apple.com/library/mac/samplecode/PlaySoftMIDI/Listings/main_cpp.html) -- Apple origonal program
+ [PlaySoftMidi.swift](https://github.com/joearms/joearms.github.com/tree/master/_posts/swift/PlaySoftMidi.swift) -- The above converted to Swift and modified

These two program also show the equivalence between `C++` and `Swift`


# The Bad

For example in C++ we say:

<pre>
enum {
    kMidiMessage_ControlChange      = 0xB,
    kMidiMessage_ProgramChange      = 0xC,
    kMidiMessage_BankMSBControl     = 0,
    kMidiMessage_BankLSBControl     = 32,
    kMidiMessage_NoteOn             = 0x9
};
</pre>

And to use a constant just refer to it by name.

And In Swift we say:

<pre>
enum Midi : UInt32 {
    case kMidiMessage_ControlChange = 0xB
    case kMidiMessage_ProgramChange = 0xC
    case kMidiMessage_BankMSBControl = 0
    case kMidiMessage_BankLSBControl = 32
    case kMidiMessage_NoteOn = 0x9
    case kMidiMessageProgramChange = 0xC0
}
</pre>

And to use a value say something like: `Midi.kMidiMessageProgramChange.rawValue`

I hope this is not a trend - just when you though programming language
couldn't get any more verbose, they did.

# The Good

Type casts seem to be functions. If `x` in Swift is an `Int` and I
want to convert it to a variable `y` of type `UInt32` I say:

    y = UInt32(x)

And NOT `y = (UInt32) x` -- *horay* say I.

# Next Steps

When I've got a socket interface working I'll connect this to Erlang so that I can
hit the midi interface from Erlang -- then the fun can begin.


