// http://www.rockhoppertech.com/blog/ios-audio-unit-graph/

import Foundation
import AudioToolbox
// import Darwin

let instruments : [String] =
   ["Acoustic Grand Piano", "Bright Acoustic Piano",
    "Electric Grand Piano", "Honky-tonk Piano",
    "Electric Piano 1", "Electric Piano 2", "Harpsichord",
    "Clavi", "Celesta", "Glockenspiel", "Music Box",
    "Vibraphone", "Marimba", "Xylophone", "Tubular Bells",
    "Dulcimer", "Drawbar Organ", "Percussive Organ",
    "Rock Organ", "Church Organ", "Reed Organ",
    "Accordion", "Harmonica", "Tango Accordion",
    "Acoustic Guitar (nylon)", "Acoustic Guitar (steel)",
    "Electric Guitar (jazz)", "Electric Guitar (clean)",
    "Electric Guitar (muted)", "Overdriven Guitar",
    "Distortion Guitar", "Guitar harmonics",
    "Acoustic Bass", "Electric Bass (finger)",
    "Electric Bass (pick)", "Fretless Bass",
    "Slap Bass 1", "Slap Bass 2", "Synth Bass 1",
    "Synth Bass 2", "Violin", "Viola", "Cello",
    "Contrabass", "Tremolo Strings", "Pizzicato Strings",
    "Orchestral Harp", "Timpani", "String Ensemble 1",
    "String Ensemble 2", "SynthStrings 1", "SynthStrings 2",
    "Choir Aahs", "Voice Oohs", "Synth Voice",
    "Orchestra Hit", "Trumpet", "Trombone", "Tuba",
    "Muted Trumpet", "French Horn", "Brass Section",
    "SynthBrass 1", "SynthBrass 2", "Soprano Sax",
    "Alto Sax", "Tenor Sax", "Baritone Sax", "Oboe",
    "English Horn", "Bassoon", "Clarinet", "Piccolo",
    "Flute", "Recorder", "Pan Flute", "Blown Bottle",
    "Shakuhachi", "Whistle", "Ocarina", "Lead 1 (square)",
    "Lead 2 (sawtooth)", "Lead 3 (calliope)", "Lead 4 (chiff)",
    "Lead 5 (charang)", "Lead 6 (voice)", "Lead 7 (fifths)",
    "Lead 8 (bass + lead)", "Pad 1 (new age)", "Pad 2 (warm)",
    "Pad 3 (polysynth)", "Pad 4 (choir)", "Pad 5 (bowed)",
    "Pad 6 (metallic)", "Pad 7 (halo)", "Pad 8 (sweep)",
    "FX 1 (rain)", "FX 2 (soundtrack)", "FX 3 (crystal)",
    "FX 4 (atmosphere)", "FX 5 (brightness)", "FX 6 (goblins)",
    "FX 7 (echoes)", "FX 8 (sci-fi)", "Sitar", "Banjo",
    "Shamisen", "Koto", "Kalimba", "Bag pipe", "Fiddle",
    "Shanai", "Tinkle Bell", "Agogo", "Steel Drums",
    "Woodblock", "Taiko Drum", "Melodic Tom", "Synth Drum",
    "Reverse Cymbal", "Guitar Fret Noise", "Breath Noise",
    "Seashore", "Bird Tweet", "Telephone Ring",
    "Helicopter", "Applause", "Gunshot"]

enum Midi : UInt32 {
    case kMidiMessage_ControlChange = 0xB
    case kMidiMessage_ProgramChange = 0xC
    case kMidiMessage_BankMSBControl = 0
    case kMidiMessage_BankLSBControl = 32
    case kMidiMessage_NoteOn = 0x9
    case kMidiMessageProgramChange = 0xC0
}

func set_voice(outSynth: AudioUnit, _ instrument:Int){
    print("instrument ", instruments[instrument]);
    MusicDeviceMIDIEvent(outSynth, Midi.kMidiMessageProgramChange.rawValue,
			 UInt32(instrument), 0, 0);
}
  

func play_notes(outSynth: AudioUnit, _ time:Int){
    let midiChannelInUse: UInt32 = 0; //we're using midi channel 1...
    for i: UInt32 in 0..<10 {
        let noteNum = i+60
        let onVelocity:UInt32 = 127
        let noteOnCommand:UInt32 = Midi.kMidiMessage_NoteOn.rawValue << 4 |
                                   midiChannelInUse
        MusicDeviceMIDIEvent(outSynth, noteOnCommand, noteNum, onVelocity, 0)
        usleep (1 * 1000 * UInt32(time))
        MusicDeviceMIDIEvent(outSynth, noteOnCommand, noteNum, 0, 0)
    }
}

func main(){
    var outGraph = AUGraph()
    var synthNode = AUNode()
    var limiterNode = AUNode()
    var outNode = AUNode()
    var outSynth = AudioUnit()
    var cd = AudioComponentDescription()
    var result = NewAUGraph(&outGraph)
    
    
    print("result1 =", result);

    cd.componentManufacturer = kAudioUnitManufacturer_Apple;
    cd.componentFlags = 0;
    cd.componentFlagsMask = 0;
    cd.componentType = kAudioUnitType_MusicDevice;
    cd.componentSubType = kAudioUnitSubType_DLSSynth;
    result = AUGraphAddNode(outGraph, &cd, &synthNode);
    print("result2 = ", result);

    cd.componentType = kAudioUnitType_Effect;
    cd.componentSubType = kAudioUnitSubType_PeakLimiter;  
    
    result = AUGraphAddNode(outGraph, &cd, &limiterNode);
    print("result3 = ", result);
    
    cd.componentType = kAudioUnitType_Output;
    cd.componentSubType = kAudioUnitSubType_DefaultOutput;  
    result = AUGraphAddNode (outGraph, &cd, &outNode);
    print("result4 = ", result);

    result = AUGraphOpen (outGraph);
    print("result5 = ", result);

    result = AUGraphConnectNodeInput (outGraph, synthNode, 0, limiterNode, 0);
    print("result6 = ", result);

    result = AUGraphConnectNodeInput (outGraph, limiterNode, 0, outNode, 0);
    print("result7 = ", result);
	
    // ok we're good to go - get the Synth Unit...
    
    result = AUGraphNodeInfo(outGraph, synthNode, nil, &outSynth);
    print("result8 = ", result);
  
    result = AUGraphInitialize (outGraph);
    print("result9 = ", result);
    
    // CAShow (outGraph); // prints out the graph so we can see what it looks like...
    	
    result = AUGraphStart (outGraph);
    print("result12 = ", result);

    set_voice(outSynth, 2) // piano
    play_notes(outSynth, 250)
    
    for instrument in 1..<127 {
        set_voice(outSynth, instrument)
        play_notes(outSynth, 25)
    }
}

main()
