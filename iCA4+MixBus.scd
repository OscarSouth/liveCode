// s.options.numBuffers = 1024 * 4; // increase this if you need to load more samples
// s.options.memSize = 1024 * 128; // increase this if you get "alloc failed" messages
// s.options.maxNodes = 1024 * 128; // increase this if you are getting drop outs and the message "too many nodes"
s.options.numBuffers = 4096 * 16;
s.options.memSize = 8192 * 16;
s.options.maxNodes = 1024 * 32;
// s.options.device = "iConnectAUDIO4+";
s.options.numOutputBusChannels = 16;
s.waitForBoot {
	~dirt = SuperDirt(2, s); // two output channels
	~dirt.loadSoundFiles("/home/oscarsouth/liveCode/samples/*");
	s.sync; // wait for samples to be read
	~dirt.start(57120, [0,2,4,6,8]);   // start listening on port 57120, create N busses each sending audio to channel 0
	// MIDIClient.init;
	// MIDIClient.destinations;
	// ~midiDIN = MIDIOut.newByName("iConnectAUDIO4+", "iConnectAUDIO4+ MIDI 1");
	// ~midiOut = MIDIOut.newByName("iConnectAUDIO4+", "iConnectAUDIO4+ MIDI 2");
	// ~midiThru = MIDIOut.newByName("Midi Through", "Midi Through Port-0");
	// ~midiSync = MIDIOut.newByName("Midi Through", "Midi Through Port-1");
	// ~midiClick = MIDIOut.newByName("Midi Through", "Midi Through Port-2");
	// ~latency = 0.068;
	// ~midiDIN.latency = ~latency;
	// ~midiOut.latency = ~latency;
	// ~midiThru.latency = ~latency;
	// ~midiSync.latency = 0;
	// ~midiClick.latency = ~latency;
	// ~dirt.soundLibrary.addMIDI(\din, ~midiDIN);
	// ~dirt.soundLibrary.addMIDI(\midi, ~midiOut);
	// ~dirt.soundLibrary.addMIDI(\thru, ~midiThru);
	// ~dirt.soundLibrary.addMIDI(\sync, ~midiSync);
	// ~dirt.soundLibrary.addMIDI(\click, ~midiClick);
};

/*// s.options.numBuffers = 1024 * 4; // increase this if you need to load more samples
// s.options.memSize = 1024 * 128; // increase this if you get "alloc failed" messages
// s.options.maxNodes = 1024 * 128; // increase this if you are getting drop outs and the message "too many nodes"
s.options.numBuffers = 4096 * 16;
s.options.memSize = 8192 * 16;
s.options.maxNodes = 1024 * 32;
s.options.device = "iConnectAUDIO4+";
s.options.numOutputBusChannels = 16;
s.waitForBoot {
	~dirt = SuperDirt(2, s); // two output channels
	// ~dirt.loadSoundFiles("/home/oscarsouth/dirt_samples/*");
	s.sync; // wait for samples to be read
	~dirt.start(57120, [0,2,4,6,8]);   // start listening on port 57120, create N busses each sending audio to channel 0
	MIDIClient.init;
	// MIDIClient.destinations;
	~midiDIN = MIDIOut.newByName("iConnectAUDIO4+", "iConnectAUDIO4+ MIDI 1");
	~midiOut = MIDIOut.newByName("iConnectAUDIO4+", "iConnectAUDIO4+ MIDI 2");
	~midiThru = MIDIOut.newByName("Midi Through", "Midi Through Port-0");
	~midiSync = MIDIOut.newByName("Midi Through", "Midi Through Port-1");
	~midiClick = MIDIOut.newByName("Midi Through", "Midi Through Port-2");
	// ~latency = 0.068;
	~midiDIN.latency = ~latency;
	~midiOut.latency = ~latency;
	~midiThru.latency = ~latency;
	~midiSync.latency = 0;
	~midiClick.latency = ~latency;
	~dirt.soundLibrary.addMIDI(\din, ~midiDIN);
	~dirt.soundLibrary.addMIDI(\midi, ~midiOut);
	~dirt.soundLibrary.addMIDI(\thru, ~midiThru);
	~dirt.soundLibrary.addMIDI(\sync, ~midiSync);
	~dirt.soundLibrary.addMIDI(\click, ~midiClick);
};*/

