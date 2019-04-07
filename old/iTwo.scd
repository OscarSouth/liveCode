// s.options.numBuffers = 1024*3;
// s.options.numBuffers = 4096*256;
// s.options.memSize = (512*1024);

s.options.numBuffers = 1024 * 128; // increase this if you need to load more samples
s.options.memSize = 1024 * 128; // increase this if you get "alloc failed" messages
s.options.maxNodes = 1024 * 128; // increase this if you are getting drop outs and the message "too many nodes"
s.options.device = "PreSonus AudioBox iTwo";
s.options.numOutputBusChannels = 16;
s.waitForBoot {
	~dirt = SuperDirt(2, s); // two output channels, increase if you want to pan across more channels
	// ~dirt.loadSoundFiles("/home/oscarsouth/dirt_samples/*");
	s.sync; // wait for samples to be read
	~dirt.start(57120, [0,2,4,6,8]);   // start listening on port 57120, create N busses each sending audio to channel 0
	MIDIClient.init;
	// MIDIClient.destinations;
	~midiOut = MIDIOut.newByName("PreSonus AudioBox iTwo", "PreSonus AudioBox iTwo MIDI 1");
	~latency = 0.0;
	~midiOut.latency = ~latency;
	~dirt.soundLibrary.addMIDI(\midi, ~midiOut);
};