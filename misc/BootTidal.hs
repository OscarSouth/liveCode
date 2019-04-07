:set -XOverloadedStrings
:set prompt ""
:set prompt-cont ""

import Sound.Tidal.Context

-- total latency = oLatency + cFrameTimespan
tidal <- startTidal (superdirtTarget {oLatency = 0.2, oAddress = "127.0.0.1", oPort = 57120}) (defaultConfig {cFrameTimespan = 1/20})

let p = streamReplace tidal
let hush' = streamHush tidal
let mute = streamMute tidal
let unmute = streamUnmute tidal
let solo = streamSolo tidal
let unsolo = streamUnsolo tidal
let once = streamOnce tidal False
let asap = streamOnce tidal True
let nudgeAll = streamNudgeAll tidal
let setcps = asap . cps
let xfade = transition tidal (Sound.Tidal.Transition.xfadeIn 4)
let xfadeIn t = transition tidal (Sound.Tidal.Transition.xfadeIn t)
let histpan t = transition tidal (Sound.Tidal.Transition.histpan t)
let wait t = transition tidal (Sound.Tidal.Transition.wait t)
let waitT f t = transition tidal (Sound.Tidal.Transition.waitT f t)
let jump = transition tidal (Sound.Tidal.Transition.jump)
let jumpIn t = transition tidal (Sound.Tidal.Transition.jumpIn t)
let jumpIn' t = transition tidal (Sound.Tidal.Transition.jumpIn' t)
let jumpMod t = transition tidal (Sound.Tidal.Transition.jumpMod t)
let mortal lifespan release = transition tidal (Sound.Tidal.Transition.mortal lifespan release)
let interpolate = transition tidal (Sound.Tidal.Transition.interpolate)
let interpolateIn t = transition tidal (Sound.Tidal.Transition.interpolateIn t)
let clutch = transition tidal (Sound.Tidal.Transition.clutch)
let clutchIn t = transition tidal (Sound.Tidal.Transition.clutchIn t)
let anticipate = transition tidal (Sound.Tidal.Transition.anticipate)
let anticipateIn t = transition tidal (Sound.Tidal.Transition.anticipateIn t)
let d1 = p "1"
let d2 = p "2"
let d3 = p "3"
let d4 = p "4"
let d5 = p "5"
let d6 = p "6"
let d7 = p "7"
let d8 = p "8"
let d9 = p "9"
let d10 = p "10"
let d11 = p "11"
let d12 = p "12"
let d13 = p "13"
let d14 = p "14"
let d15 = p "15"
let d16 = p "16"
let hush = mapM_ ($ silence) [d1,d2,d3,d4,d5,d6,d7,d8,d9,d10,d11,d12,d13,d14,d15,d16] -- >> click 0

let resetCycles = streamResetCycles tidal
let runWith f = resetCycles >> f
let motion = p "" silence
let midi = s "midi"
let ch n = (s "midi" #midichan (n - 1))
let bar b1 b2 p = (b1+2, b2+3, p)
let sync = (0, 1, silence)
let out = 4
let setCC c n val = once $ control (val) #io n c where io n c = (s "midi" #midicmd "control" #midichan (c-1) #ctlNum (n))
let setCC' c n val = control (val) #io n c where io n c = (s "midi" #midicmd "control" #midichan (c-1) #ctlNum (n))
let startTransport = bar 0 0 (setCC' 1 1 127)
let startTransport' = setCC' 1 1 127
let stopTransport out = bar (out+1) (out+1) (setCC' 1 1 0)
let stopTransport' = setCC' 1 1 0
let stopTransportNow = once stopTransport' >> hush >> (p "transport" $ silence)
let midiScale n = 0.9 + n*0.03
let ccScale n = (n/9) * 127
let cc n val = control (ccScale val) #io n where io n = (midicmd "control" #ctlNum n)
let cc' c n val = control (ccScale val) #io n c where io n c = (s "midi" #midicmd "control" #midichan (c-1) #ctlNum (n))
let setCC c n val = once $ control (val) #io n c where io n c = (s "midi" #midicmd "control" #midichan (c-1) #ctlNum (n))
let setCC' c n val = control (val) #io n c where io n c = (s "midi" #midicmd "control" #midichan (c-1) #ctlNum (n))
let lfo wave lo hi = segment 128 $ range lo hi wave
let lfo' wave lo hi = segment 128 $ rangex (s lo) (s hi) $ wave where s n | n > 0 = n | n <= 0 = 0.001
let ped = cc 64
let vel n = gain $ midiScale n
let vel' n = gain $ n*0.03

let humanise n = gain $ (range (-0.01 * n) (0.01 * n) $ rand)
:{
andante = do
    setCC 1 2 127
    setcps (85/60/4)
:}

:{
moderato = do
    setCC 1 3 127
    setcps (110/60/4)
:}

:{
allegro = do
    setCC 1 4 127
    setcps (135/60/4)
:}

:{
let metronome =
      stack[silence
        ,n "0*4" #midinote 66
        ,cc 87 "2.8 0 . 0 0 . 0 2.8"
        ] #ch 10
:}

let click val = setCC 10 90 (val*127)

:{
let son32 =
      stack[silence
        ,n "0(3,8) . ~ 0 0 ~" #midinote "66"
        ,cc 87 0
        ] #ch 10
:}

let son23 = 0.5 <~ son32

:{
let rumba32 =
      stack[silence
        ,n "0 [~ 0] ~ [~ 0] . ~ 0 0 ~" #midinote "66"
        ,cc 87 0
        ] #ch 10
:}

let rumba23 = 0.5 <~ rumba32

:{
let bossa32 =
      stack[silence
        ,n "0(3,8) . ~ 0 [~ 0] ~" #midinote "66"
        ,cc 87 0
        ] #ch 10
:}

let bossa23 = 0.5 <~bossa32

-- MOOG MODEL D BINDINGS

volume val = cc 7 (val*9)
modwheel val = cc 1 (val*9)
modsource val = cc 14 (val*9)
finetune val = cc 3 ((((val-(-2.5))*1)/5)*9)
glide val = cc 5 (val*9)
glideio val = cc 65 (val*9)
decayio val = cc 80 (val*9)
oscmod val = cc 30 (val*9)
osc3ctrl val = cc 29 (val*9)
osc1range val = cc 21 (val*9)
osc1wave val = cc 22 (val*9)
osc2range val = cc 23 (val*9)
osc2tune val = cc 24 (val*9)
osc2wave val = cc 25 (val*9)
osc3range val = cc 26 (val*9)
osc3tune val = cc 27 (val*9)
osc3wave val = cc 28 (val*9)
osc1vol val = cc 46 (val*9)
osc1io val = cc 47 (val*9)
osc2vol val = cc 48 (val*9)
osc2io val = cc 49 (val*9)
osc3vol val = cc 50 (val*9)
osc2io val = cc 51 (val*9)
feedbackvol val = cc 9 (val*9)
feedbackio val = cc 52 (val*9)
noisevol val = cc 54 (val*9)
noiseio val = cc 53 (val*9)
noisetype val = cc 55 (val*9)
filtermod val = cc 81 (val*9)
keyboard1 val = cc 82 (val*9)
keyboard2 val = cc 82 (val*9)
fresonance val = cc 71 (val*9)
fdecay val = cc 72 (val*9)
fattack val = cc 73 (val*9)
fcutoff val = cc 74 (val*9)
fcontour val = cc 75 (val*9)
fsustain val = cc 76 (val*9)
adecay val = cc 77 (val*9)
aattack val = cc 78 (val*9)
asustain val = cc 79 (val*9)

-- APP SWITCHING BINDINGS

let modelD = once $ cc' 1 101 9
let laplace = once $ cc' 1 102 9
let ruismaker = once $ cc' 1 103 9
let egoist = once $ cc' 1 104 9
let samplr = once $ cc' 1 105 9
let loopy = once $ cc' 1 106 9
let wow = once $ cc' 1 107 9
let kosmonaut = once $ cc' 1 108 9
let perforator = once $ cc' 1 109 9

:set prompt "tidal> "
