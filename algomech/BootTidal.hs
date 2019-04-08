:set -XOverloadedStrings
:set prompt ""
:set prompt-cont ""

import Sound.Tidal.Context
import Sound.Tidal.Scales

-- total latency = oLatency + cFrameTimespan
tidal <- startTidal (superdirtTarget {oLatency = 0.2, oAddress = "127.0.0.1", oPort = 57120}) (defaultConfig {cFrameTimespan = 1/20})

p = streamReplace tidal
hush' = streamHush tidal
mute = streamMute tidal
unmute = streamUnmute tidal
solo = streamSolo tidal
unsolo = streamUnsolo tidal
once = streamOnce tidal False
asap = streamOnce tidal True
nudgeAll = streamNudgeAll tidal
setcps = asap . cps
xfade = transition tidal (Sound.Tidal.Transition.xfadeIn 4)
xfadeIn t = transition tidal (Sound.Tidal.Transition.xfadeIn t)
histpan t = transition tidal (Sound.Tidal.Transition.histpan t)
wait t = transition tidal (Sound.Tidal.Transition.wait t)
waitT f t = transition tidal (Sound.Tidal.Transition.waitT f t)
jump = transition tidal (Sound.Tidal.Transition.jump)
jumpIn t = transition tidal (Sound.Tidal.Transition.jumpIn t)
jumpIn' t = transition tidal (Sound.Tidal.Transition.jumpIn' t)
jumpMod t = transition tidal (Sound.Tidal.Transition.jumpMod t)
mortal lifespan release = transition tidal (Sound.Tidal.Transition.mortal lifespan release)
interpolate = transition tidal (Sound.Tidal.Transition.interpolate)
interpolateIn t = transition tidal (Sound.Tidal.Transition.interpolateIn t)
clutch = transition tidal (Sound.Tidal.Transition.clutch)
clutchIn t = transition tidal (Sound.Tidal.Transition.clutchIn t)
anticipate = transition tidal (Sound.Tidal.Transition.anticipate)
anticipateIn t = transition tidal (Sound.Tidal.Transition.anticipateIn t)
d01 = p "1"
d02 = p "2"
d03 = p "3"
d04 = p "4"
d05 = p "5"
d06 = p "6"
d07 = p "7"
d08 = p "8"
d09 = p "9"
d10 = p "10"
d11 = p "11"
d12 = p "12"
d13 = p "13"
d14 = p "14"
d15 = p "15"
d16 = p "16"
hush = mapM_ ($ silence) [d01,d02,d03,d04,d05,d06,d07,d08,d09,d10,d11,d12,d13,d14,d15,d16] -- >> click 0

n \\\ s = toScale $ fromIntegral . (+ i n) . toInteger <$> s
r /// m = (r \\\ m, transpose) where transpose = toScale m

resetCycles = streamResetCycles tidal
runWith f = resetCycles >> f
motion = p "" silence
midi = s "din"
ch n = (midi #midichan (n - 1))
bar b1 b2 p = (b1+2, b2+3, p)
sync = (0, 1, silence)
out = 4
setCC c n val = once $ control (val) #io n c where io n c = (midi #midicmd "control" #midichan (c-1) #ctlNum (n))
setCC' c n val = control (val) #io n c where io n c = (midi #midicmd "control" #midichan (c-1) #ctlNum (n))
startTransport = bar 0 0 (setCC' 6 50 127)
startTransport' = setCC' 6 00 127
stopTransport out = bar (out+1) (out+1) (setCC' 6 50 0)
stopTransport' = setCC' 6 50 0
stopTransportNow = once stopTransport' >> hush >> (p "transport" $ silence)
midiScale n = 0.9 + n*0.03
ccScale = (*127)
cc n val = control (ccScale val) #io n where io n = (midicmd "control" #ctlNum n)
cc' c n val = control (ccScale val) #io n c where io n c = (midi #midicmd "control" #midichan (c-1) #ctlNum (n))
setCC c n val = once $ control (val) #io n c where io n c = (midi #midicmd "control" #midichan (c-1) #ctlNum (n))
setCC' c n val = control (val) #io n c where io n c = (midi #midicmd "control" #midichan (c-1) #ctlNum (n))
lfo wave lo hi = segment 64 $ range lo hi wave
lfo' wave lo hi = segment 64 $ rangex (s lo) (s hi) $ wave where s n | n > 0 = n | n <= 0 = 0.001
ped = cc 64
vel = pF "amp"
humanise n = vel $ (range (-0.09 * n) (0.09 * n) $ rand)
patToList n pat = fmap value $ queryArc pat (Arc 0 n)
pullBy = (<~)
pushBy = (~>)
(|=) = (#)
prog bars keyPat = note (slow bars $ keyPat)
var each v p = pullBy 1 $ every each v p
inKey k b p = note (slow b $ k p)


on1 = within (0,0.25)
up1 = within (0.125,0.25)
on2 = within (0.25,0.5)
up2 = within (0.375,0.5)
on3 = within (0.5,0.75)
up3 = within (0.625,0.75)
on4 = within (0.75,1)
up4 = within (0.825,1)

:{
adagio = do
    setCC 6 51 127
    setcps (65/60/4)
:}
:{
andante = do
    setCC 6 52 127
    setcps (85/60/4)
:}
:{
moderato = do
    setCC 6 53 127
    setcps (110/60/4)
:}
:{
allegro = do
    setCC 6 54 127
    setcps (135/60/4)
:}
:{
metronome =
    stack[silence
      ,n "0*4" #midinote 66
      ,cc 87 "2.8 0 . 0 0 . 0 2.8"
      ] #ch 09
:}
click val = setCC 9 90 (val*127)
:{
son32 =
    stack[silence
      ,n "0(3,8) . ~ 0 0 ~" #midinote "66"
      ,cc 87 0
      ] #ch 09
:}
son23 = 0.5 <~ son32
:{
rumba32 =
    stack[silence
      ,n "0 [~ 0] ~ [~ 0] . ~ 0 0 ~" #midinote "66"
      ,cc 87 0
      ] #ch 09
:}
rumba23 = 0.5 <~ rumba32
:{
bossa32 =
    stack[silence
      ,n "0(3,8) . ~ 0 [~ 0] ~" #midinote "66"
      ,cc 87 0
      ] #ch 09
:}
bossa23 = 0.5 <~ bossa32

-- MODEL D BINDINGS
volume val = cc 7 (val)
modwheel val = cc 1 (val)
modsource val = cc 14 (val)
finetune val = cc 3 (((val-(-2.5))*1)/5)
glide val = cc 5 (val)
modlfo val = cc 4 (val)
glideio val = cc 65 (val)
decayio val = cc 80 (val)
oscmod val = cc 30 (val)
osc3ctrl val = cc 29 (val)
osc1range val = cc 21 (val)
osc1wave val = cc 22 (val)
osc2range val = cc 23 (val)
osc2tune val = cc 24 (((val-(-8))*1)/16)
osc2wave val = cc 25 (val)
osc3range val = cc 26 (val)
osc3tune val = cc 27 (((val-(-8))*1)/16)
osc3wave val = cc 28 (val)
osc1vol val = cc 46 (val)
osc1io val = cc 47 (val)
osc2vol val = cc 48 (val)
osc2io val = cc 49 (val)
osc3vol val = cc 50 (val)
osc3io val = cc 51 (val)
feedbackvol val = cc 9 (val)
feedbackio val = cc 52 (val)
noisevol val = cc 54 (val)
noiseio val = cc 53 (val)
noisetype val = cc 55 (val)
filtermod val = cc 81 (val)
keyboard1 val = cc 82 (val)
keyboard2 val = cc 83 (val)
fresonance val = cc 71 (val)
fdecay val = cc 72 (val)
fattack val = cc 73 (val)
fcutoff val = cc 74 (val)
fcontour val = cc 75 (val)
fsustain val = cc 76 (val)
adecay val = cc 77 (val)
aattack val = cc 78 (val)
asustain val = cc 79 (val)

-- MERSENNE BINDINGS
toneA lvl atk rel = stack [(cc 2 lvl), (cc 3 atk), (cc 4 rel)] #ch 14
toneB lvl atk rel = stack [(cc 5 lvl), (cc 6 atk), (cc 7 rel)] #ch 14

-- LAPLACE BINDINGS


sb p = n p #midinote 49 #vel 0.4 #ch 9
bd p = n p #midinote 51 #vel 0.4 #ch 9
sn p = n p #midinote 54 #vel 0.4 #ch 9
cp p = n p #midinote 56 #vel 0.4 #ch 9
hh p = n p #midinote 58 #vel 0.4 #ch 9
oh p = n p #midinote 61 #vel 0.4 #ch 9
fm p = n p #midinote 63 #vel 0.4 #ch 9

kick p = n p #note 1 #ch 10 #vel 0.4
snare p = n p #note 2 #ch 10 #vel 0.4
click p = n p #note 3 #ch 10 #vel 0.4
rim p = n p #note 4 #ch 10 #vel 0.4
hpedal p = n p #note 5 #ch 10 #vel 0.4
hclosed p = n p #note 6 #ch 10 #vel 0.4
hhalf p = n p #note 7 #ch 10 #vel 0.4
hopen p = n p #note 8 #ch 10 #vel 0.4
ride p = n p #note 9 #ch 10 #vel 0.4
ridebell p = n p #note 10 #ch 10 #vel 0.4
crash p = n p #note 11 #ch 10 #vel 0.4
crashbell p = n p #note 12 #ch 10 #vel 0.4
hchoke p = n p #note 13 #ch 10 #vel 0.4
ridechoke p = n p #note 14 #ch 10 #vel 0.4
crashchoke p = n p #note 15 #ch 10 #vel 0.4
floortom p = n p #note 16 #ch 10 #vel 0.4

overtones = cc' 6 10
-- stochastic = cc' 6 9
granular = cc' 6 8
wavetable = cc' 7 2
tb303 = cc' 8 7
rhythmic n = stack[(cc' 6 7 n), (cc' 9 "[33, 41, 49, 57, 66, 74, 82]" n)]
voyager = cc' 6 2
modelD = cc' 12 2
laplace = cc' 13 3
mersenne = cc' 6 4
wurlitz = cc' 6 5
piano = cc' 6 (6*2)
clicktrack v = p "click" $ cc' 9 90 v

pickup xs = mapM_ (once) (xs $ lfo saw 1 0)

:{
mixer p =
    [overtones p
--    ,stochastic p
    ,granular p
    ,wavetable p
    ,tb303 p
    ,rhythmic p
    ,voyager p
    ,modelD p
    ,laplace p
    ,mersenne p
    ,wurlitz p
    ,piano p
    ,click p
    ]
:}

hemidemisemiquaver = 1/64
demisemiquaver = 1/32
semiquaver = 1/16
quaver = 1/8
crotchet = 1/4
minim = 1/2

:{
ionian :: Num a => [a]
ionian = [0,2,4,5,7,9,11]
:}
:{
dorian :: Num a => [a]
dorian = [0,2,3,5,7,9,10]
:}
:{
phrygian :: Num a => [a]
phrygian = [0,1,3,5,7,8,10]
:}
:{
lydian :: Num a => [a]
lydian = [0,2,4,6,7,9,11]
:}
:{
mixolydian :: Num a => [a]
mixolydian = [0,2,4,5,7,9,10]
:}
:{
aeolian :: Num a => [a]
aeolian = [0,2,3,5,7,8,10]
:}
:{
locrian :: Num a => [a]
locrian = [0,1,3,5,6,8,10]
:}
:{
melMin :: Num a => [a]
melMin = [0,2,3,5,7,9,11]
:}
:{
melMin2 :: Num a => [a]
melMin2 = [0,1,3,5,7,9,10]
:}
:{
melMin3 :: Num a => [a]
melMin3 = [0,2,4,6,8,9,11]
:}
:{
melMin4 :: Num a => [a]
melMin4 = [0,2,4,6,7,9,10]
:}
:{
melMin5 :: Num a => [a]
melMin5 = [0,2,4,5,7,8,10]
:}
:{
melMin6 :: Num a => [a]
melMin6 = [0,2,3,5,6,8,10]
:}
:{
melMin7 :: Num a => [a]
melMin7 = [0,1,3,4,6,8,10]
:}
:{
harmMin :: Num a => [a]
harmMin = [0,2,3,5,7,8,11]
:}
:{
harmMin2 :: Num a => [a]
harmMin2 = [0,1,3,5,6,9,10]
:}
:{
harmMin3 :: Num a => [a]
harmMin3 = [0,2,4,5,8,9,11]
:}
:{
harmMin4 :: Num a => [a]
harmMin4 = [0,2,3,6,7,9,10]
:}
:{
harmMin5 :: Num a => [a]
harmMin5 = [0,1,4,5,7,8,10]
:}
:{
harmMin6 :: Num a => [a]
harmMin6 = [0,3,4,6,7,9,11]
:}
:{
harmMin7 :: Num a => [a]
harmMin7 = [0,1,3,4,6,8,9]
:}
:{
penta :: Num a => [a]
penta = [0,2,4,7,9]
:}
:{
penta2 :: Num a => [a]
penta2 = [0,2,5,7,10]
:}
:{
penta3 :: Num a => [a]
penta3 = [0,3,5,8,10]
:}
:{
penta4 :: Num a => [a]
penta4 = [0,2,5,7,9]
:}
:{
penta5 :: Num a => [a]
penta5 = [0,3,5,7,10]
:}
:{
dimWhole :: Num a => [a]
dimWhole = [0,2,3,5,6,8,9,11]
:}
:{
dimHalf :: Num a => [a]
dimHalf = [0,1,3,4,6,7,9,10]
:}
:{
wholeTone :: Num a => [a]
wholeTone = [0,2,4,6,8,10]
:}

runSilence p0 = p "silence" silence
runSilence' = p "silence" silence
p01 = runSilence'
p02 = runSilence'
p03 = runSilence'
p04 = runSilence'
p05 = runSilence'
p06 = runSilence
p07 = runSilence
p08 = runSilence
p09 = runSilence
p10 = runSilence
p11 = runSilence
p12 = runSilence
p13 = runSilence
p14 = runSilence
p15 = runSilence
p16 = runSilence

:{
:}

:set prompt "tidal> "
