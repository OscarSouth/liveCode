:set -XOverloadedStrings
:set prompt ""
:module Sound.Tidal.Context

import Sound.Tidal.Scales
import qualified Sound.Tidal.Chords as Chord
import MusicData
import qualified MusicData ( NoteName(F) )

(cps, nudger, getNow) <- cpsUtils'

(d1,t1) <- superDirtSetters getNow
(d2,t2) <- superDirtSetters getNow
(d3,t3) <- superDirtSetters getNow
(d4,t4) <- superDirtSetters getNow
(d5,t5) <- superDirtSetters getNow
(d6,t6) <- superDirtSetters getNow
(d7,t7) <- superDirtSetters getNow
(d8,t8) <- superDirtSetters getNow
(d9,t9) <- superDirtSetters getNow

(midicmd, midicmd_p) = pS "midicmd" (Nothing)
(midichan, midichan_p) = pF "midichan" (Nothing)
(progNum, progNum_p) = pF "progNum" (Nothing)
(val, val_p) = pF "val" (Nothing)
(uid, uid_p) = pF "uid" (Nothing)
(array, array_p) = pF "array" (Nothing)
(frames, frames_p) = pF "frames" (Nothing)
(seconds, seconds_p) = pF "seconds" (Nothing)
(minutes, minutes_p) = pF "minutes" (Nothing)
(hours, hours_p) = pF "hours" (Nothing)
(frameRate, frameRate_p) = pF "frameRate" (Nothing)
(songPtr, songPtr_p) = pF "songPtr" (Nothing)
(ctlNum, ctlNum_p) = pF "ctlNum" (Nothing)
(control, control_p) = pF "control" (Nothing)

let bps x = cps (x/2)
let bpm n = cps (n/60/4)
let hush = mapM_ ($ silence) [d1,d2,d3,d4,d5,d6,d7,d8,d9]
let solo = (>>) hush
let n \\\ s = toScale $ fromIntegral . (+ i n) . toInteger <$> s

-- toDo: create a function that takes a keySig and two patterns
-- that modally transposes the first pattern by the second

let master = 1
let keySig = B \\\ aeolian
let ch n = (s "midi" #midichan (n - 1))
let midiScale n = 0.9 + n*0.03
let vel n = gain $ midiScale n
let vel' n = gain $ n*0.03
let humanise n = gain $ (scale (-0.01 * n) (0.01 * n) $ rand)
let oct n = note (n*12)
let oct' n = note (n*7)
let ccScale n = (n/9) * 127
let cc' c n val = control (ccScale val) #io n c where io n c = (s "midi" #midicmd "control" #midichan (c-1) #ctlNum (n))
let lfo wave lo hi = _discretise 128 $ scale lo hi $ wave
let lfo' wave lo hi = _discretise 128 $ scalex (s lo) (s hi) $ wave where s n | n > 0 = n | n <= 0 = 0.001
let cc n val = control (ccScale val) #io n where io n = (midicmd "control" #ctlNum n)
let ped' = cc' 1 64
let ped = cc 64

let runWithTempo bpm = cps (-1) >> cps (bpm/60/4)

let sub p = n p #midinote 49 #ch 9
let bd p = n p #midinote 51 #ch 9
let cp p = n p #midinote 56 #ch 9
let sn p = n p #midinote 54 #ch 9
let hh p = n p #midinote 58 #ch 9
let oh p = n p #midinote 61 #ch 9
let pc p = n p #midinote 63 #ch 9

let clhat p = n p #midinote 58 #ch 9
let perc p = n p #midinote 63 #ch 9

let midi = s "midi"
let din = s "din"
let sync = s "sync"
let thru = s "thru"
let midithru = s "[midi, thru]"
let click = s "[midi, click]"
let click' = s "click"
let bar b1 b2 p = (b1+2, b2+3, p)
let begin = (0, 1, silence)
let out = 4
let clockStart = (1, 2, midicmd "start" #sync)
let clockRun = (1, out+2, midicmd "midiClock*24" #sync)
let clockStop = (out+1, out+2, midicmd "stop" #sync)

:{
let metronome =
      stack[silence
        ,n "0*4" #midinote 66
        ,cc 87 "2.8 0 . 0 0 . 0 2.8"
        ] #ch 9
:}

:{
let sonClave32 =
      stack[silence
        ,n "0(3,8) . ~ 0 0 ~" #midinote "66"
        ,cc 87 0
        ] #ch 9
:}

let sonClave23 = 0.5 <~ sonClave32

:{
let rumbaClave32 =
      stack[silence
        ,n "0 [~ 0] ~ [~ 0] . ~ 0 0 ~" #midinote "66"
        ,cc 87 0
        ] #ch 9
:}

let rumbaClave23 = 0.5 <~ rumbaClave32

:set prompt "VEDMλ | "
