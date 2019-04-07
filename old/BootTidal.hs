:set -XOverloadedStrings
:module Sound.Tidal.Context

import Sound.Tidal.Scales
import qualified Sound.Tidal.Chords as Chord
import MusicData
import qualified MusicData ( NoteName(F) )

:set prompt "λ | "

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
    hush = mapM_ ($ silence) [d1,d2,d3,d4,d5,d6,d7,d8,d9]

let h1 = d1 $ silence
    h2 = d2 $ silence
    h3 = d3 $ silence
    h4 = d4 $ silence
    h5 = d5 $ silence
    h6 = d6 $ silence
    h7 = d7 $ silence
    h8 = d8 $ silence
    h9 = d9 $ silence

let solo = (>>) hush
    n \\\ s = toScale $ fromIntegral . (+ i n) . toInteger <$> s

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

let harmonics = cc' 16 85
let slicer = cc' 16 20
let organ = cc' 16 21
let winds = cc' 16 22
let strings = cc' 16 23
let ruismk = cc' 16 86
let drumkit = cc' 16 87
let analog = cc' 16 88
let polybass = cc' 16 26
let tbmk = cc' 16 25
let mersenne = cc' 16 24
let wurlitz = cc' 16 89
let modelD = cc' 16 26
let piano = cc' 16 90
let fxsendA = cc' 16 28
let fxsendB = cc' 16 29

let attack = cc' 16 27

let subkick p = n p #midinote 49 #ch 9
let kick p = n p #midinote 51 #ch 9
let clap p = n p #midinote 56 #ch 9
let snare p = n p #midinote 54 #ch 9
let c'hat p = n p #midinote 58 #ch 9
let o'hat p = n p #midinote 61 #ch 9
let perc p = n p #midinote 63 #ch 9

let bd p = n p #note 1 #ch 10
let sn p = n p #note 2 #ch 10
let click p = n p #note 3 #ch 10
let rim p = n p #note 4 #ch 10
let hatpedal p = n p #note 5 #ch 10
let hatclosed p = n p #note 6 #ch 10
let hathalf p = n p #note 7 #ch 10
let hatopen p = n p #note 8 #ch 10
let ride p = n p #note 9 #ch 10
let ridebell p = n p #note 10 #ch 10
let crash p = n p #note 11 #ch 10
let crashbell p = n p #note 12 #ch 10
let hatchoke p = n p #note 13 #ch 10
let ridechoke p = n p #note 14 #ch 10
let crashchoke p = n p #note 15 #ch 10
let floortom p = n p #note 16 #ch 10

let ped' = cc' 1 64
let ped = cc 64

let one = note "0"
let up = note "~ 0"
