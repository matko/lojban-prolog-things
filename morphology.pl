:- module(morphology, []).
:- use_module(peg).
:- use_module(peg_syntax).

words_strings(Ws) <-- 
    words(Ws_Code),
    { once(maplist([C,S]>>(string_codes(S,C)), Ws_Code, Ws)) }.

words(Ws) <--
  pause?,
  words_tail(Ws).

words_tail([W|Ws]) <--
  !,
  word(W),
  pause?,
  words_tail(Ws)? .

words_tail([]) <-- [].

word(W) <--
  match(W, lojban_word), spaces? .
  

any_word <--
  lojban_word, spaces? .

lojban_word <--
  cmevla
/ cmavo
/ brivla_word.

brivla_word <--
  gismu
/ fuhivla
/ lujvo.

%% cmene
cmevla <--
  jbocme
/ zifcme.

zifcme <--
  !h,
  ( nucleus
  / glide
  / h
  / consonant, !pause
  / digit)*,
  consonant,
  &pause.

jbocme <--
  &zifcme,
  ( any_syllable
  / digit)+,
  &pause.

%% cmavo
cmavo <--
  !cmevla, !cvcy_lujvo, cmavo_form, &post_word.

cvcy_lujvo <--
  cvc_rafsi, y, h?, initial_rafsi*, brivla_core
/ stressed_cvc_rafsi, y, short_final_rafsi.

cmavo_form <--
  !h, !cluster,
  onset,
  (nucleus, h)*,
  ( !stressed, nucleus
  / nucleus, !cluster)
/ y+
/ digit.

%% brivla
brivla <--
  !cmavo, initial_rafsi*, brivla_core.

brivla_core <--
  fuhivla
/ gismu
/ cvv_final_rafsi
/ stressed_initial_rafsi, short_final_rafsi.

stressed_initial_rafsi <--
  stressed_extended_rafsi
/ stressed_y_rafsi
/ stressed_y_less_rafsi.

initial_rafsi <--
  extended_rafsi
/ y_rafsi
/ !any_extended_rafsi,y_less_rafsi,!any_extended_rafsi.

%% fuhivla
any_extended_rafsi <--
  fuhivla
/ extended_rafsi
/ stressed_extended_rafsi.

fuhivla <--
  fuhivla_head,
  stressed_syllable,
  consonantal_syllable*,
  final_syllable.

stressed_extended_rafsi <--
  stressed_brivla_rafsi
/ stressed_fuhivla_rafsi.

extended_rafsi <--
  brivla_rafsi
/ fuhivla_rafsi.

stressed_brivla_rafsi <--
  &unstressed_syllable,
  brivla_head,
  stressed_syllable,
  h, y.

brivla_rafsi <--
  &((syllable, consonantal_syllable*, syllable)),
  brivla_head,
  h, y, h? .

stressed_fuhivla_rafsi <--
  fuhivla_head,
  stressed_syllable,
  consonantal_syllable*,
  !h,
  onset,
  y.

fuhivla_rafsi <--
  &unstressed_syllable, fuhivla_head,
  !h, onset, y, h? .

fuhivla_head <--
  !rafsi_string, brivla_head.

brivla_head <--
  !cmavo, !slinkuhi, !h,
  &onset, unstressed_syllable* .

slinkuhi <--
  !rafsi_string, consonant, rafsi_string.

rafsi_string <--
  y_less_rafsi*,
  ( gismu
  / cvv_final_rafsi
  / stressed_y_less_rafsi, short_final_rafsi
  / y_rafsi
  / stressed_y_rafsi
  / stressed_y_less_rafsi?, initial_pair, y
  / hy_rafsi
  / stressed_hy_rafsi).

%% gismu
gismu <--
  ( initial_pair, stressed_vowel
  / consonant, stressed_vowel, consonant),
  &final_syllable, consonant, vowel, &post_word.

cvv_final_rafsi <--
  consonant,
  stressed_vowel,
  h,
  &final_syllable, vowel,
  &post_word.

short_final_rafsi <--
  &final_syllable,
  ( consonant, diphthong
  / initial_pair, vowel),
  &post_word.

stressed_y_rafsi <--
  ( stressed_long_rafsi
  / stressed_cvc_rafsi),
  y.

stressed_y_less_rafsi <--
  stressed_cvc_rafsi, !y
/ stressed_ccv_rafsi
/ stressed_cvv_rafsi.

stressed_long_rafsi <--
  initial_pair, stressed_vowel, consonant
/ consonant, stressed_vowel, consonant, consonant.

stressed_cvc_rafsi <--
  consonant, stressed_vowel, consonant.

stressed_ccv_rafsi <--
  initial_pair, stressed_vowel.

stressed_cvv_rafsi <--
  consonant,
  ( unstressed_vowel, h, stressed_vowel
  / stressed_diphthong),
  r_hyphen? .

y_rafsi <--
  ( long_rafsi
  / cvc_rafsi),
  y,
  h? .

y_less_rafsi <--
  !y_rafsi, !stressed_y_rafsi, !hy_rafsi, !stressed_hy_rafsi,
  ( cvc_rafsi
  / ccv_rafsi
  / cvv_rafsi),
  !h.

hy_rafsi <--
  ( long_rafsi, vowel
  / ccv_rafsi
  / cvv_rafsi),
  h, y, h? .

stressed_hy_rafsi <--
  ( long_rafsi, stressed_vowel
  / stressed_ccv_rafsi
  / stressed_cvv_rafsi),
  h, y.

long_rafsi <--
  initial_pair, unstressed_vowel, consonant
/ consonant, unstressed_vowel, consonant, consonant.

cvc_rafsi <--
  consonant, unstressed_vowel, consonant.

ccv_rafsi <--
  initial_pair, unstressed_vowel.

cvv_rafsi <--
  consonant,
  ( unstressed_vowel, h, unstressed_vowel
  / unstressed_diphthong),
  r_hyphen? .

r_hyphen <--
  r, &consonant
/ n, &r.

%% syllable
final_syllable <--
  onset,
  !y,
  !stressed,
  nucleus,
  !cmevla,
  &post_word.

stressed_syllable <--
  &stressed, syllable
/ syllable, &stress.

stressed_diphthong <--
  &stressed, diphthong
/ dipthong, &stress.

stressed_vowel <--
  &stressed, vowel
/ vowel, &stress.

unstressed_syllable <--
  !stressed, syllable, !stress
/ consonantal_syllable.

unstressed_diphthong <--
  !stressed, diphthong, !stress.

unstressed_vowel <--
  !stressed, vowel, !stress.

stress <--
  ( consonant / glide)*,
  h?,
  y?,
  syllable,
  pause.

stressed <--
  onset, comma*,
  ( `A`/`E`/`I`/`O`/`U` ).

any_syllable <--
  onset, nucleus, coda?
/ consonantal_syllable.

syllable <--
  onset, !y, nucleus, coda? .

consonantal_syllable <--
  consonant, &syllabic, coda.

coda <--
  !any_syllable, consonant, &any_syllable
/ syllabic?, consonant?, &pause.

onset <--
  h
/ glide
/ initial.

nucleus <--
  vowel
/ diphthong
/ y, !nucleus.

%% vowels
glide <--
  (i/u),
  &nucleus.

diphthong <--
  ( a, i, !i
  / a, u, !u
  / e, i, !i
  / o, i, !i),
  !nucleus.

vowel <--
  ( a/e/i/o/u),
  !nucleus.

a <--
  comma*,
  (`a`/`A`).

e <--
  comma*,
  (`e`/`E`).

i <--
  comma*,
  (`i`/`I`).

o <--
  comma*,
  (`o`/`O`).

u <--
  comma*,
  (`u`/`U`).

y <--
  comma*,
  (`y`/`Y`).

%% consonant
cluster <--
  consonant,
  consonant+ .

initial_pair <--
  &initial, consonant, consonant, !consonant.

initial <--
  (affricate/sibilant?, other?, liquid?),
  !consonant,
  !glide.

affricate <--
  t, c
/ t, s
/ d, j
/ d, z.

liquid <--
  l
/ r.

other <--
  p
/ t, !l
/ k
/ f
/ x
/ b
/ d, !l
/ g
/ v
/ m
/ n, !liquid.

sibilant <--
  c
/ s, !x
/ (j / z), !n, !liquid.  

consonant <--
  voiced
/ unvoiced
/ syllabic.

syllabic <--
  l/m/n/r.

voiced <--
  b/d/g/j/v/z.

unvoiced <--
  c/f/k/p/s/t/x.

l <--
  comma*,
  (`l`|`L`),
  !h,
  !glide,
  !l.

m <--
  comma*,
  (`m`|`M`),
  !h,
  !glide,
  !m,
  !z.

n <--
  comma*,
  (`n`|`N`),
  !h,
  !glide,
  !n,
  !affricate.

r <--
  comma*,
  (`r`|`R`),
  !h,
  !glide,
  !r.

b <--
  comma*,
  (`b`|`B`),
  !h,
  !glide,
  !b,
  !unvoiced.

d <--
  comma*,
  (`d`|`D`),
  !h,
  !glide,
  !d,
  !unvoiced.

g <--
  comma*,
  (`g`|`G`),
  !h,
  !glide,
  !g,
  !unvoiced.

v <--
  comma*,
  (`v`|`V`),
  !h,
  !glide,
  !v,
  !unvoiced.

j <--
  comma*,
  (`j`|`J`),
  !h,
  !glide,
  !j,
  !z,
  !unvoiced.

z <--
  comma*,
  (`z`|`Z`),
  !h,
  !glide,
  !z,
  !j,
  !unvoiced.

s <--
  comma*,
  (`s`|`S`),
  !h,
  !glide,
  !s,
  !c,
  !voiced.

c <--
  comma*,
  (`c`|`C`),
  !h,
  !glide,
  !c,
  !s,
  !x,
  !voiced.

x <--
  comma*,
  (`x`|`X`),
  !h,
  !glide,
  !x,
  !c,
  !k,
  !voiced.

k <--
  comma*,
  (`k`|`K`),
  !h,
  !glide,
  !k,
  !x,
  !voiced.

f <--
  comma*,
  (`f`|`F`),
  !h,
  !glide,
  !f,
  !voiced.

p <--
  comma*,
  (`p`|`P`),
  !h,
  !glide,
  !p,
  !voiced.

t <--
  comma*,
  (`t`|`T`),
  !h,
  !glide,
  !t,
  !voiced.

h <--
  comma*,
  (`'`|`h`|`’`),
  &nucleus.

%% other
digit <--
  comma*,
  (`0`/`1`/`2`/`3`/`4`/`5`/`6`/`7`/`8`/`9`).

post_word <--
  pause
/ !nucleus, lojban_word.

pause <--
  comma*, space_char+
/ eof.

eof <-- comma*, !.
comma <-- `,`.

non_space <--
  !space_char,
  [_].

space_char <--
  [X],
  { member(X, `.\t\n\r?! -`) }.

%% stuff ?
spaces <--
  selmaho_y, initial_spaces.

initial_spaces <--
  ( comma*, space_char
  / !ybu, selmao_y)+,
  eof?
/ eof.

ybu <--
  selmao_y, space_char*, selmao_bu.

lujvo <--
  !gismu, !fuhivla, brivla.

%% selmaho
selmaho_bu <--
  &cmavo, b, u, &post_word.

selmaho_y <--
  &cmavo, y+, &post_word.
