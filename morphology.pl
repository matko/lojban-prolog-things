star(_X) --> [].
star(X) -->
    X,
    star(X).

plus(X) --> X.
plus(X) -->
    X,
    plus(X).

peek(X,A,A) :-
    phrase(X, A, _).

match(X, M, A, B) :-
    phrase(X, A, B),
    append(M, B, A).

optional(X,A,B) :-
    (   phrase(X, A, B)
    ->  true
    ;   A = B).

%% words
words_tail([W|Ws]) -->
    match(word,W),
    optional(pause),
    optional(words_tail(Ws)).
words_tail([]) --> [].

words(Ws) -->
    optional(pause),
    words_tail(Ws).

word --> lojban_word.
word --> non_lojban_word.

lojban_word --> cmene.
lojban_word --> cmavo.
lojban_word --> brivla.

brivla --> gismu.
brivla --> fuhivla.
brivla --> lujvo.

%% cmene
cmene --> jbocme.
cmene --> zifcme.

zifcme -->
    \+ h,
    star((   nucleus
         ;   glide
         ;   h
         ;   consonant, \+ pause
         ;   digit)),
    consonant,
    peek(pause).

jbocme -->
    peek(zifcme),
    star((   any_syllable
         ;   digit)),
    peek(pause).

%% cmavo
cmavo -->
    \+ cmene,
    \+ cvcy_lujvo,
    cmavo_form,
    peek(post_word).

cvcy_lujvo -->
    cvc_rafsi,
    y,
    optional(h),
    star(initial_rafsi),
    brivla_core.
cvcy_lujvo -->
    stressed_cvc_rafsi,
    y,
    short_final_rafsi.

cmavo_form -->
    \+ h,
    \+ cluster,
    onset,
    star((nucleus, h)),
    (   \+ stressed, nucleus
    ;   nucleus, \+ cluster).
cmavo_form -->
    plus(y).
cmavo_form -->
    digit.

%% lujvo
lujvo -->
    \+ gismu,
    \+ fuhivla,
    \+ cmavo,
    star(initial_rafsi),
    brivla_core.

brivla_core --> fuhivla.
brivla_core --> gismu.
brivla_core --> cvv_final_rafsi.
brivla_core -->
    stressed_initial_rafsi,
    short_final_rafsi.


stressed_initial_rafsi --> stressed_extended_rafsi.
stressed_initial_rafsi --> stressed_y_rafsi.
stressed_initial_rafsi --> stressed_y_less_rafsi.

initial_rafsi --> extended_rafsi.
initial_rafsi --> y_rafsi.
initial_rafsi -->
    \+ any_extended_rafsi,
    y_less_rafsi,
    \+ any_extended_rafsi.

any_extended_rafsi --> fuhivla.
any_extended_rafsi --> extended_rafsi.
any_extended_rafsi --> stressed_extended_rafsi.

%% fuhivla
fuhivla -->
    fuhivla_head,
    stressed_syllable,
    star(consonantal_syllable),
    final_syllable.


stressed_extended_rafsi --> stressed_brivla_rafsi.
stressed_extended_rafsi --> stressed_fuhivla_rafsi.

extended_rafsi --> brivla_rafsi.
extended_rafsi --> fuhivla_rafsi.

stressed_brivla_rafsi -->
    peek(unstressed_syllable),
    brivla_head,
    stressed_syllable,
    h,
    y.

brivla_rafsi -->
    peek((syllable, star(consonantal_syllable), syllable)),
    brivla_head,
    h,
    y,
    optional(h).

stressed_fuhivla_rafsi -->
    fuhivla_head,
    stressed_syllable,
    \+ h,
    onset,
    y.

fuhivla_rafsi -->
    peek(unstressed_syllable),
    fuhivla_head,
    \+ h,
    onset,
    y,
    optional(h).

fuhivla_head -->
    \+ rafsi_string,
    brivla_head.

brivla_head -->
    \+ cmavo,
    \+ slinkuhi,
    \+ h,
    peek(onset),
    star(unstressed_syllable).

slinkuhi -->
    \+ rafsi_string,
    consonant,
    rafsi_string.

rafsi_string -->
    star(y_less_rafsi),
    (   gismu
    ;   cvv_final_rafsi
    ;   stressed_y_less_rafsi
    ;   short_final_rafsi
    ;   y_rafsi
    ;   stressed_y_rafsi
    ;   peek(stressed_y_less_rafsi),
        initial_pair,
        y
    ;   hy_rafsi
    ;   stressed_hy_rafsi).

%% gismu
gismu -->
    (   initial_pair,
        stressed_vowel
    ;   consonant,
        stressed_vowel,
        consonant),
    peek(final_syllable),
    consonant,
    vowel,
    peek(post_word).

cvv_final_rafsi -->
    consonant,
    stressed_vowel,
    h,
    peek(final_syllable),
    vowel,
    peek(post_word).

short_final_rafsi -->
    peek(final_syllable),
    (   consonant, diphthong
    ;   initial_pair, vowel),
    peek(post_word).

stressed_hy_rafsi -->
    (   long_rafsi, stressed_vowel
    ;   stressed_ccv_rafsi
    ;   stressed_cvv_rafsi),
    h,
    y.


stressed_y_rafsi -->
    (   stressed_long_rafsi
    ;   stressed_cvc_rafsi),
    y.

stressed_y_less_rafsi -->
    stressed_cvc_rafsi,
    \+ y.
stressed_y_less_rafsi -->
    stressed_ccv_rafsi.
stressed_y_less_rafsi -->
    stressed_cvv_rafsi.

stressed_long_rafsi -->
    initial_pair,
    stressed_vowel,
    consonant.
stressed_long_rafsi -->
    consonant,
    stressed_vowel,
    consonant,
    consonant.

stressed_cvc_rafsi -->
    consonant,
    stressed_vowel,
    consonant.


stressed_ccv_rafsi -->
    initial_pair,
    stressed_vowel.

stressed_cvv_rafsi -->
    consonant,
    (   unstressed_vowel, h, stressed_vowel
    ;   stressed_diphthong),
    optional(r_hyphen).

hy_rafsi -->
    (   long_rafsi, vowel
    ;   ccv_rafsi
    ;   cvv_rafsi),
    h,
    y,
    optional(h).

y_rafsi -->
    (   long_rafsi
    ;   cvc_rafsi),
    y,
    optional(h).

y_less_rafsi -->
    \+ y_rafsi,
    \+ stressed_y_rafsi,
    \+ hy_rafsi,
    \+ stressed_hy_rafsi,
    (   cvc_rafsi
    ;   ccv_rafsi
    ;   cvv_rafsi),
    \+ h.

long_rafsi -->
    initial_pair,
    unstressed_vowel,
    consonant.

long_rafsi -->
    consonant,
    unstressed_vowel,
    consonant,
    consonant.

cvc_rafsi -->
    consonant,
    unstressed_vowel,
    consonant.

ccv_rafsi -->
    initial_pair,
    unstressed_vowel.

cvv_rafsi -->
    consonant,
    (   unstressed_vowel,
        h,
        unstressed_vowel
    ;   unstressed_diphthong),
    optional(r_hyphen).


r_hyphen -->
    r,
    peek(consonant).


r_hyphen -->
    n,
    peek(r).

%% vowels
final_syllable -->
    onset,
    \+ y,
    \+ stressed,
    nucleus,
    \+ cmene,
    peek(post_word).

stressed_syllable -->
    peek(stressed),
    syllable.

stressed_syllable -->
    syllable,
    peek(stress).

stressed_diphthong -->
    peek(stressed),
    diphthong.

stressed_diphthong -->
    diphthong,
    peek(stress).

stressed_vowel -->
    peek(stressed),
    vowel.

stressed_vowel -->
    vowel,
    peek(stress).

unstressed_syllable --> 
    \+ stressed,
    syllable,
    \+ stress.

unstressed_syllable -->
    consonantal_syllable.

unstressed_diphthong -->
    \+ stressed,
    diphthong,
    \+ stress.

unstressed_vowel -->
    \+ stressed,
    vowel,
    \+ stress.

stress -->
    star(consonant),
    optional(h),
    optional(y),
    syllable,
    pause.

stressed -->
    onset,
    star(comma),
    [X],
    {member(X, `AEIOU`)}.

any_syllable -->
    onset,
    nucleus,
    optional(coda).

any_syllable -->
    consonantal_syllable.

syllable -->
    onset,
    \+ y,
    nucleus,
    optional(coda).

consonantal_syllable -->
    consonant,
    peek(syllabic),
    coda.

coda --> 
    \+ any_syllable,
    consonant,
    peek(any_syllable).

coda -->
    optional(syllabic),
    optional(consonant),
    peek(pause).

onset --> h.
onset --> glide.
onset --> initial.

nucleus --> vowel.
nucleus --> diphthong.
nucleus -->
    y,
    \+ nucleus.

glide -->
    (   i
    ;   u),
    peek(nucleus).

diphthong -->
    (   a,i,\+i
    ;   a,u,\+u
    ;   e,i,\+i
    ;   o,i,\+i),
    \+ nucleus.

vowel -->
    (   a
    ;   e
    ;   i
    ;   o
    ;   u),
    \+ nucleus.

a -->
    star(comma),
    [X],
    { member(X, `aA`) }.

e -->
    star(comma),
    [X],
    { member(X, `eE`) }.

i -->
    star(comma),
    [X],
    { member(X, `iI`) }.

o -->
    star(comma),
    [X],
    { member(X, `oO`) }.

u -->
    star(comma),
    [X],
    { member(X, `uU`) }.

y -->
    star(comma),
    [X],
    { member(X, `yY`) }.


%% consonants
cluster -->
    consonant,
    plus(consonant).

initial_pair -->
    peek(initial),
    consonant,
    consonant,
    \+consonant.

initial -->
    (   affricate
    ;   optional(sibilant),
        optional(other),
        optional(liquid)),
    \+ consonant,
    \+ glide.

affricate --> t, c.
affricate --> t, s.
affricate --> d, j.
affricate --> d, z.

liquid --> l.
liquid --> r.

other --> p.
other --> t, \+ l.
other --> k.
other --> f.
other --> x.
other --> b.
other --> d, \+ l.
other --> g.
other --> v.
other --> m.
other --> n, \+ liquid.

sibilant --> c.
sibilant --> s, \+ x.
sibilant -->
    (   j
    ;   z),
    \+n,
    \+ liquid.

consonant --> voiced.
consonant --> unvoiced.
consonant --> syllabic.

voiced -->
    (   b
    ;   d
    ;   g
    ;   j
    ;   v
    ;   z).

unvoiced -->
    (   c
    ;   f
    ;   k
    ;   p
    ;   s
    ;   t
    ;   x).

syllabic -->
    (   l
    ;   m
    ;   n
    ;   r).

l -->
    star(comma),
    [X],
    {member(X, `lL`)},
    \+ h,
    \+ glide,
    \+ l.

m -->
    star(comma),
    [X],
    {member(X, `mM`)},
    \+h,
    \+ glide,
    \+ m,
    \+ z.

n -->
    star(comma),
    [X],
    {member(X, `nN`)},
    \+ h,
    \+ glide,
    \+ n,
    \+ affricate.

r -->
    star(comma),
    [X],
    {member(X, `rR`)},
    \+ h,
    \+ glide,
    \+ r.

b -->
    star(comma),
    [X],
    {member(X, `bB`)},
    \+ h,
    \+ glide,
    \+ b,
    \+ unvoiced.

d -->
    star(comma),
    [X],
    {member(X, `dD`)},
    \+ h,
    \+ glide,
    \+ d,
    \+ unvoiced.


g -->
    star(comma),
    [X],
    {member(X, `gG`)},
    \+ h,
    \+ glide,
    \+ g,
    \+ unvoiced.

v -->
    star(comma),
    [X],
    {member(X, `vV`)},
    \+ h,
    \+ glide,
    \+ v,
    \+ unvoiced.

j -->
    star(comma),
    [X],
    {member(X, `jJ`)},
    \+ h,
    \+ glide,
    \+ j,
    \+ z,
    \+ unvoiced.

z -->
    star(comma),
    [X],
    {member(X, `zZ`)},
    \+ h,
    \+ glide,
    \+ z,
    \+ j,
    \+ unvoiced.

s -->
    star(comma),
    [X],
    {member(X, `sS`)},
    \+ h,
    \+ glide,
    \+ s,
    \+ c,
    \+ voiced.

c -->
    star(comma),
    [X],
    {member(X, `cC`)},
    \+ h,
    \+ glide,
    \+ c,
    \+ s,
    \+ x,
    \+ voiced.

x -->
    star(comma),
    [X],
    {member(X, `xX`)},
    \+ h,
    \+ glide,
    \+ x,
    \+ c,
    \+ k,
    \+ voiced.

k -->
    star(comma),
    [X],
    {member(X, `kK`)},
    \+ h,
    \+ glide,
    \+ k,
    \+ x,
    \+ voiced.

f -->
    star(comma),
    [X],
    {member(X, `fF`)},
    \+ h,
    \+ glide,
    \+ f,
    \+ voiced.

p -->
    star(comma),
    [X],
    {member(X, `pP`)},
    \+ h,
    \+ glide,
    \+ p,
    \+ voiced.

t -->
    star(comma),
    [X],
    {member(X, `tT`)},
    \+ h,
    \+ glide,
    \+ t,
    \+ voiced.

h -->
    star(comma),
    [X],
    {member(X, `\'h`)},
    peek(nucleus).

%% misc
digit -->
    star(comma),
    [X],
    {member(X,`0123456789`)},
    \+ h,
    \+ nucleus.

post_word -->
    pause.

post_word -->
    \+ nucleus,
    lojban_word.

pause -->
    star(comma),
    (   plus(space_char)
    ;   eof).

eof --> star(comma), end.
end --> \+ [_].

non_lojban_word -->
    \+ lojban_word,
    plus(non_space).

non_space -->
    \+ space_char,
    [_].

space_char -->
    [X],
    { member(X, `.\t\n\r?! `) }.

comma --> `,` .

