;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-reader.ss" "lang")((modname CSC104.2015F.Project.II) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/batch-io)
(define CMU (read-words/line "cmudict-0.7b.txt"))

(check-expect (list-ref CMU 10000) (list "BEINGS" "B" "IY1" "IH0" "NG" "Z"))

(check-expect (filter-2 < 100 (list 23 104 56 789 104))
              (list 104 789 104))
(check-expect (filter-2 = 104 (list 23 104 56 789 104))
              (list 104 104))

(define (filter-2 binary-predicate a-value a-list)
  (local [(define (unary-predicate an-element)
            (binary-predicate a-value an-element))]
    (filter unary-predicate a-list)))

(require "GUI.rkt")

(check-expect (string-ci=? "HelLo" "helLO") #true)
(check-expect      (equal? "HelLo" "helLO") #false)
;
(check-expect (string-ci=? "Hello" "Hello") #true)
(check-expect      (equal? "Hello" "Hello") #true)
;
(check-expect (string-ci=? "Hi" "Ha") #false)
(check-expect      (equal? "Hi" "Ha") #false)

(check-expect (string->number "7") 7)
(check-expect (string->number "104") 104)

(check-expect
 (string->characters "Human beings united.")
 (list "H" "u" "m" "a" "n" " " "b" "e" "i" "n" "g" "s" " " "u" "n" "i" "t" "e" "d" "."))

(define (string->characters a-string)
  (local [(define (character-ref i) (substring a-string i (add1 i)))]
    (map character-ref (range 0 (string-length a-string) 1))))

; ★ word-entry? ★

; 1. Write a full design 'check-expect'.
; 2. Fill in the contract.
; 3. Fix the body.

(check-expect (word-entry? "beings" (list "BEINGS" "B" "IY1" "IH0" "NG" "Z"))
              #true)
(check-expect (word-entry? "beings" (list "BEINGS" "B" "IY1" "IH0" "NG" "Z"))
              (string-ci=? "beings" "BEINGS"))
(check-expect (word-entry? "human" (list "BEINGS" "B" "IY1" "IH0" "NG" "Z"))
              #false)
(check-expect (word-entry? "human" (list "BEINGS" "B" "IY1" "IH0" "NG" "Z"))
              (string-ci=? "human" "BEINGS"))
(check-expect (word-entry? "human" (list "HUMAN" "HH" "Y" "UW1" "M" "AH0" "N"))
              #true)
(check-expect (word-entry? "human" (list "HUMAN" "HH" "Y" "UW1" "M" "AH0" "N"))
              (string-ci=? "human" "HUMAN"))
(check-expect (word-entry? "beings" (list "HUMAN" "HH" "Y" "UW1" "M" "AH0" "N"))
              (string-ci=? "beings" "HUMAN"))
(check-expect (word-entry? "beings" (list "HUMAN" "HH" "Y" "UW1" "M" "AH0" "N"))
              (string-ci=? "beings" (first (list "HUMAN" "HH" "Y" "UW1" "M" "AH0" "N"))))


; word-entry? : string list --> boolean
; Whether 'entry' is in the form of a dictionary entry for 'word'.
(define (word-entry? word entry)
  (string-ci=? word (first entry)))

; ★ lookup-entry ★

; 1. Fill in the contract.
; 2. Fix the body.

(check-expect (lookup-entry "beings")
              (list "BEINGS" "B" "IY1" "IH0" "NG" "Z"))
(check-expect (lookup-entry "human")
              (list "HUMAN" "HH" "Y" "UW1" "M" "AH0" "N"))
(check-expect (lookup-entry "beings")
              (first (filter-2 word-entry? "beings" CMU)))
(check-expect (lookup-entry "human")
              (first (filter-2 word-entry? "human" CMU)))

; lookup-entry : string --> list
; The CMU dictionary entry for 'word'.
(define (lookup-entry word)
  (first (filter-2 word-entry? word CMU)))

; ★ word->phonemes ★

; 1. Write a full design 'check-expect'.
; 2. Fill in the contract.
; 3. Fix the body.

(check-expect (word->phonemes "beings") (list "B" "IY1" "IH0" "NG" "Z"))
(check-expect (word->phonemes "human") (list "HH" "Y" "UW1" "M" "AH0" "N"))
(check-expect (word->phonemes "human") (rest (lookup-entry "human")))

; word->phonemes : word -> list
; The phonemes for 'word'.
(define (word->phonemes word)
  (rest (lookup-entry word)))

; ★ line->phonemes ★

; 1. Write a full design 'check-expect'.
; 2. Fill in the contract.
; 3. Fix the body.

(check-expect (line->phonemes (list "Human" "beings"))
              (list "HH" "Y" "UW1" "M" "AH0" "N"
                    "B" "IY1" "IH0" "NG" "Z"))
(check-expect (line->phonemes (list "Human" "beings" "united"))
              (list "HH" "Y" "UW1" "M" "AH0" "N"
                    "B" "IY1" "IH0" "NG" "Z"
                    "Y" "UW0" "N" "AY1" "T" "AH0" "D"))
(check-expect (line->phonemes (list "Human" "beings" "united"))
              (append (list "HH" "Y" "UW1" "M" "AH0" "N")
                      (list "B" "IY1" "IH0" "NG" "Z")
                      (list "Y" "UW0" "N" "AY1" "T" "AH0" "D")))
(check-expect (line->phonemes (list "Human" "beings" "united"))
              (append (word->phonemes "Human")
                      (word->phonemes "beings")
                      (word->phonemes "united")))
(check-expect (line->phonemes (list "Human" "beings" "united"))
              (apply append (map word->phonemes (list "Human" "beings" "united"))))

; line->phonemes : list --> list
; The phonemes for the words in 'line'.
(define (line->phonemes line)
  (apply append (map word->phonemes line)))


; ★ vowel? ★

; 1. Fill in the contract.
; 2. Fix the body.

(check-expect (vowel? "B") #false)
(check-expect (vowel? "B") (member? (substring "B" (sub1 (string-length "B")))
                                    (list "0" "1" "2")))
(check-expect (vowel? "IY1") #true)
(check-expect (vowel? "IY1") (member? (substring "IY1" (sub1 (string-length "IY1")))
                                      (list "0" "1" "2")))

; vowel? : list --> boolean
; Whether 'a-phoneme' represents a vowel.
(define (vowel? a-phoneme)
  (member? (substring a-phoneme (sub1 (string-length (list "0" "1" "2"))))))

; ★ number-of-vowels ★

; 1. Write a full design 'check-expect'.
; 2. Fill in the contract.
; 3. Fix the body.

(check-expect (number-of-vowels (list "Human" "beings"))
              4)
(check-expect (number-of-vowels (list "Human" "beings"))
              (length (list "UW1" "AH0" "IY1" "IH0")))
(check-expect (number-of-vowels (list "Human" "beings" "united"))
              7)
(check-expect (number-of-vowels (list "Human" "beings" "united"))
              (length (list "UW1" "AH0" "IY1" "IH0" "UW0" "AY1" "AH0")))
(check-expect (number-of-vowels (list "Human" "beings" "united"))
              (length (vowel? (line->phonemes (list "Human" "beings" "united")))))


; number-of-vowels :
; The total number of vowels in the words in 'line'.
(define (number-of-vowels line)
  (length (vowel? (line->phonemes line))))


; ★ compare-syllables ★

; 1. Write a full design 'check-expect'.
; 2. Fill in the contract.
; 3. Fix the body.

(check-expect
 (compare-syllables
  (list (list "Cherry" "blossoms" "bloom")
        (list "softly" "falling" "from" "the" "tree")
        (list "explode" "into" "night"))
  (list 5 7 5))
 #true)

(check-expect
 (compare-syllables
  (list (list "Cherry" "blossoms" "bloom")
        (list "softly" "falling" "from" "the" "tree")
        (list "explode" "into" "night"))
  (list 8 8 5 5 8))
 #false)

(check-expect
 (compare-syllables
  (list
   (list "I" "wish" "I" "had" "thought" "of" "a" "rhyme")
   (list "Before" "I" "ran" "all" "out" "of" "time")
   (list "I'll" "sit" "here" "instead")
   (list "A" "cloud" "on" "my" "head")
   (list "That" "rains" "'til" "I'm" "covered" "with" "slime"))
  (list 5 7 5))
 #false)

(check-expect
 (compare-syllables
  (list
   (list "I" "wish" "I" "had" "thought" "of" "a" "rhyme")
   (list "Before" "I" "ran" "all" "out" "of" "time")
   (list "I'll" "sit" "here" "instead")
   (list "A" "cloud" "on" "my" "head")
   (list "That" "rains" "'til" "I'm" "covered" "with" "slime"))
  (list 8 8 5 5 8))
 #true)



; compare-syllables :
; Whether each line of 'poem' has the number of vowels listed in 'syllables'.
(define (compare-syllables poem syllables)
  (equal? poem syllables))


; ★ letter-or-apostrophe? ★

; 1. Fill in the contract.
; 2. Fix the body.

(check-expect (letter-or-apostrophe? "g") #true)
(check-expect (letter-or-apostrophe? "g") (or (string-alphabetic? "g")
                                              (equal? "g" "'")))
(check-expect (letter-or-apostrophe? "7") #false)
(check-expect (letter-or-apostrophe? "7") (or (string-alphabetic? "7")
                                              (equal? "7" "'")))
(check-expect (letter-or-apostrophe? "'") #true)
(check-expect (letter-or-apostrophe? "'") (or (string-alphabetic? "'")
                                              (equal? "'" "'")))
(check-expect (letter-or-apostrophe? "!") #false)
(check-expect (letter-or-apostrophe? "!") (or (string-alphabetic? "!")
                                              (equal? "!" "'")))

; letter-or-apostrophe? :
; Whether 's' contains only letters, or is an apostrophe.
(define (letter-or-apostrophe? s)
  ....)


; ★ de-punctuate-word ★

; 1. Fill in the contract.
; 2. Fix the body.

(check-expect (de-punctuate-word "bloom,") "bloom")
(check-expect (de-punctuate-word "bloom,")
              (apply string-append (filter letter-or-apostrophe? (string->characters "bloom,"))))
(check-expect (de-punctuate-word "I'm") "I'm")
(check-expect (de-punctuate-word "I'm")
              (apply string-append (filter letter-or-apostrophe? (string->characters "I'm"))))

; de-punctuate-word :
; 'word' with only its letters and apostrophes.
(define (de-punctuate-word word)
  "fix")


; ★ de-punctuate-words ★

; 1. Write a full design 'check-expect'.
; 2. Fill in the contract.
; 3. Fix the body.

(check-expect (de-punctuate-words (list "That" "rains" "'til" "I'm" "covered" "with" "slime"))
              (list "That" "rains" "'til" "I'm" "covered" "with" "slime"))
(check-expect (de-punctuate-words (list "Teach" "us," "Sprite" "or" "Bird,"))
              (list "Teach" "us" "Sprite" "or" "Bird"))
(check-expect (de-punctuate-words (list "Teach" "us," "Sprite" "or" "Bird,"))
              (list (de-punctuate-word "Teach")
                    (de-punctuate-word "us,")
                    (de-punctuate-word "Sprite")
                    (de-punctuate-word "or")
                    (de-punctuate-word "Bird,")))



; de-punctuate-words :
; 'words' with each word in it reduced to just its letters and apostrophes.
(define (de-punctuate-words words)
  ....)


; ★ de-punctuate-poem ★

; 1. Write a full design 'check-expect'.
; 2. Fill in the contract.
; 3. Fix the body.

(check-expect (de-punctuate-poem
               (list (list "Teach" "us," "Sprite" "or" "Bird,")
                     (list "What" "sweet" "thoughts" "are" "thine:")
                     (list "I" "have" "never" "heard")
                     (list "Praise" "of" "love" "or" "wine")
                     (list "That" "panted" "forth" "a" "flood" "of" "rapture" "so" "divine.")))
              (list
               (list "Teach" "us" "Sprite" "or" "Bird")
               (list "What" "sweet" "thoughts" "are" "thine")
               (list "I" "have" "never" "heard")
               (list "Praise" "of" "love" "or" "wine")
               (list "That" "panted" "forth" "a" "flood" "of" "rapture" "so" "divine")))
(check-expect (de-punctuate-poem
               (list
                (list "Cherry" "blossoms" "bloom,")
                (list "softly" "falling" "from" "the" "tree,")
                (list "explode" "into" "night.")))
              (list (list "Cherry" "blossoms" "bloom")
                    (list "softly" "falling" "from" "the" "tree")
                    (list "explode" "into" "night")))



; de-punctuate-poem :
; 'poem' with each word in each line of words reduced to just its letters and apostrophes.
(define (de-punctuate-poem poem)
  ....)


; ★ Running the Whole Program ★

; 1. When everything else is done, uncomment the last expression below and run the program.
;
; You can then choose:
;   A file containing a poem (e.g. the file "a-haiku.txt" from the course website).
; Then choose:
;   A file containing the name of a poem form and the expected number of syllables per line
;    (e.g. the file "haiku-form.txt").
;
; A message will pop up stating whether the poem has the expected number of syllables per line.

(define (ask-forever _)
  (local [(define poem-file (choose-file "Poem"))
          (define raw-poem (read-file poem-file))
          (define poem (de-punctuate-poem (read-words/line poem-file)))
          (define poem-form (first (read-words-and-numbers/line (choose-file "Syllable Counts"))))
          (define next-line "\n")]
    (ask-forever
     (message "Poetry Syllable Counts"
              (string-append "The poem"
                             next-line
                             next-line
                             raw-poem
                             next-line
                             next-line
                             " has the "
                             (cond [(compare-syllables poem (rest poem-form)) "correct"]
                                   [else "incorrect"])
                             " number of syllables per line for a "
                             (first poem-form)
                             ".")))))

; (ask-forever "ignored")