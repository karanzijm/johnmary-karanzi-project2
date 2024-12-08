#lang racket

;;; Require the data science library
(require net/url)
(require data-science-master)
(require plot math math/matrix racket/hash srfi/19)

;;; Define the tweet struct
(struct tweet (date location text) #:transparent)

;;; Dummy tweet data
(define tweets
  (list
   (tweet "2023-01-15" "Uganda" "This is amazing!")
   (tweet "2023-12-10" "Uganda" "This is terrible.")
   (tweet "2023-03-05" "Uganda" "I love it.")
   (tweet "2023-03-15" "Uganda" "Not bad at all.")
   (tweet "2023-04-01" "Uganda" "This is okay.")
   (tweet "2023-04-20" "Uganda" "This is disappointing.")
   (tweet "2023-02-10" "Uganda" "Such a joyous moment!")
   (tweet "2023-05-12" "Uganda" "How can this happen? I'm furious!")
   (tweet "2023-06-15" "Uganda" "Life is beautiful today!")
   (tweet "2023-07-20" "Uganda" "Unbelievable! This is absurd.")
   (tweet "2023-08-05" "Uganda" "Feeling grateful and happy.")
   (tweet "2023-09-12" "Uganda" "This is absolutely outrageous!")
   (tweet "2023-10-18" "Uganda" "What a wonderful experience!")
   (tweet "2023-11-25" "Uganda" "So frustrating, I can't believe this.")
   (tweet "2023-12-01" "Uganda" "Pure joy! Couldn’t be happier.")
   (tweet "2023-01-20" "Uganda" "This is completely unacceptable.")
   (tweet "2023-02-25" "Uganda" "What an incredible achievement!")
   (tweet "2023-03-30" "Uganda" "Feeling ecstatic right now!")
   (tweet "2023-04-15" "Uganda" "So disappointed. This could’ve been better.")
   (tweet "2023-05-22" "Uganda" "Anger beyond words! Why this?")
   (tweet "2023-06-07" "Uganda" "This is pure bliss.")
   (tweet "2023-07-15" "Uganda" "I’m so mad, this isn’t fair.")
   (tweet "2023-08-25" "Uganda" "Love every bit of this moment!")
   (tweet "2023-09-30" "Uganda" "This just made my day!")
   (tweet "2023-10-10" "Uganda" "Absolutely furious right now.")
   (tweet "2023-11-15" "Uganda" "Over the moon with excitement!")
   (tweet "2023-12-20" "Uganda" "Such an incredible journey!")
   (tweet "2023-01-05" "Uganda" "This is unacceptable, really.")
   (tweet "2023-02-18" "Uganda" "A joyful celebration indeed!")
   (tweet "2023-03-25" "Uganda" "I am so angry, this shouldn’t happen.")
   (tweet "2023-04-05" "Uganda" "Happiest day of my life!")
   (tweet "2023-05-10" "Uganda" "This fills me with so much happiness.")
   (tweet "2023-06-20" "Uganda" "Absolutely livid right now!")
   (tweet "2023-07-25" "Uganda" "Pure and utter joy.")
   (tweet "2023-08-30" "Uganda" "This is infuriating beyond measure.")
   (tweet "2023-09-18" "Uganda" "Couldn’t be happier right now!")
   (tweet "2023-10-01" "Uganda" "This is so disappointing.")
   (tweet "2023-11-05" "Uganda" "Feeling absolutely thrilled!")
   (tweet "2023-12-31" "Uganda" "Ending the year on such a high note!")
)
)

;; Function to convert the date string to a Scheme date
(define (convert-date date-string)
  (string->date date-string "~Y-~m-~d"))

;; Function to make the date more readable
(define (format-date date)
  (date->string date "~Y-~m-~d"))


;; Example usage
(define date (convert-date "2023-01-15"))

(display (format-date date)) ;; Outputs "2023-01-15"
;;(display (format-date date-tsp))

;;; Custom aggregate function since the library's aggregate might not work as expected
(define (custom-aggregate sentiment-list)
  (let loop ([result '()]
             [remaining sentiment-list])
    (cond 
      [(null? remaining) 
       (sort result (lambda (x y) (> (second x) (second y))))]
      [else
       (let* ([current (car remaining)]
              [emotion (second current)]
              [freq (third current)]
              [existing (assoc emotion result)])
         (loop 
          (if existing
              (map (lambda (item)
                     (if (equal? (car item) emotion)
                         (list emotion (+ (second item) freq))
                         item))
                   result)
              (cons (list emotion freq) result))
          (cdr remaining)))])))

;;; Filter tweets for Uganda
(define uganda-tweets 
  (filter (lambda (t) (string=? (tweet-location t) "Uganda")) tweets))



;;; Prepare tweet texts for analysis
(define uganda-tweet-texts 
  (map tweet-text uganda-tweets))

(take uganda-tweet-texts 15)

;;; Print details of tweets to be analyzed
(displayln "Uganda Tweets to be Analyzed:")
(for ([tweet uganda-tweets])
  (displayln 
   (format "Date: ~a, Text: ~a" 
           (tweet-date tweet) 
           (tweet-text tweet))))

;;; Combine tweet texts into a single string for analysis
(define uganda-text 
  (string-join uganda-tweet-texts " "))

;;; Normalize the text 
(define normalized-text 
  (string-normalize-spaces
   (remove-punctuation
    (string-downcase uganda-text) #:websafe? #t)))

;;; Extract tokens from the text
(define words 
  (document->tokens normalized-text #:sort? #t))

;;; Perform sentiment analysis using NRC lexicon
(define sentiment 
  (list->sentiment words #:lexicon 'nrc))

;;; Define emotion-aggregate before using it
(define emotion-aggregate 
  (custom-aggregate (cdr sentiment)))

;;; Print out sentiment analysis results
(displayln "\nSentiment Analysis Results:")
(display "Emotion Breakdown: ")
(for ([emotion emotion-aggregate])
  (displayln 
   (format "~a: ~a" 
           (first emotion) 
           (second emotion))))

;;; Optional visualization using built-in plot
(require plot)
(parameterize ((plot-width 800))
  (plot (list
         (tick-grid)
         (discrete-histogram
          emotion-aggregate
          #:color "MediumSlateBlue"
          #:line-color "MediumSlateBlue"))
        #:x-label "Emotions"
        #:y-label "Frequency"))