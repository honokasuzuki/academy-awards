## Text Analysis: Academy Awards Acceptance Speeches - Actors vs. Actresses
**Is there a systematic difference between speech styles of actresses and actors at the Academy Awards, and what does that difference (if it exists) convey about the attitudes and social hierarchies of actresses and actors in the film industry?**

Transcripts of historical Academy Awards acceptances speeches were scraped from the Academy Awards Acceptance Speech Database (http://aaspeechesdb.oscars.org/) using the R package rvest. All available acceptance speeches in the database were scraped from the award categories *Actor in a Leading Role, Actress in a Leading Role, Actor in a Supporting Role,* and *Actress in a Supporting Role*. This resulted in a corpus of 229 documents, ranging from the 12th Academy Awards in 1939 to the 91st Academy Awards in 2018. Scraped variables per document (speech) included a transcript of the acceptance speech, name of actor or actress, name of the film, year of the Academy Awards, and award category (actor/actress, leading/supporting role).

The analysis will mostly be guided by past findings on differences in speech style across gender to investigate if such findings can be replicated with the present study’s corpus.

First, using sentence-level **sentiment analysis**, I will compare the emotional valence of speeches given by actors and those given by actresses using average sentiment polarities. This will elucidate how positive or negative the actors and actresses’ speeches are compared to one another. To investigate further, emotion classification can provide a more specific idea of what dominant emotions beyond just positive or negative are expressed in these speeches, such as joy, surprise, and anticipation.  

Second, I will use **part-of-speech tagging** to compare the top pronouns and nouns used in actors and actresses’ speeches. This will provide an understanding of who and what the award-winners speak about in their speeches. 

Third, I will perform **structural topic modeling** to generate possible speech topics using gender and year of awards as predictors. If any apparently clear topics are generated, this can lead to an exploration of any differences in the contents of acceptance speeches across gender and whether the topics differ in prevalence across gender. 

Lastly, I will perform a **naïve Bayes classifier** to investigate how well the algorithm can predict the gender of the award-winner given a speech transcript. The performance of the classification will reveal the distinctiveness and ‘learnability’ of the indicators of actors and actresses’ speeches. 
 

*see literature review.
