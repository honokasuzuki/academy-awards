## Text Analysis: Academy Awards Acceptance Speeches - Actors vs. Actresses
**Is there a systematic difference between speech styles of actresses and actors at the Academy Awards, and what does that difference (if it exists) convey about the attitudes and social hierarchies of actresses and actors in the film industry?**

Transcripts of historical Academy Awards acceptances speeches were scraped from the Academy Awards Acceptance Speech Database (http://aaspeechesdb.oscars.org/) using the R package rvest. All available acceptance speeches in the database were scraped from the award categories *Actor in a Leading Role, Actress in a Leading Role, Actor in a Supporting Role,* and *Actress in a Supporting Role*. This resulted in a corpus of 229 documents, ranging from the 12th Academy Awards in 1939 to the 91st Academy Awards in 2018. Scraped variables per document (speech) included a transcript of the acceptance speech, name of actor or actress, name of the film, year of the Academy Awards, and award category (actor/actress, leading/supporting role).

This analysis will largely be guided by past findings* on differences in speech style across gender to see if such findings are replicated with the present study’s corpus. 

First, using sentence-level **sentiment analysis**, I will compare the emotional valence of speeches given by actors and those given by actresses using average sentiment polarities. This will provide a general idea of how positive or negative the actors and actresses’ speeches are compared to each other. To investigate further, emotion classification can provide a more specific idea of what dominant emotions beyond just positive or negative, such as joy, surprise, and anger, are expressed in these speeches.  

Second, I will use **part-of-speech tagging** to compare the top pronouns used in actors and actresses’ speeches. This will provide an understanding of who the award-winners talk about in their speeches (other men, other women) and whether they tend to speak only on their behalf (I, my) or on behalf of a collective group (we, our). 

Third, I will employ **structural topic modeling** to generate possible speech topics using gender and year of awards as predictors. If any apparently clear topics are generated, this will help me explore if there are significant differences in the contents of the acceptance speeches across gender and whether there are any topics that are talked about more by one gender than another. 

Lastly, I will perform a **naïve Bayes classification** to investigate how well the algorithm can predict the gender of the award-winner given a speech transcript. The performance of the classification will reveal how distinct and ‘learnable’ the differences between actors and actresses’ speeches are. 

*see literature review.
