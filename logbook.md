##2019-06-28 Fri



TODO:

logistics
- check offer letter to make sure names and dates are all correct
	- print, sign, and put it in mail box, Tim will sign it and give it to office next week
	- might need to fill out other paperwork??
	
corpus-building:
- work backwards from analyses
	- start on this first
	- type frequencies of surface forms of plurals (or rules??)
- merge based on unimorph keys
	- make sure celex is a subset of unimorph
	- do pairwise indic check between celex and unimorph
		- if there's stuff not in both, check the union
- then go step by step 
- figure out what celex endings are, do they solve our problem?

git stuff
- put all the files I have on the git
	- data
	- presentation
	- papers
- start document in there where describe analyses that I'm doing
	- helpful for meetings
	- start using that to congeal our thoughts
	
yang's model
- wrote a section in the document about figuring out yang's model

goals for next time:
- get all data together that I need
- write up what yang's analysis was
	- goal #1: figure out what the actual issue was 
	week after: replicate yang (or not, depending on if it works)
- REPLICATE YANG IN THE NEXT 2 WEEKS
	- figure out some basic things to try to see if it's cherry picking
	- then can do something new! (try to model this in an extension of fragment grammars??)
	
later:
- either yang's works, or it doesn't
- are there incorrect analyses that also satisfy the tolerance principle?


NOTES:
general
- write it up as we go!!
	- will make lives easier down the road
	- submit a cogsci paper on this in January?
- write up in file for meeting every week
tim's paper in this year's ACL
- model of irregularity
- morphology stuff and deep learning–look into this
tools
- r tinyverse– might be better for this kind of thing
- python pandas

UPDATE:
before the 'break': 
- was working on getting the dataset together
	- celex --> morphology, frequencies
		- extracted all singular
		- extracted all plural
		- put everything (as well as above two) into human-readable format
	- unimorph --> morphology
		- separated unimorph nouns from rest
	- subtlex --> frequencies

QUESTIONS (pre-meeting, for Tim):
- what format should I put the final dataset in?	TBD, DEPENDS ON WHAT WE ACTUALLY NEED
- what info should I keep, what should I throw out?	KEEP EVERYTHING
	- ex: stuff about verb tense isn't relevant, so throw it out, but stuff about case is???




~~ BELOW IS TEMPLATE (JACOB'S STUFF) ~~

##2019-06-18 Tue

- look at relationship between pythagorean from pair-code blog for [coenen](cites/coenen.a.2019.pdf) and ultrametric for tree embedding.
- read [BERT](cites/devlin/j.2019.pdf) [ELMo](cites/peters.m.2018.pdf)  original papers
- in Hewitt and Manning
    - immediate goal: understand how they used ELMo
    - we must replicate this paper.
    - think about the relationship to projectivity here: is there one? see the Kulhmann paper

- get familiar with stanford dependencies, relationship with constituent trees
