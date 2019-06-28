##2019-06-18 Tue

- look at relationship between pythagorean from pair-code blog for [coenen](cites/coenen.a.2019.pdf) and ultrametric for tree embedding.
- read [BERT](cites/devlin/j.2019.pdf) [ELMo](cites/peters.m.2018.pdf)  original papers
- in Hewitt and Manning
    - immediate goal: understand how they used ELMo
    - we must replicate this paper.
    - think about the relationship to projectivity here: is there one? see the Kulhmann paper

- get familiar with stanford dependencies, relationship with constituent trees

##2019-06-21 Fri

Meeting postponed to Tuesday. Goal for then: start a TeX document outline on what ELMo is, how it differs from a biLSTM, and what it means to use it pre-trained, as Hewitt did.

##2019-06-25 Tue

Some ideas so far:

With the current goal being to reimplement the Hewitt structural probe, directions to take with that:

- surely the biLM part of ELMO would be much better for computing PMI than whatever Richard used. Try it.
	- possible problem: how to find the marginals without looping over the whole vocabulary?
- test the Hewitt probe on linguistically interesting examples, and just describe what we see (rather than testing its performance)
	- for example: make a hand-crafted set of probe sentences, with interesting linguistic properties: like attachment ambiguity, examples of raising, control, a-bar movement, and see if the Hewitt probe reconstructs this, fails in a consistent way, is chaotic, etc.
- do the same thing that Hewitt does only with Universal Dependencies instead of Stanford Dependencies
	- then experiment with structural probes in other languages (ELMo has been trained for Portuguese and Japanese, [see here](https://allennlp.org/elmo))

Some more general directions:


- think about the relationship to projectivity and mild context sesitivity

- look for implicit trees (with a leaf-only tree distance measure) built via the ultrametric property in these kind of embeddings
