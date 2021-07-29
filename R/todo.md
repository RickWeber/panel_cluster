#note to self
What I *really* want to do is figure out what sorts of paths countries take 
through the EFW dataset. But the set of plausible paths is one of those 
combinatorial messes that comically dwarfs this data set. It's worth reminding
a reader that although this code allows us to find clusters, it's only clustering
what we've seen, and that's a small subset of what's possible. There is no guarantee 
that what humans have tried in the last 50 years includes the things we *should*
try (by any normative standard!).
#/note to self

#Late July.

I'll have to go through this document soon to clear things out.

Here's a thought on comparing different methods for clustering a panel dataset: if I've got a year-by-year set of clusters with aligned labels, then I can use that as a baseline--e.g. compare within sum of squares of different methods as a ratio to this metric. 

For example, it would be nice to know if making a convoluted model-based cluster was only marginally better than just widening the data (or not). 

That requires a few todo items:

* Code to align year-wise clusters
* Code to calculate some kind of loss function for a clustered panel
* Code to visualize clusters (probably PCA the data to 2D, plot and color, etc.)
* 

#Early July!

I'm back in this project after a delay of a few weeks. To take advantage of having
a blank slate, I'm going to work on the project without looking at my old work.
After a creative burst, I'll integrate the old code and adjust as appropriate.

So what do I need to do?

Big picture:
* Write software to cluster panel data using a variety of approaches.
* Cluster the EFW dataset specifically. Write a paper about the patterns found.

So what needs to be done? 
There's the lit review and writing work for the EFW project, but I'll worry about
that somewhere else.

For the coding end of things, here are functions I need/want:
* wrapper to allow mapping several variables, approaches, parameter values, etc.
* cluster the panel by widening the dataset. Allow optional weighting by year.
* cluster the panel using a time trend or some other function of the data.
* align cluster membership between two different years to minimize the number of
countries that aren't able to be given the same cluster membership.
(e.g. Venezuela in 2000 should be in the same cluster in 1995, but maybe some 
other country has reforms and changes clusters.)
* cluster the panel by a chaining process... cluster for 2018, then for 2017 
including 2018 cluster membership as data.

I also need to go back and read the sources on Gower distance to make sure it does
what I think it does... which is allow distances between factors and hopefully between 
numeric values and NAs. 

###############

In functions.R:
* Make sure I'm not clustering based on year or name variables. I'm not sure it matters, but I'd better check.
* Pass cluster centroids from one year to another when doing cluster_yr_up
I'm closer on this, but I'm getting some sort of errors. I'll take another swing at it later.
* Maybe I'm overthinking it. What I really want is to align two sets of clusters (then align all sets of clusters for the data). I *do* want to also get clustering by NA values, but I'm thinking Gower distance might do the trick for me.
* Investigate Gower distance and alternatives more thoroughly.

Done?
* Make sure cluster_back() does what it's supposed to.
I think I've got this doing what it's supposed to, but that isn't the full story.