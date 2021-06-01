In functions.R:
* Make sure I'm not clustering based on year or name variables. I'm not sure it matters, but I'd better check.
* Pass cluster centroids from one year to another when doing cluster_yr_up
I'm closer on this, but I'm getting some sort of errors. I'll take another swing at it later.
* Maybe I'm overthinking it. What I really want is to align two sets of clusters (then align all sets of clusters for the data). I *do* want to also get clustering by NA values, but I'm thinking Gower distance might do the trick for me.
* Investigate Gower distance and alternatives more thoroughly.

Done?
* Make sure cluster_back() does what it's supposed to.
I think I've got this doing what it's supposed to, but that isn't the full story.