### benford\_law\_test ###

A script in R that tests whether the leading digit of upvotes per post on android.stackexchange.com follows Benford’s distribution and plots the results. It also uses a chi-square goodness-of-fit test to see whether the deviation is statistically significant.

`votes.xml` contains votes details for [android.stackexchange.com](http://android.stackexchange.com) and is part of the [stackexchange data dump (Sep 11)](http://blog.stackoverflow.com/2011/09/creative-commons-data-dump-sep-11/).

Based on <https://github.com/drewconway/WikiLeaks_Analysis/blob/master/wikileaks_analysis.R>.