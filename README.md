# Catta

##Tools and data for systematic studies of text classification

Many studies on text classification have used arbitrarily sized samples for the classes, reporting at best the average amount of data per class. This makes it very difficult to compare studies. It is unlikely that two studies will use the same amount of data, and even if they do, the distribution between classes may be very different. It is well known that for many classification tasks, the amount of data is the most important factor for success, so this is certainly important information being lost.

If we look at author identification, being one of the most common classification tasks, we we can see some other common issues that may lead to an overestimation of the accuracy. First, there is the issue of how the data is divided between training set and test set. In many cases, the division is random, or even using alternating lines from the original data. But sections which appear close to each other in a text are more likely to be similar, so this practice will lead to higher accuracies reported in the test, but not in a real application.

Second, we need to consider topic dependence. It should come as no surprise that texts written on the same topic show similarities, and even within the works of a single author, samples from the same work are more similar than those from different works. In many cases, tests are done on a random mix of works for each author, or even on a single work for each author, whereas of course real applications would almost always use texts from different sources. This can potentially lead to a large overestimation of the accuracies.

Apart from this, many newer studies on classification also use more opaque methods, making it impossible to see how the classifier came to its conclusion. This not only makes it difficult to understand and learn more about the process, but also means that we cannot tell if the methods are likely to be reliable in a real-life setting.<

In order to address these issues, we need:

- Large amounts of text-based data, marked for at least author, preferably other properties.
- Data from multiple sources for each author, with sufficient amounts of data for each author and source.
- The data from each source split up into specific sample sizes, so that we can compare methods and classes without the influence of varying amount of data.
- Each sample consisting of a continuous block of text (from a single source), rather than samples from the entire work mixed randomly, to reduce the influence of close-text similarity
- Datasets with different sample sizes, so that we can investigate the effects of amount of data, and make more reasonable comparisons between studies
- Simple, transparent methods for classification, so that we can understand and compare the processes that lead to success in different classification tasks
- Freely available data and modular tools that allow us to compare one part at a time

For the data, our solution is to use novels. This allows us to use each separate book as an approximation of topic, so that we can investigate the influence of topic and show that it matters. Each novel also has enough text that we can cut out several samples from each, and still have sufficient sample sizes to get reasonable accuracies, as well as investigate the influence of varying the amount of data.

One problem with novels (and many other types of texts) is that they may not be free to distribute them. Many corpora include the sentences in random order, to avoid copyright infringement, but this makes it impossible to avoid close-text similarity. We have therefore taken continuous samples from each novel, and pre-calculated the statistics for several features that may be used in classification.

We have included several sample sizes, based on the same original data. When comparing sample sizes, it is often convenient to vary the amount of data exponentially, since the difference between 100 and 200 words of data will be much larger than between 10100 and 10200. To make it easier to draw plots and conclusions based on the results, we have used sample sizes based on powers of two.

For the tools, we have used small Perl scripts that work on a simple format of text files. This makes it easy for users and programmers regardless of knowledge to see each step in the process, and to replace it with their own, all without the need for opaque databases or import/export functions.

Our aim is to continue building on this toolset by adding more scripts for different kinds of classification, more classification algorithms, more different feature sets, more data, and so on.

The tools and data are available at github.com/spraakbanken/catta.
