# CS 770, Fall 2026 | Methods: Statistics I (Oct 30)
# Factor analysis and scale reliability with practice set A
#
# Data: Doshi, A. R. & Hauser, O. P. (2024). Generative AI enhances individual
# creativity but reduces the collective diversity of novel content.
# Science Advances 10(28), eadn5290. doi:10.1126/sciadv.adn5290
# Codebook: https://cs770.github.io/Fall26/practice-data.html
#
# What this script does:
#   1. Loads the evaluator ratings and reverse-keys the negatively worded item
#   2. Averages the ratings per story (our unit of analysis)
#   3. Uses parallel analysis to decide how many factors to keep
#   4. Extracts and rotates the factors, then prints the loadings
#   5. Builds two scale scores and checks Cronbach's alpha for each
#   6. Compares our scales with the authors' novelty and usefulness indices

# ---- 0. Packages (install once, then comment out) -------------------------
# install.packages(c("psych", "GPArotation"))
library(psych)       # fa.parallel(), fa(), alpha()
library(GPArotation) # rotation methods used by fa()

# ---- 1. Load the data ------------------------------------------------------
# One row per evaluator rating of a story: 3,519 rows, 12 items rated 1 to 9.
# Reads straight from the course site; to use a downloaded copy instead,
# replace the URL with the file name, e.g., read.csv("ai-creativity-ratings.csv")
ratings <- read.csv("https://cs770.github.io/Fall26/practice-data/ai-creativity-ratings.csv")

items <- c("novel", "original", "rare",
           "appropriate", "feasible", "publishable",
           "enjoyed", "well_written", "boring", "funny", "twist", "future")

# "boring" is negatively worded: a high score means a worse story.
# Reverse-key it so that higher means better on every item (1 becomes 9, 9 becomes 1).
# Do this BEFORE any factor analysis, alpha, or scale score.
ratings$boring <- 10 - ratings$boring

# ---- 2. Choose the unit of analysis ---------------------------------------
# Ratings are nested: each evaluator rated about six stories, and each story
# has about 12 evaluators. The 3,519 rows are not 3,519 independent cases, and
# evaluator habits (e.g., someone who rates everything high) add shared variance
# across items. The scales describe stories, so we average each item per story
# and factor-analyze the 293 story means.
stories <- aggregate(ratings[items],
                     by = list(story_id = ratings$story_id, condition = ratings$condition),
                     FUN = mean)
X <- stories[items]
nrow(X)  # 293 stories

# The item correlations are worth a look before anything else
round(cor(X), 2)

# ---- 3. How many factors? --------------------------------------------------
# Parallel analysis (Horn, 1965) compares the eigenvalues of our data with
# eigenvalues from random data of the same size. Keep the factors whose
# eigenvalues beat the random ones. The plot doubles as a scree plot.
set.seed(770)  # the random comparison data change from run to run
pa <- fa.parallel(X, fm = "minres", fa = "fa")
round(pa$fa.values, 2)  # observed eigenvalues of the factor solution
pa$nfact                # suggested number of factors (2 for the story means)

# For comparison: on the 3,519 individual ratings, parallel analysis suggests 4.
# With many rows the random eigenvalues sit close to zero, so even very small
# factors beat them (the psych documentation notes this sample-size problem).
# fa.parallel(ratings[items], fm = "minres", fa = "fa")

# ---- 4. Extract and rotate -------------------------------------------------
# Two factors, minimum residual extraction, varimax (orthogonal) rotation.
fit <- fa(X, nfactors = 2, rotate = "varimax", fm = "minres")

# Hide loadings below .32, a common minimum (Tabachnick & Fidell, cited in
# Costello & Osborne, 2005). An item at .32 or more on both factors cross-loads.
print(fit$loadings, cutoff = 0.32)

# Communality: how much of each item's variance the two factors explain.
# Low values (e.g., funny) mean the item does not belong with the others.
round(fit$communality, 2)

# Varimax forces the factors to be uncorrelated. An oblique rotation (oblimin)
# lets them correlate, which is usually more realistic. Compare the loadings,
# and look at the factor correlation in Phi.
fit_ob <- fa(X, nfactors = 2, rotate = "oblimin", fm = "minres")
print(fit_ob$loadings, cutoff = 0.32)
round(fit_ob$Phi, 2)

# ---- 5. Build the scales and check reliability ----------------------------
# Group the items by the factor they load on most. We leave out "funny":
# it has a low communality and does not fit either factor cleanly.
novelty_items <- c("novel", "original", "rare", "twist", "future")
quality_items <- c("appropriate", "feasible", "publishable",
                   "enjoyed", "well_written", "boring")

# Cronbach's alpha for each scale. Read raw_alpha, and the "Reliability if an
# item is dropped" table: an item whose removal raises alpha is a weak item.
# (psych may print "Number of categories should be increased"; it is harmless
# here, because story means are not whole-number responses.)
alpha(X[novelty_items])
alpha(X[quality_items])

# What happens to alpha if we keep "funny" in the novelty scale?
alpha(X[c(novelty_items, "funny")])$total$raw_alpha

# A scale score is the mean of its items
stories$novelty <- rowMeans(X[novelty_items])
stories$quality <- rowMeans(X[quality_items])

# ---- 6. Compare with the authors' indices ---------------------------------
# The paper's novelty index = novel, original, rare (alpha = .92) and its
# usefulness index = appropriate, feasible, publishable (alpha = .89).
paper_novelty    <- c("novel", "original", "rare")
paper_usefulness <- c("appropriate", "feasible", "publishable")
stories$novelty_paper    <- rowMeans(X[paper_novelty])
stories$usefulness_paper <- rowMeans(X[paper_usefulness])

# How close are our scales to theirs?
round(cor(stories$novelty, stories$novelty_paper), 2)
round(cor(stories$quality, stories$usefulness_paper), 2)

# Alpha depends on the unit of analysis. On story means (averages of about 12
# raters) alpha is higher than on individual ratings. The individual ratings
# are the closer match to the paper's values.
round(c(
  novelty_story_means    = alpha(X[paper_novelty])$total$raw_alpha,
  usefulness_story_means = alpha(X[paper_usefulness])$total$raw_alpha,
  novelty_ratings        = alpha(ratings[paper_novelty])$total$raw_alpha,
  usefulness_ratings     = alpha(ratings[paper_usefulness])$total$raw_alpha
), 2)

# Next: use stories$novelty_paper and stories$usefulness_paper (or your own
# scales) as outcomes when you compare conditions with an effect size and CI.
# stories$condition is already here; for the other story-level variables
# (e.g., dat_group), merge with ai-creativity-stories.csv by story_id.
