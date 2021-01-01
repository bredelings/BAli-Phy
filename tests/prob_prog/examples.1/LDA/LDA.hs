-- See https://github.com/probmods/webppl/blob/dev/examples/lda.wppl

import Probability

vocabulary = ["bear", "wolf", "python", "prolog"];

ntopics = 2

word_frequencies_dist = symmetric_dirichlet_on vocabulary 1.0

word_dist_for_doc word_frequencies_for_topic nwords = do
  topic_frequencies <- symmetric_dirichlet ntopics 1.0
  topics  <- iid nwords $ categorical topic_frequencies
  let word_dist = independent [ categorical_on $ word_frequencies_for_topic!!topic | topic <- topics ]
  return (word_dist, topic_frequencies)

prior doc_lengths = do
  word_frequencies_for_topic <- iid ntopics word_frequencies_dist
      
  (word_dists, topic_frequencies) <- unzip `liftM` independent [word_dist_for_doc word_frequencies_for_topic nwords | nwords <- doc_lengths]

  let loggers = ["word_frequencies_for_topic" %=% word_frequencies_for_topic,
                 "topic_frequencies_for_doc" %=% topic_frequencies]

  return (word_dists, loggers)


observe_data docs = do
  let doc_lengths = map length docs
  (word_dists, loggers) <- sample $ prior doc_lengths
  docs ~> independent word_dists
  return loggers


docs = map words [
        "bear wolf bear wolf bear wolf python wolf bear wolf",
        "python prolog python prolog python prolog python prolog python prolog",
        "bear wolf bear wolf bear wolf bear wolf bear wolf",
        "python prolog python prolog python prolog python prolog python prolog",
        "bear wolf bear python bear wolf bear wolf bear wolf"]

main = do
  let model = observe_data docs
  mcmc model
