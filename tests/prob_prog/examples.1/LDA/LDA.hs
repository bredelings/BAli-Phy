-- See https://github.com/probmods/webppl/blob/dev/examples/lda.wppl

import Probability

vocabulary = ["bear", "wolf", "python", "prolog"];

ntopics = 2

docs = [
  ("doc1", words "bear wolf bear wolf bear wolf python wolf bear wolf"),
  ("doc2", words "python prolog python prolog python prolog python prolog python prolog"),
  ("doc3", words "bear wolf bear wolf bear wolf bear wolf bear wolf"),
  ("doc4", words "python prolog python prolog python prolog python prolog python prolog"),
  ("doc5", words "bear wolf bear python bear wolf bear wolf bear wolf")
  ]

docs_words = map snd docs

doc_lengths = map length docs_words

word_frequencies_dist = symmetric_dirichlet_on vocabulary 1.0

sample_topics_for_doc nwords = do
  topic_frequencies <- symmetric_dirichlet ntopics 1.0
  topics <- iid nwords $ categorical topic_frequencies
  return (topics, topic_frequencies)

word_dist_for_doc word_frequencies_for_topic nwords = do
  (topics_for_doc_words, topic_frequencies) <- sample_topics_for_doc nwords
  return (independent [ categorical_on $ word_frequencies_for_topic!!topic | topic <- topics_for_doc_words ], topic_frequencies)

model = do
  word_frequencies_for_topic <- iid ntopics word_frequencies_dist
      
  (word_dists, topic_frequencies) <- unzip `liftM` independent [word_dist_for_doc word_frequencies_for_topic nwords | nwords <- doc_lengths]

  let loggers = ["word_frequencies_for_topic" %=% word_frequencies_for_topic,
                 "topic_frequencies_for_doc" %=% topic_frequencies]

  return (word_dists, loggers)


main = do
  (word_dists, loggers) <- random $ model
  observe (independent word_dists) docs_words
  return loggers
