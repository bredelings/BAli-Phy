-- See https://github.com/probmods/webppl/blob/dev/examples/lda.wppl

import Probability

vocabulary = ["bear", "wolf", "python", "prolog"];

topics = ["topic1","topic2"]

docs = [
  ("doc1", words "bear wolf bear wolf bear wolf python wolf bear wolf"),
  ("doc2", words "python prolog python prolog python prolog python prolog python prolog"),
  ("doc3", words "bear wolf bear wolf bear wolf bear wolf bear wolf"),
  ("doc4", words "python prolog python prolog python prolog python prolog python prolog"),
  ("doc5", words "bear wolf bear python bear wolf bear wolf bear wolf")
  ]

docs_words = map snd docs

sample_word_dist = do
  word_frequencies <- symmetric_dirichlet_on vocabulary 1.0
  let word_dist = categorical_on word_frequencies
  return (word_dist, word_frequencies)

sample_topics_for_doc nwords = do
  topic_frequencies <- symmetric_dirichlet (length topics) 1.0
  topics <- iid nwords $ categorical topic_frequencies
  return (topics, topic_frequencies)

word_dist_for_doc word_dist_for_topic nwords = do
  (topics_for_doc_words, topic_frequencies) <- sample_topics_for_doc nwords
  return (independent [ word_dist_for_topic!!topic | topic <- topics_for_doc_words ], topic_frequencies)

model = do
  (word_dist_for_topic, word_frequencies_for_topic) <- unzip `liftM` independent [sample_word_dist | topic <- topics]
      
  (word_dists, topic_frequencies) <- unzip `liftM` independent [word_dist_for_doc word_dist_for_topic nwords | w <- docs_words, let nwords = length w]

  let loggers = ["word_frequencies_for_topic" %=% word_frequencies_for_topic,
                 "topic_frequencies_for_doc" %=% topic_frequencies]
  return (word_dists, loggers)


main = do
  (word_dists, loggers) <- random $ model
  observe (independent word_dists) docs_words
  return loggers

main2 = do
  (dist,frequencies) <- random $ sample_word_dist
  ws <- random $ iid 10 dist
  observe (independent [iid 2 dist,iid 3 dist]) [["bear","python"],["wolf","bear","bear"]]

  (word_dist_for_topic, word_frequencies_for_topic) <- random $ unzip `liftM` independent [sample_word_dist | topic <- topics]
  let nwords1 = length (docs_words!!0)
  (topics_for_doc1, topic_frequencies_for_doc1) <- random $ sample_topics_for_doc nwords1
  (word_dist_for_doc1, tf_for_doc1) <- random $ word_dist_for_doc word_dist_for_topic nwords1
  words1 <- random $ word_dist_for_doc1
  observe word_dist_for_doc1 (docs_words!!0)
  return ["words" %=% ws,
          "words1" %=% words1,
          "topics1" %=% topics_for_doc1,
          "topics_frequencies1" %=% topic_frequencies_for_doc1,
          "frequencies" %=% frequencies,
          "wf_for_topic" %=% word_frequencies_for_topic]

