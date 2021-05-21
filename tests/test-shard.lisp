(in-package :cl-naive-store.tests)

(defun shard-example (&aux (persist-p t))
  "This example sets up a store and populates a collection with a 100 data documents and then queries
the collection to retrieve the 50 documents that have a :emp-no >= 50.
Only persisted if persist-p is t."
  ;;Clear any residual
  (tear-down-universe)
  ;;Setup the data universe aka the documents that will contain the data
  (setup-universe)
  ;;Generate some data and put it in the universe documents
  (populate-simple-data persist-p :size 100)

  ;;Query the data in the universe
  (query-simple-data))
