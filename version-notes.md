# Version 2020.07.08

A majour code refactoring exercise was under taken. Not only was code improved where possible new functionality was added and in a few cases removed.

## Incompatibility Issues:

- load-store-collections was renamed to load-collections

- loaded-p slot was removed form collection, store and universe. Use data-loaded-p in the future.

- Removed handle-duplicates on universe, store and collection and replaced it with keys slot on collection that defaults to :key. This simplifies behaviour for handling duplicates, and also speeds up handling of duplicates. If no keys is set to nil then duplicates are allowed, if :key is not found in the document duplicates will occur.

- Removed must-handle-duplicates since it was part and parcel of handle-duplicates.


## Other Work Done:

- collection-container-loaded-p was added to use :around load-data method.

- data-loaded-p was added to test if a collection, store or universe was truely/completely loaded.

- Added :before method for query-data query-document and naive-reduce to ensure data is lazy loaded.

- find-document-by-hash was added to help with different collection containers.

- set-print-reabability and print-readability-p was to set on *print-readably* for write-document

- Exported write-document so that it can be specialized if needed.

- Moved lazy loading when querying to :before methods.

- Updated and changed a lot of doc strings.

- Refactored code al over the place.

- Adjusted tests to cope with compatibility breaks.

- Stripped out experimental avl tree stuff.

- Refactored the code naive-store-documents persist.lisp heavily.

## Bug fixes

- Fixed add-document in core to replace document when key values match to an existing document and not to just ignore it. 