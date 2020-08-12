# Version 2020.8.12

Lots of bug fixes and internalized modules.

## Incompatibility Issues:

cl-naive-indexed, cl-naive-document-types, cl-naive-document-types, cl-naive-documents and cl-naive-store-tests will not appear in quicklisp anymore because they dont have their own .asd files.

Loading cl-naive-store now loads all modules by default, to do conditional loading you need to use to add to various features to *features* before you load cl-naive-store.


## Other Work Done:

Empty source files where removed.

Removed top-level-p every where.

Added maintenance back into asd

Added tests for reference objects to tests

Added getx for document-type and element

Disabled validate-xe it needs a lot more work.

Added murmurhash for local-time:timestamp

Dropped write-object and kin

Changed some errors to write to log instead

Added more checks on ensure path for collections.


## Bug fixes


Fixed sed replace that blurred the line between document-type of collection and document-type-def of document.

Fixed getsfx to getxe replacement missed previously

Fixed persist-document to return document

Fixed reference check in type-of-doc-element

Fixed perist-form for references

Fixed perist-parse to check for types in the right place

Fixed find-document-by-hash for naive-indexed

Removed duplicate getx for naive-documents that was clobbering normal behaviour

Fixed method confusion for getx of (document document) (element element) vs document (element element)

Fixed load parsing of child and reference documents where they where not picked up by the type check.

Fixed type-defs parameter order for various functions.

Fixed checking for real changes in persist-document, document-persisted-p was unreliable.

Fixed collection-class init for document-types

Fixed init args for document-store class