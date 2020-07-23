20 Jul 2020

Empty source files where removed.

21 Jul 2020

Removed top-level-p every where.

Fixed sed replace that blurred the line between document-type of collection and document-type-def of document.

Fixed getsfx to getxe replacement missed previously

Fixed persist-document to return document

Fixed reference check in type-of-doc-element

Fixed perist-form for references

Fixed perist-parse to check for types in the right place

Fixed find-document-by-hash for naive-indexed

22 Jun 2020

Added maintenance back into asd

Added tests for reference objects to tests

Removed duplicate getx for naive-documents that was clobbering normal behaviour

Removed with-standard-io-syntax from with-open-file-lock to prevent *print-readably* defualting to t. The default behaviour for naive-store should *print-readably* = nil.

Fixed method confusion for getx of (document document) (element element) vs document (element element)

Added getx for document-type and element

Fixed load parsing of child and reference documents where they where not picked up by the type check.

Fixed type-defs parameter order for various functions.

Fixed checking for real changes in persist-document, document-persisted-p was unreliable.

Fixed collection-class init for document-types