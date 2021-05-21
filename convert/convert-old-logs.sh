#!/bin/bash

find $1 -type f -not -path '*/\.*' -name '*\.log' -exec sed -i 's/:DATA-TYPE/:DOCUMENT-TYPE/g' {} +

find $1 -type f -not -path '*/\.*' -name '*\.col' -exec sed -i 's/:DATA-TYPE/:DOCUMENT-TYPE/g' {} +

find $1 -type f -not -path '*/\.*' -name '*\.log' -exec sed -i 's/:VALUES/:ELEMENTS/g' {} +

find $1 -type f -not -path '*/\.*' -name '*\.type' -exec sed -i 's/:FIELDS/:ELEMENTS/g' {} +

find $1 -type f -not -path '*/\.*' -name '*\.type' -exec sed -i 's/:TYPE-DEF/:CONCRETE-TYPE/g' {} +
