syntax match lispScratchMarker /\v\#;.*/
highlight link lispScratchMarker lispComment

syntax match lispEscapeChar /\v\#\\./
highlight link lispEscapeChar lispString

syntax match lispExcludedForm /\v\#\+nil \([^)]+\)/
highlight link lispExcludedForm lispComment
syn cluster lispBaseListCluster add=lispExcludedForm

syntax match lispExcludedElement /\v\#\+nil [^(][^ ]+/
highlight link lispExcludedElement lispComment
syn cluster lispBaseListCluster add=lispExcludedElement
