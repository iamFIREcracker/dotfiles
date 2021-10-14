syntax match lispScratchMarker /\v\#;.*/
highlight link lispScratchMarker lispComment

syntax match lispEscapeChar /\v\#\\./
highlight link lispEscapeChar lispString
