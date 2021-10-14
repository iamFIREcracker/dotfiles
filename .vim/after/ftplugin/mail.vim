setlocal nobackup noswapfile nowritebackup

function! EnableFormatFlowed() abort "{{{ https://rinzewind.org/blog-en/2017/a-small-trick-for-sending-flowed-mail-in-mutt-with-vim.html
    setlocal textwidth=72
    setlocal formatoptions=watqc
    setlocal nojs
    match ErrorMsg '\s\+$'
endfunction " }}}
au User MailProfilePersonal call EnableFormatFlowed()
au User MailProfilePersonal let b:goobookprg='goobook'
au User MailProfileWork setlocal textwidth=0 wrap
au User MailProfileWork let b:goobookprg='aadbook'

function! EnableMailProfile() abort "{{{
    let first_line = getline('1')
    if first_line =~? '\vFrom:.*\<matteo\@matteolandi.net\>'
        doautocmd User MailProfilePersonal
    else
        doautocmd User MailProfileWork
    endif
endfunction " }}}

call EnableMailProfile()
