" vim: set fdm=marker :

if !exists("g:fetcher_list_args")
    let g:fetcher_list_args = ""
endif
if !exists("g:fetcher_info_args")
    let g:fetcher_info_args = ""
endif
if !exists("g:fetcher_download_args")
    let g:fetcher_download_args = ""
endif
if !exists("g:fetcher_search_args")
    let g:fetcher_search_args = ""
endif

func! MakeArgsCmd(arg)
    let var = 'g:fetcher_' . a:arg . '_args'
    let in = input("New arguments to '".a:arg."': ", eval(var))
    let cmd = "let " . var . "='" . in . "'"
    exe cmd
endfunc

func! ExecCmd(arg)
    let args = eval("g:fetcher_".a:arg."_args")
    echo "Running action '".a:arg."' with arguments: " . args
    exe 'w | !runhaskell Main.hs ' . a:arg . " " . args
endfunc

" Convenience access to testing
nnoremap <F9> :so torrent-fetcher.vim <CR>

nnoremap <F7>i :call ExecCmd('info')<CR>
nnoremap <F7>d :call ExecCmd('download')<CR>
nnoremap <F7>s :call ExecCmd('search')<CR>
nnoremap <F7>l :call ExecCmd('list')<CR>

nnoremap <F8>i :exe "call MakeArgsCmd('info')"<CR>
nnoremap <F8>d :exe "call MakeArgsCmd('download')"<CR>
nnoremap <F8>s :exe "call MakeArgsCmd('search')"<CR>
nnoremap <F8>l :exe "call MakeArgsCmd('list')"<CR>
