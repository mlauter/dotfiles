execute pathogen#infect()
" call pathogen#helptags()

" MAKE THE SYSTEM PASTE BUFFER PLAY NICE
set clipboard=unnamed

" ADD CTRLP FOR FILE NAVIGATION
set runtimepath^=~/.vim/bundle/ctrlp.vim

" DISPLAY LINE NUMBERS
set number
set relativenumber

" HIGHLIGHT CURRENT LINE
set cul

" SET COLOR THEME
" colorscheme Tomorrow-Night-Eighties
" :hi CursorLine   cterm=NONE ctermbg=darkblue ctermfg=white guibg=darkblue guifg=white

syntax enable
"set background=dark
colorscheme monokai

" NO ANNOYING SOUND ON ERRORS
set noerrorbells
set novisualbell
set t_vb=
set tm=500

" ENABLE THE MOUSE
    set mouse=a

"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" => TEXT, TAB AND INDENT RELATED
"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" USE SPACES INSTEAD OF TABS
set expandtab

" BE SMART WHEN USING TABS
set smarttab

" 1 TAB == 4 SPACES
set shiftwidth=4
set tabstop=4

" LINEBREAK ON 500 CHARACTERS
set lbr
set tw=500

set ai "AUTO INDENT
set si "SMART INDENT
set wrap "WRAP LINES

" LETS YOU GO TO ONE PAST END OF LINE
set virtualedit=onemore

set hlsearch " HIGHLIGHT SEARCHES

" SHOW MATCHES BRACES/PARENS/ETC
set showmatch

set ignorecase                     " make searches case-insensitive
set smartcase                      " unless they contain upper-case letters ;)

"Opens the file for a class under cursor with <leader>e
"You can set g:EtsyDir to the root of Etsy's repo
"or leave blank to default to ~/development/Etsyweb
function! OpenEtsyFile()
    if !exists('g:EtsyDir')
        let g:EtsyDir = $HOME . '/development/Etsyweb'
    endif
    let fn = expand("<cword>")
    let fn = substitute(fn, '_', '/', 'g')
    let fn = g:EtsyDir . "/phplib/" . fn . ".php"
    tabe `=fn`
endfunction
nnoremap .e :call OpenEtsyFile()<CR>

" (CTRL P
" https://github.com/burke/matcher
let goodmatch_vim = $HOME . "/.vim/goodmatch.vim"
if filereadable(goodmatch_vim)
    exe 'source' goodmatch_vim
    let g:path_to_matcher = $HOME . "/bin/matcher"
    let g:ctrlp_match_func = { 'match': 'GoodMatch' }
endif

" index the worrrrrld
let g:ctrlp_max_files = 0
let g:ctrlp_user_command = 'find %s -type f | grep -v -P "\.jpg$|\.gif$|\.png$|\.un~|/tmp/"'
let g:ctrlp_open_multiple_files = '2h'
let g:ctrlp_open_new_file = 'h'
let g:ctrlp_show_hidden = 1

" Plugin for loading templates via
" stencil plugin
" https://github.com/mrtazz/vim-stencil
:imap jk <Esc>

" break_on_open is set to 0 so that the debugger will stop at the first breakpoint instead of at the first line of the server, which is the router.
  " path_maps is set as such b/c apache runs the server via a symlink.
let g:vdebug_options = {
    \ "break_on_open" : 0,
    \ "path_maps" : {
    \     "/var/etsy/current" : "/home/<your ldap>/development/Etsyweb",
     \},
\}
