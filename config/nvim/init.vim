autocmd BufNewFile,BufRead *.tsx set filetype=typescriptreact

" --- Vim Settings ---
" folddigest
let g:folddigest_options = 'vertical, flexnumwidth'
let g:folddigest_size = 30

function! s:check_back_space() abort
  let col = col('.') - 1
  return !col || getline('.')[col - 1]  =~# '\s'
endfunction

" autoformat
" au BufWrite * :Autoformat
let g:autoformat_autoindent = 0
let g:autoformat_retab = 0
let g:autoformat_remove_trailing_spaces = 0

set completeopt=longest,menu,menuone
"               |       |    +-- Show popup even with one match
"               |       +------- Use popup menu with completions
"               +--------------- Insert longest completion match
set wildmode=longest:full,list:full
"            |            +-- List matches, complete first match
"            +--------------- Complete longest prefix, use wildmenu


" visual
set shortmess+=c               " add to search
set signcolumn=yes             " always show sign column

" NERDTree settings
let g:NERDTreeIgnore = ["^.git$", "^node_modules$","^__pycache__$", "^venv$", "^.vscode$"]
let g:NERDTreeShowHidden=1
let NERDTreeShowHidden = 1                      " show hidden files
let NERDTreeMinimalUI = 1                       " minimal ui
let NERDTreeChDirMode = 2                       " change cwd with root dir
let NERDTreeHijackNetrw = 1                     " always use nerdtree
let NERDTreeDirArrowExpandable = "\u00a0"       " make arrows invisible
let NERDTreeDirArrowCollapsible = "\u00a0"      " make arrows invisible
let NERDTreeNodeDelimiter = "\u263a"            " smiley face


" http://karolis.koncevicius.lt/posts/porn_zen_and_vimrc/
" make n always search forward and N backward
nnoremap <expr> n 'Nn'[v:searchforward]
nnoremap <expr> N 'nN'[v:searchforward]

" make ; always "find" forward and , backward
nnoremap <expr> ; getcharsearch().forward ? ';' : ','
nnoremap <expr> , getcharsearch().forward ? ',' : ';'

" Y behavior consistent with C and D
nnoremap Y y$
" select entire file
nnoremap <leader>V ggVG
" select last insertion
nnoremap gV `[v`]
" paragraph formatting
nnoremap Q gqap
vnoremap Q gq
" make backspace delete selected text
xmap <BS> x

" search for current selection in visual mode
vnoremap <silent> * :<C-u>call VisualSelection('', '')<CR>/<C-R>=@/<CR><CR>
vnoremap <silent> # :<C-u>call VisualSelection('', '')<CR>?<C-R>=@/<CR><CR>

" text alignment
xmap ga <Plug>(EasyAlign)
nmap ga <Plug>(EasyAlign)

" f :: Find
nmap <leader>fw     :Windows<CR>
nmap <leader>fh     :History<CR>
nmap <leader>ft     :Tags<CR>
nmap <leader>fc     :Commits<CR>
nmap <leader>fg     :GFiles<CR>
nmap <leader>fm     :Marks<CR> 
nmap <leader>fs     :Snippets<CR> 
nmap <leader>fc     :Commands<CR>
nmap <leader>fm     :Maps<CR>

" m :: Mode
map <leader>mf      :Goyo<CR>
autocmd! User GoyoEnter Limelight  | set cursorline!
autocmd! User GoyoLeave Limelight! | set cursorline

" o :: Open
nmap <leader>ot    :Ttoggle<CR>
nmap <leader>ob    :TagbarToggle<CR>

" open visual selection in new window
vmap o :NR<CR>

" --- Event Listeners ---
autocmd BufEnter * lcd %:p:h | " nerdtree opens in current dir
autocmd FocusLost * :wa | " save file when focus is lost

" remove trailing whitespace on save
" https://gitlab.com/kmidkiff/vim-configuration/-/blob/master/vimrc
function! RemoveWhitespace()
  :%s/\s\+$//e
endfunction

inoremap <silent><expr> <TAB>
      \ pumvisible() ? "\<C-n>" :
      \ <SID>check_back_space() ? "\<TAB>" :
      \ coc#refresh()
inoremap <expr><S-TAB> pumvisible() ? "\<C-p>" : "\<C-h>"

function! s:check_back_space() abort
  let col = col('.') - 1
  return !col || getline('.')[col - 1]  =~# '\s'
endfunction

" Make <CR> auto-select the first completion item and notify coc.nvim to
" format on enter, <cr> could be remapped by other vim plugin
inoremap <silent><expr> <cr> pumvisible() ? coc#_select_confirm()
      \: "\<C-g>u\<CR>\<c-r>=coc#on_enter()\<CR>"


" Use K to show documentation in preview window.
nnoremap <silent> K :call <SID>show_documentation()<CR>

function! s:show_documentation()
  if (index(['vim','help'], &filetype) >= 0)
    execute 'h '.expand('<cword>')
  elseif (coc#rpc#ready())
    call CocActionAsync('doHover')
  else
    execute '!' . &keywordprg . " " . expand('<cword>')
  endif
endfunction

" Highlight the symbol and its references when holding the cursor.
autocmd CursorHold * silent call CocActionAsync('highlight')

" Symbol renaming.
nmap <leader>rn <Plug>(coc-rename)

" Formatting selected code.
xmap <leader>f  <Plug>(coc-format-selected)
nmap <leader>f  <Plug>(coc-format-selected)

augroup mygroup
  autocmd!
  " Setup formatexpr specified filetype(s).
  autocmd FileType typescript,json setl formatexpr=CocAction('formatSelected')
  " Update signature help on jump placeholder.
  autocmd User CocJumpPlaceholder call CocActionAsync('showSignatureHelp')
augroup end

