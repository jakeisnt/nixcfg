" Install vim-plug if not already installed
if empty(glob('$XDG_CONFIG_HOME/nvim/autoload/plug.vim'))
    silent !curl -fLo $XDG_CONFIG_HOME/nvim/autoload/plug.vim --create-dirs
                \ https://raw.githubusercontent.com/junegunn/vim-plug/master/plug.vim
    autocmd VimEnter * PlugInstall
endif

" --- Plugins ---
call plug#begin('$XDG_CONFIG_HOME/nvim/plugged')
" editing
Plug 'tpope/vim-surround', {'tag': 'v2.1'}         " close parens
Plug 'junegunn/vim-easy-align', {'tag': 'v2.10.0'} " align text
Plug 'matze/vim-move', {'tag': 'v1.4'}             " move lines without cut-paste
Plug 'tomtom/tcomment_vim', {'tag': 'v3.08.1'}     " autocomment support
Plug 'tpope/vim-repeat', {'tag': 'v1.2'}           " improves repeats
Plug 'tpope/vim-unimpaired', {'tag': 'v2.0'}       " bracket mapping
Plug 'chrisbra/NrrwRgn', {'tag': 'v0.33'}          " narrow region for editing file in new buffer
Plug 'wellle/targets.vim', {'tag': 'v0.5.0'}       " better targets

" Language server support
Plug 'neoclide/coc.nvim', {'do': { -> coc#util#install()}}
Plug 'neoclide/coc-snippets' 
Plug 'neoclide/coc-eslint'
Plug 'neoclide/coc-css'
Plug 'neoclide/coc-json'
Plug 'neoclide/coc-git'
Plug 'neoclide/coc-clangd'  " C/C++/Objective-C
Plug 'neoclide/coc-sh'      " shell scripting
Plug 'neoclide/coc-lists' 
Plug 'neoclide/coc-highlight'
Plug 'fannheyward/coc-rust-analyzer'

" Navigation
Plug 'scrooloose/nerdtree', {'tag': 'v6.9.11'} " directory navigation
Plug 'Xuyuanp/nerdtree-git-plugin'             " git integration for nerdtree 
Plug 'junegunn/fzf', { 'do': { -> fzf#install() } }
Plug 'junegunn/fzf.vim'                        " fuzzy file finding
Plug 'justinmk/vim-sneak', {'tag': 'v1.9'}     " hunt with two chars
Plug 'majutsushi/tagbar', {'tag': 'v2.7'}      " browse tags of current file
Plug 'vim-scripts/utl.vim'                     " text linking

" Appearance
Plug 'vim-airline/vim-airline', {'tag': 'v0.11'} " status bar
Plug 'junegunn/goyo.vim', {'tag': 'v1.6.0'}      " minimalist vim
Plug 'junegunn/limelight.vim'                    " highlight current paragraph
Plug 'arcticicestudio/nord-vim', {'tag': 'v0.15.0'}     
Plug 'jlesquembre/coc-conjure'
Plug 'Olical/conjure'

" git
Plug 'airblade/vim-gitgutter'               " displays git diff info 
Plug 'tpope/vim-fugitive', {'tag': 'v3.2'}  " git convenience functions
Plug 'rbong/vim-flog', {'tag': 'v1.2.0'}    " git branch viewer

" tools
Plug 'farmergreg/vim-lastplace' " save place in file
Plug 'wincent/terminus'         " better terminal integration
Plug 'direnv/direnv.vim'        " direnv support

" file type support
Plug 'axvr/org.vim'             " orgm ode support
Plug 'inkarkat/vim-SyntaxRange' " syntax for just part of file
Plug 'reedes/vim-pencil'        " writing
call plug#end()

" --- Vim Settings ---
" folddigest
let g:folddigest_options = 'vertical, flexnumwidth'
let g:folddigest_size = 30


" FZF
" match fzf colors with vim scheme
let g:fzf_colors = {
            \ 'fg':      ['fg', 'Normal'],
            \ 'bg':      ['bg', 'Normal'],
            \ 'hl':      ['fg', 'Comment'],
            \ 'fg+':     ['fg', 'CursorLine', 'CursorColumn', 'Normal'],
            \ 'bg+':     ['bg', 'CursorLine', 'CursorColumn'],
            \ 'hl+':     ['fg', 'Statement'],
            \ 'info':    ['fg', 'PreProc'],
            \ 'border':  ['fg', 'Ignore'],
            \ 'prompt':  ['fg', 'Conditional'],
            \ 'pointer': ['fg', 'Exception'],
            \ 'marker':  ['fg', 'Keyword'],
            \ 'spinner': ['fg', 'Label'],
            \ 'header':  ['fg', 'Comment'] }

let g:fzf_preview_window = 'right:60%'
let g:fzf_buffers_jump = 1

" utilsnips TODO
" let g:UltiSnipsExpandTrigger='<tab>'
" let g:UltiSnipsJumpForwardTrigger='<c-b>'
" let g:UltiSnipsJumpBackwardTrigger='<c-z>'

" autoformat
" au BufWrite * :Autoformat
let g:autoformat_autoindent = 0
let g:autoformat_retab = 0
let g:autoformat_remove_trailing_spaces = 0

" editing
set expandtab             " expand tabs to spaces
set tabstop=4             " tabs are 4 spaces
set shiftwidth=4          " reindents are 4 spaces
set smartindent           " determines indentation with context
set magic                 " enable regex
set formatoptions+=j      " delete comment character when joining lines
set clipboard=unnamedplus " shared system clipboard
set textwidth=0           " line wrap
set wrapmargin=0
set undofile              " undo history persists across sessions

" search
set incsearch " highlight as characters are entered
set hlsearch  " persistent highlight of prev search
set smartcase " ignore case if search all lowercase
set wildmenu  " show menu of suggestions with tab-complete

" folding
set foldenable        " turn on folding
set foldmethod=marker " marker indicates folds TODO use markers?
set foldlevelstart=10
set modelines=1 " use modeline to configure marker
" vim:foldmethod=marker:foldlevel=0 (put at bottom of file for fold configuraion)

" performance
set lazyredraw   " only redraw components on change
set nobackup     " remove backups
set nowritebackup
set noswapfile
set hidden       " cache more
set history=1000 " default is 20
set complete-=5  " limit autocomplete
set scrolloff=3  " scroll 3l in advance of window
set backspace=2  " backspace is normal

" show hidden characters and linewraps
" https://github.com/dm3/cygwin-config/blob/master/.vimrc
set list
set listchars=tab:▸\ ,extends:❯,precedes:❮
set showbreak=↪
set completeopt=longest,menu,menuone
"               |       |    +-- Show popup even with one match
"               |       +------- Use popup menu with completions
"               +--------------- Insert longest completion match
set wildmode=longest:full,list:full
"            |            +-- List matches, complete first match
"            +--------------- Complete longest prefix, use wildmenu

set splitbelow  " sensible splits
set splitright


" visual
set cmdheight=1                " one line for commands
set shortmess+=c               " add to search
set signcolumn=yes             " always show sign column
syntax on                      " syntax highlighting
filetype plugin indent on      " file type detection
set cursorline                 " current line is visible
set showmatch                  " show matching braces
syntax enable                  " enable syntax highlighting
set termguicolors
set background=dark
highlight Comment gui=italic | " make comments italic
set foldmethod=indent

let g:limelight_conceal_ctermfg = 'Gray'
let g:gitgutter_sign_column_always=1 " always display gutter

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

" airline
" TODO review config
let g:airline#extensions#coc#enabled          = 1
let g:airline#extensions#tabline#enabled      = 1
let g:airline#extensions#tabline#formatter    = 'unique_tail_improved'
let g:airline#extensions#bufferline#enabled   = 1
let g:airline#extensions#gutentags#enabled    = 1
let g:airline_powerline_fonts                 = 1
let g:airline_left_sep                        = '█'
let g:airline_right_sep                       = '█'
let g:airline_left_alt_sep                    = ' '
let g:airline#extensions#tabline#left_sep     = '█'
let g:airline#extensions#tabline#right_sep    = '█'
let g:airline#extensions#tabline#left_alt_sep = ' '


" --- Key Mappings ---
let mapleader=" "

" --- Text Navigation ---
" travel by visible lines
map j gj
map k gk

" avoid pressing shift
nnoremap ; :

" Browser-similar tab navigation
nnoremap <C-T> :tabnew<CR>
nnoremap <C-W> :tabclose<CR>
nnoremap <C-J> :tabprev<CR>
nnoremap <C-K> :tabnext<CR>
nnoremap <silent> <Tab> :bnext<CR>
nnoremap <silent> <S-Tab> :bprevious<CR>

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

" cancel search with esc
nnoremap <silent> <Esc> :nohlsearch<Bar>:echo<CR>

" text alignment
xmap ga <Plug>(EasyAlign)
nmap ga <Plug>(EasyAlign)

" --- Vim Navigation ---
nnoremap <leader>.    :NERDTreeToggle<CR>
" toggle folds
nnoremap <leader><tab> za
" toggle with previous file
nnoremap <Leader><Leader> :e#<CR>

" f :: Find
" TODO: file path completion in edit mode
nmap <leader>ff     :Files<CR>
nmap <leader>fw     :Windows<CR>
nmap <leader>fh     :History<CR>
nmap <leader>ft     :Tags<CR>
nmap <leader>fc     :Commits<CR>
nmap <leader>fl     :Lines<CR>
nmap <leader>fb     :Buffers<CR>
nmap <leader>fg     :GFiles<CR>
nmap <leader>fm     :Marks<CR> 
nmap <leader>fs     :Snippets<CR> 
nmap <leader>fc     :Commands<CR>
nmap <leader>fm     :Maps<CR>
nmap <leader>ft     :NERDTreeFind<CR>

" g :: Git
nmap <leader>gg    :Gstatus<CR>

" m :: Mode
map <leader>mf      :Goyo<CR>
autocmd! User GoyoEnter Limelight  | set cursorline!
autocmd! User GoyoLeave Limelight! | set cursorline

" o :: Open
nmap <leader>ot    :Ttoggle<CR>
nmap <leader>ob    :TagbarToggle<CR>

" open visual selection in new window
vmap o :NR<CR>

" u :: Undo
map <leader>ut     :UndotreeToggle<CR>

" v :: Vimux (Tmux Vim Interaction)
map <leader>vp :VimuxPromptCommand<CR>
map <leader>vl :VimuxRunLastCommand<CR>
map <leader>vi :VimuxInspectRunner<CR>
map <leader>vz :VimuxZoomRunner<CR>

" w :: (split) Window
" commonly used shortcuts
nnoremap <leader>wh <C-W>h
nnoremap <leader>wj <C-W>j
nnoremap <leader>wk <C-W>k
nnoremap <leader>wl <C-W>l
nnoremap <leader>ws  :sp<CR>
nnoremap <leader>wv  :vsp<CR>
nnoremap <leader>wc  <C-W>c

" --- Event Listeners ---
autocmd BufEnter * lcd %:p:h | " nerdtree opens in current dir
autocmd FocusLost * :wa | " save file when focus is lost

" remove trailing whitespace on save
" https://gitlab.com/kmidkiff/vim-configuration/-/blob/master/vimrc
function! RemoveWhitespace()
    :%s/\s\+$//e
endfunction

if !empty(glob("$XDG_CONFIG_HOME/nvim/theme.vim"))
  source $XDG_CONFIG_HOME/nvim/theme.vim
endif


" Coc settings
let g:coc_global_extensions = ['coc-conjure', 'coc-rust-analyzer', 'coc-json', 'coc-clangd', 'coc-eslint', 'coc-sh', 'coc-git']

inoremap <silent><expr> <TAB>
      \ pumvisible() ? "\<C-n>" :
      \ <SID>check_back_space() ? "\<TAB>" :
      \ coc#refresh()
inoremap <expr><S-TAB> pumvisible() ? "\<C-p>" : "\<C-h>"

" Use <c-space> to trigger completion.
inoremap <silent><expr> <c-space> coc#refresh()

function! s:check_back_space() abort
  let col = col('.') - 1
  return !col || getline('.')[col - 1]  =~# '\s'
endfunction

" Make <CR> auto-select the first completion item and notify coc.nvim to
" format on enter, <cr> could be remapped by other vim plugin
inoremap <silent><expr> <cr> pumvisible() ? coc#_select_confirm()
                              \: "\<C-g>u\<CR>\<c-r>=coc#on_enter()\<CR>"

" Use `[g` and `]g` to navigate diagnostics
" Use `:CocDiagnostics` to get all diagnostics of current buffer in location list.
nmap <silent> [g <Plug>(coc-diagnostic-prev)
nmap <silent> ]g <Plug>(coc-diagnostic-next)

" GoTo code navigation.
nmap <silent> gd <Plug>(coc-definition)
nmap <silent> gy <Plug>(coc-type-definition)
nmap <silent> gi <Plug>(coc-implementation)
nmap <silent> gr <Plug>(coc-references)

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

" Applying codeAction to the selected region.
" Example: `<leader>aap` for current paragraph
xmap <leader>a  <Plug>(coc-codeaction-selected)
nmap <leader>a  <Plug>(coc-codeaction-selected)

" Remap keys for applying codeAction to the current buffer.
nmap <leader>ac  <Plug>(coc-codeaction)
" Apply AutoFix to problem on the current line.
nmap <leader>qf  <Plug>(coc-fix-current)

" Map function and class text objects
" NOTE: Requires 'textDocument.documentSymbol' support from the language server.
xmap if <Plug>(coc-funcobj-i)
omap if <Plug>(coc-funcobj-i)
xmap af <Plug>(coc-funcobj-a)
omap af <Plug>(coc-funcobj-a)
xmap ic <Plug>(coc-classobj-i)
omap ic <Plug>(coc-classobj-i)
xmap ac <Plug>(coc-classobj-a)
omap ac <Plug>(coc-classobj-a)

" Remap <C-f> and <C-b> for scroll float windows/popups.
nnoremap <silent><nowait><expr> <C-f> coc#float#has_scroll() ? coc#float#scroll(1) : "\<C-f>"
nnoremap <silent><nowait><expr> <C-b> coc#float#has_scroll() ? coc#float#scroll(0) : "\<C-b>"
inoremap <silent><nowait><expr> <C-f> coc#float#has_scroll() ? "\<c-r>=coc#float#scroll(1)\<cr>" : "\<Right>"
inoremap <silent><nowait><expr> <C-b> coc#float#has_scroll() ? "\<c-r>=coc#float#scroll(0)\<cr>" : "\<Left>"
vnoremap <silent><nowait><expr> <C-f> coc#float#has_scroll() ? coc#float#scroll(1) : "\<C-f>"
vnoremap <silent><nowait><expr> <C-b> coc#float#has_scroll() ? coc#float#scroll(0) : "\<C-b>"

" Use CTRL-S for selections ranges.
" Requires 'textDocument/selectionRange' support of language server.
nmap <silent> <C-s> <Plug>(coc-range-select)
xmap <silent> <C-s> <Plug>(coc-range-select)

" Add `:Format` command to format current buffer.
command! -nargs=0 Format :call CocAction('format')

" Add `:Fold` command to fold current buffer.
command! -nargs=? Fold :call     CocAction('fold', <f-args>)

" Add `:OR` command for organize imports of the current buffer.
command! -nargs=0 OR   :call     CocAction('runCommand', 'editor.action.organizeImport')

" Add (Neo)Vim's native statusline support.
" NOTE: Please see `:h coc-status` for integrations with external plugins that
" provide custom statusline: lightline.vim, vim-airline.
set statusline^=%{coc#status()}%{get(b:,'coc_current_function','')}

" Mappings for CoCList
" Show all diagnostics.
nnoremap <silent><nowait> <space>a  :<C-u>CocList diagnostics<cr>
" Manage extensions.
nnoremap <silent><nowait> <space>e  :<C-u>CocList extensions<cr>
" Show commands.
nnoremap <silent><nowait> <space>c  :<C-u>CocList commands<cr>
" Find symbol of current document.
nnoremap <silent><nowait> <space>o  :<C-u>CocList outline<cr>
" Search workspace symbols.
nnoremap <silent><nowait> <space>s  :<C-u>CocList -I symbols<cr>
" Do default action for next item.
nnoremap <silent><nowait> <space>j  :<C-u>CocNext<CR>
" Do default action for previous item.
nnoremap <silent><nowait> <space>k  :<C-u>CocPrev<CR>
" Resume latest coc list.
nnoremap <silent><nowait> <space>p  :<C-u>CocListResume<CR>


