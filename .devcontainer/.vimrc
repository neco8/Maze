" leaderを設定する
let mapleader = "\<Space>"

" matchit.vim有効
set nocompatible
runtime macros/matchit.vim

" コマンドラインの履歴上限を増やす
set history=1000

" 折返しを見せる
set showbreak=➥

" すべての行を通る
noremap j gj
noremap k gk

" 検索でC-p/C-nを↑↓と変換する
cnoremap <C-p> <Up>
cnoremap <C-n> <Down>

" indent fix paste
nnoremap p ]p
nnoremap P ]P

nnoremap Y mzVy`z

" g; ⇔ g,
nnoremap g: g,
nnoremap g, g;
nnoremap g; <Nop>

" escape with jj
inoremap <silent> jj <Esc>
set timeoutlen=250

" 検索はvery magic前提
nnoremap / /\v
cnoremap ve; \v

" アクティブなディレクトリを手早く展開
cnoremap <expr> %% getcmdtype() == ':' ? expand('%:h').'/' : '%%'

" 検索時画面の真ん中に表示
nnoremap n nzz
nnoremap N Nzz

" plannerのコマンドを扱いやすく
nnoremap _t V:!planner function -t<CR>
vnoremap _t :!planner function -t<CR>
nnoremap _k V:!planner function -k<CR>
vnoremap _k :!planner function -k<CR>
nnoremap _at V:!planner function --add-habit-tag 
nnoremap __ :%!planner function<CR>
vnoremap __ :!planner function<CR>
" nnoremap _c :%!planner compile -f %<CR>

" quickfix の一覧を自動で開く
autocmd QuickFixCmdPost *grep* cwindow

" ; :
nnoremap ; :
nnoremap : ;
vnoremap ; :
vnoremap : ;

function s:indent(is_visual_mode, is_back)
	let s:searchflag = (a:is_back ? "b" : "") . "es"
	let s:searchstr = (a:is_back ? "<" : ">")
	call search('^' .. matchstr(getline('.'), '\(^\s*\)') .. '\%' .. s:searchstr .. line('.') .. 'l\S', s:searchflag)
	if a:is_visual_mode
		return "v``o"
	else
		return ""
	endif
endfunction

" 同じインデントレベルで前後
nnoremap <silent> <expr> <script> [i :call <SID>indent(0,1)<CR>
vnoremap <silent> <expr> <script> [i :call <SID>indent(1,1)<CR>
nnoremap <silent> <expr> <script> ]i :call <SID>indent(0,0)<CR>
vnoremap <silent> <expr> <script> ]i :call <SID>indent(1,0)<CR>

" cursor
set cursorline
set cursorcolumn

" 行番号を表示
set number

" 全ての数字を10進数として見る
set nrformats=alpha

" {でインデント、}でインデント浅く。他にもif, elseなどインデント
set smartindent

" 空白系文字可視化
set list
set listchars=tab:>-,space:_,trail:!

" nvlを読み込む
autocmd BufRead,BufNewFile *.nvl setfiletype nvl

" python
autocmd FileType python setl autoindent
autocmd FileType python setl smartindent cinwords=if,elif,else,for,while,try,except,finally,def,class
autocmd FileType python setl tabstop=8 expandtab shiftwidth=4 softtabstop=4
" コメント時のインデント
inoremap # X<C-H>#

" vue
autocmd BufRead,BufNewFile *.vue setfiletype vue
autocmd FileType vue syntax sync fromstart
autocmd FileType vue setl autoindent
autocmd FileType vue setl smartindent
autocmd FileType vue setl expandtab tabstop=2 shiftwidth=2 softtabstop=2

" js
autocmd FileType javascript setl autoindent
autocmd FileType javascript setl smartindent
autocmd FileType javascript setl expandtab tabstop=2 shiftwidth=2 softtabstop=2

" haskell
autocmd FileType haskell setl autoindent
autocmd FileType haskell setl smartindent
autocmd FileType haskell setl expandtab tabstop=2 shiftwidth=2

" markdown
autocmd FileType markdown setl autoindent
autocmd FileType markdown setl smartindent
autocmd FileType markdown setl expandtab tabstop=2 shiftwidth=2
autocmd FileType markdown set foldmethod=manual
let g:vim_markdown_folding_disabled = 1
let g:vim_markdown_new_list_item_indent = 0

" expressive writing
autocmd BufNewFile,BufRead *.ew.md setlocal filetype=ew
autocmd FileType ew syntax on
autocmd FileType ew setl autoindent
autocmd FileType ew setl smartindent
autocmd FileType ew setl expandtab tabstop=2 shiftwidth=2 softtabstop=2

" font
set guifont=Cica\ 13

" 検索時大文字小文字を無視
set wildignorecase
set wildmenu
set wildmode=full

" エクスプレッシブライティング
" function! s:wopen()
" execute ":tabe ~/Memo/ExpressiveWriting/".strftime('%Y-%m-%d_%H:%M:%S').".exwr"
" endfunction
" nnoremap <silent> <Leader>w :<C-u>call <SID>wopen()<CR>
" " メモ
" function! s:memoopen()
" execute ":tabe ~/Memo/Todo/".strftime('%Y-%m-%d').".memo"
" endfunction
" nnoremap <silent> <Leader>m :<C-u>call <SID>memoopen()<CR>

" undoを永続的に
if has('persistent_undo')
	set undodir=~/.vim/undo
	set undofile
endif

" クリップボード
set clipboard=unnamedplus,autoselect

" " auto-save
" let g:auto_save_in_insert_mode = 1
" let g:auto_save_silent = 1
" let g:auto_save = 1
" set noswapfile

" プラグインが実際にインストールされるディレクトリ
let s:dein_dir = expand('~/.cache/dein')
" dein.vim 本体
let s:dein_repo_dir = s:dein_dir . '/repos/github.com/Shougo/dein.vim'

" dein.vim がなければ github から落としてくる
if &runtimepath !~# '/dein.vim'
  if !isdirectory(s:dein_repo_dir)
    execute '!git clone https://github.com/Shougo/dein.vim' s:dein_repo_dir
  endif
  execute 'set runtimepath^=' . fnamemodify(s:dein_repo_dir, ':p')
endif

" 設定開始
if dein#load_state(s:dein_dir)
  call dein#begin(s:dein_dir)

  " プラグインリストを収めた TOML ファイル
  " 予め TOML ファイル（後述）を用意しておく
  let g:rc_dir    = expand('~/.vim/rc')
  let s:toml      = g:rc_dir . '/dein.toml'
  " let s:lazy_toml = g:rc_dir . '/dein_lazy.toml'

  " TOML を読み込み、キャッシュしておく
  call dein#load_toml(s:toml,      {'lazy': 0})
  " call dein#load_toml(s:lazy_toml, {'lazy': 1})

  " 設定終了
  call dein#end()
  call dein#save_state()
endif

" もし、未インストールものものがあったらインストール
if dein#check_install()
  call dein#install()
endif

filetype plugin indent on
syntax on

colorscheme pencil
" いずれ時間指定してみたい
set background=light
