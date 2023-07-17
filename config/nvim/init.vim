call plug#begin(stdpath('data') . '/plugged')

Plug 'neovim/nvim-lspconfig'
Plug 'morhetz/gruvbox'
Plug 'junegunn/fzf', {'do': { -> fzf#install() }}
Plug 'junegunn/fzf.vim'
Plug 'tpope/vim-fugitive'
Plug 'tpope/vim-obsession'
Plug 'easymotion/vim-easymotion'

Plug 'itchyny/lightline.vim'
Plug 'shinchu/lightline-gruvbox.vim'

Plug 'hrsh7th/nvim-cmp'
Plug 'hrsh7th/cmp-nvim-lsp'
Plug 'hrsh7th/cmp-cmdline'
Plug 'hrsh7th/cmp-path'
Plug 'hrsh7th/cmp-buffer'
Plug 'hrsh7th/cmp-calc'
Plug 'f3fora/cmp-spell'
Plug 'tamago324/cmp-zsh'
Plug 'hrsh7th/cmp-nvim-lsp-signature-help'
Plug 'onsails/lspkind.nvim'
Plug 'L3MON4D3/LuaSnip'
Plug 'lukas-reineke/lsp-format.nvim'

call plug#end()

colorscheme gruvbox 
set termguicolors

nmap \ :GFiles<CR>
nmap ; :Files<CR>

let mapleader=','
set clipboard+=unnamedplus

set tabstop=4
set shiftwidth=4
set expandtab

syntax on

set nobackup
set nowritebackup
set updatetime=300
set noswapfile

nmap <leader>cc :clo<CR>
nmap <leader>vs :vsplit<CR>
nmap <leader>ss :split<CR>

set completeopt-=preview

let g:lightline = {
            \ 'colorscheme': 'gruvbox',
            \ 'active': {
            \   'left': [ ['mode', 'paste'], ['gitbranch', 'readonly', 'filename', 'modified'] ]
            \},
            \ 'component_function': {
            \   'gitbranch': 'FugitiveHead'
            \}
            \}

source $HOME/.config/nvim/plug-config/cmp.lua
source $HOME/.config/nvim/plug-config/lsp.lua

function GoFumpt()
    let saved_view = winsaveview()
    silent %!gofumpt
    if v:shell_error > 0
        cexpr getline(1, '$')->map({ idx, val -> val->substitute('<standard input>', expand('%'), '') })
        silent undo
        cwindow
    else
        cclose
    endif
    call winrestview(saved_view)
endfunction

command! GoFumpt call GoFumpt()

augroup go_autocmd
    autocmd BufWritePre *.go GoFumpt
augroup END



