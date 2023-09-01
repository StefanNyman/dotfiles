call plug#begin(stdpath('data') . '/plugged')

Plug 'neovim/nvim-lspconfig'
Plug 'morhetz/gruvbox'
Plug 'junegunn/fzf', {'do': { -> fzf#install() }}
Plug 'junegunn/fzf.vim'
Plug 'tpope/vim-fugitive'
Plug 'airblade/vim-gitgutter'
Plug 'tpope/vim-obsession'
"Plug 'easymotion/vim-easymotion'
Plug 'phaazon/hop.nvim'

Plug 'itchyny/lightline.vim'
Plug 'shinchu/lightline-gruvbox.vim'

Plug 'hrsh7th/nvim-cmp'
Plug 'hrsh7th/cmp-nvim-lsp'
Plug 'hrsh7th/cmp-nvim-lua'
Plug 'hrsh7th/cmp-nvim-lsp-signature-help'
Plug 'hrsh7th/cmp-vsnip'
Plug 'hrsh7th/cmp-path'
Plug 'hrsh7th/cmp-buffer'
Plug 'hrsh7th/cmp-cmdline'
Plug 'hrsh7th/cmp-calc'
Plug 'hrsh7th/vim-vsnip'

Plug 'f3fora/cmp-spell'
Plug 'tamago324/cmp-zsh'
Plug 'onsails/lspkind.nvim'
Plug 'L3MON4D3/LuaSnip'
Plug 'lukas-reineke/lsp-format.nvim'

Plug 'instant-markdown/vim-instant-markdown', {'for': 'markdown', 'do': 'yarn install'}

Plug 'simrat39/rust-tools.nvim'
Plug 'nvim-lua/plenary.nvim'
Plug 'mfussenegger/nvim-dap'

Plug 'williamboman/mason.nvim'
Plug 'williamboman/mason-lspconfig.nvim'

"Plug 'nvim-treesitter/nvim-treesitter'

Plug 'voldikss/vim-floaterm'
Plug 'nvim-telescope/telescope.nvim', { 'tag': '0.1.2' }
Plug 'preservim/tagbar'

Plug 'navarasu/onedark.nvim'

Plug 'danilamihailov/beacon.nvim'

Plug 'nvim-treesitter/nvim-treesitter'
Plug 'ray-x/go.nvim'

call plug#end()

"colorscheme gruvbox 
set termguicolors

lua << END
require("onedark").setup {
    style = "warmer"
}
require("onedark").load()
END

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

lua << END
require("mason").setup({
    ui = {
        icons = {
            package_installed = "",
            package_pending = "",
            package_uninstalled = "",
        },
    }
})
require("mason-lspconfig").setup()
END

nmap <leader>ft :FloatermNew --name=myfloat --height=0.8 --width=0.7 --autoclose=2 fish <CR>
nmap t :FloatermToggle myfloat<CR>
tnoremap <Esc> <C-\><C-n>:q<CR>

nnoremap <leader>ff <cmd>Telescope find_files<cr>
nnoremap <leader>fg <cmd>Telescope live_grep<cr>
nnoremap <leader>fb <cmd>Telescope buffers<cr>
nnoremap <leader>fh <cmd>Telescope help_tags<cr>

nmap <F8> :TagbarToggle<CR>

lua << END
local hop = require("hop")
hop.setup()
local directions = require('hop.hint').HintDirection
vim.keymap.set('', 'f', function()
  hop.hint_char1({ direction = directions.AFTER_CURSOR, current_line_only = false })
end, {remap=true})
vim.keymap.set('', 'F', function()
  hop.hint_char1({ direction = directions.BEFORE_CURSOR, current_line_only = false })
end, {remap=true})
END

source $HOME/.config/nvim/plug-config/cmp.lua
source $HOME/.config/nvim/plug-config/lsp.lua

lua << END
local format_sync_grp = vim.api.nvim_create_augroup("GoFormat", {})
vim.api.nvim_create_autocmd("BufWritePre", {
    pattern = "*.go",
    callback = function()
        require("go.format").goimport()
    end,
    group = format_sync_grp
})
require("go").setup()
END


"function GoFumpt()
"    let saved_view = winsaveview()
"    silent %!gofumpt
"    if v:shell_error > 0
"        cexpr getline(1, '$')->map({ idx, val -> val->substitute('<standard input>', expand('%'), '') })
"        silent undo
"        cwindow
"    else
"        cclose
"    endif
"    call winrestview(saved_view)
"endfunction
"
"command! GoFumpt call GoFumpt()
"
"augroup go_autocmd
"    autocmd BufWritePre *.go GoFumpt
"augroup END
"

filetype plugin on

"let g:instant_markdown_slow = 1
let g:instant_markdown_autostart = 0
"let g:instant_markdown_open_to_the_world = 1
"let g:instant_markdown_allow_unsafe_content = 1
"let g:instant_markdown_allow_external_content = 0
"let g:instant_markdown_mathjax = 1
"let g:instant_markdown_mermaid = 1
"let g:instant_markdown_logfile = '/tmp/instant_markdown.log'
"let g:instant_markdown_autoscroll = 0
"let g:instant_markdown_port = 8888
"let g:instant_markdown_python = 1
"
