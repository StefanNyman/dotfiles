local status, cmp = pcall(require, "cmp")
if (not status) then return end

local lspkind = require 'lspkind'

cmp.setup({
    snippet = {
        expand = function(args)
            require('luasnip').lsp_expand(args.body)
        end,
    },
    mapping = cmp.mapping.preset.insert({
        ['<C-p>'] = cmp.mapping.select_prev_item(),
        ['<C-n>'] = cmp.mapping.select_next_item(),
        --['<C-d>'] = cmp.mapping.scroll_docs(-4),
        --['<C-f>'] = cmp.mapping.scroll_docs(4),
        ['<C-Space>'] = cmp.mapping.complete(),
        ['<C-e>'] = cmp.mapping.close(),
        ['<CR>'] = cmp.mapping.confirm({
            behavior = cmp.ConfirmBehavior.Replace,
            select = true,
        }),
    }),
    sources = cmp.config.sources({
        { name = 'path' },
        { name = 'nvim_lsp',               keyword_length = 2 },
        { name = 'nvim_lsp_signature_help' },
        { name = 'nvim_lua',               keyword_length = 2 },
        { name = 'buffer',                 keyword_length = 2 },
        { name = 'vsnip',                  keyword_length = 2 },
        { name = 'calc' }
    }),
    window = {
        completion = cmp.config.window.bordered(),
        documentation = cmp.config.window.bordered(),
    },
    formatting = {
        fields = { 'menu', 'abbr', 'kind' },
        format = lspkind.cmp_format({
            mode = 'symbol_text',
            maxwidth = 50,
            ellipsis_char = '...',
            before = function(entry, vim_item)
                vim_item.menu = ({
                    nvim_lsp = "[LSP]",
                    spell = "[Spellings]",
                    zsh = "[Zsh]",
                    buffer = "[Buffer]",
                    ultisnips = "[Snip]",
                    treesitter = "[Treesitter]",
                    calc = "[Calculator]",
                    nvim_lua = "[Lua]",
                    path = "[Path]",
                    nvim_lsp_signature_help = "[Signature]",
                    cmdline = "[Vim Command]"
                })[entry.source.name]
                return vim_item
            end,
        }),
    },
})

cmp.setup.cmdline(':', {
    sources = cmp.config.sources({
        { name = 'path' },
        { name = 'cmdline' },
    })
})

cmp.setup.cmdline('/', {
    sources = {
        { name = 'buffer' },
    }
})

vim.cmd [[
  set completeopt=menuone,noinsert,noselect
  highlight! default link CmpItemKind CmpItemMenuDefault
]]
