local status, nvim_lsp = pcall(require, "lspconfig")
if (not status) then return end

-- local capabilities = require('cmp_nvim_lsp').default_capabilities()
local lsp_defaults = nvim_lsp.util.default_config

lsp_defaults.capabilities = vim.tbl_deep_extend("force", lsp_defaults.capabilities,
    require("cmp_nvim_lsp").default_capabilities())

local on_attach = function(client)
    vim.buf_set_option('omnifunc', 'v:lua.vim.lsp.omnifunc')
end

vim.cmd [[autocmd BufWritePre * lua vim.lsp.buf.format()]]

vim.keymap.set('n', '<space>e', vim.diagnostic.open_float)
vim.keymap.set('n', '[d', vim.diagnostic.goto_prev)
vim.keymap.set('n', ']d', vim.diagnostic.goto_next)
vim.keymap.set('n', '<space>q', vim.diagnostic.setloclist)

vim.api.nvim_create_autocmd('LspAttach', {
    group = vim.api.nvim_create_augroup('UserLspConfig', {}),
    callback = function(ev)
        -- Buffer local mappings.
        -- See `:help vim.lsp.*` for documentation on any of the below functions
        local opts = { buffer = ev.buf }
        vim.keymap.set('n', 'gD', vim.lsp.buf.declaration, opts)
        vim.keymap.set('n', 'gd', vim.lsp.buf.definition, opts)
        vim.keymap.set('n', 'K', vim.lsp.buf.hover, opts)
        vim.keymap.set('n', 'gi', vim.lsp.buf.implementation, opts)
        vim.keymap.set('n', '<C-k>', vim.lsp.buf.signature_help, opts)
        vim.keymap.set('n', '<space>wa', vim.lsp.buf.add_workspace_folder, opts)
        vim.keymap.set('n', '<space>wr', vim.lsp.buf.remove_workspace_folder, opts)
        vim.keymap.set('n', '<space>wl', function()
            print(vim.inspect(vim.lsp.buf.list_workspace_folders()))
        end, opts)
        vim.keymap.set('n', '<space>D', vim.lsp.buf.type_definition, opts)
        vim.keymap.set('n', '<space>rn', vim.lsp.buf.rename, opts)
        vim.keymap.set({ 'n', 'v' }, '<space>ca', vim.lsp.buf.code_action, opts)
        vim.keymap.set('n', 'gr', vim.lsp.buf.references, opts)
        vim.keymap.set('n', '<space>f', function()
            vim.lsp.buf.format { async = true }
        end, opts)
    end,
})

nvim_lsp.lua_ls.setup {
    on_attach = on_attach,
    settings = {
        Lua = {
            runtime = {
                version = 'LuaJIT',
            },
            diagnostics = {
                globals = { 'vim' },
            },
            workspace = {
                library = vim.api.nvim_get_runtime_file("", true),
                checkThirdParty = false,
            },
            telemetry = {
                enable = false,
            },
        },
    },
}

nvim_lsp.pyright.setup {
    on_attach = on_attach,
}
nvim_lsp.gopls.setup {
    on_attach = on_attach,
    settings = {
        gopls = {
            gofumpt = true,
        },
    },
}
nvim_lsp.asm_lsp.setup {
    on_attach = on_attach,
}
nvim_lsp.awk_ls.setup {
    on_attach = on_attach,
}
nvim_lsp.bashls.setup {
    on_attach = on_attach,
}
nvim_lsp.ccls.setup {
    on_attach = on_attach,
    init_options = {
        compilationDatabaseDirectory = "build",
        index = {
            threads = 10,
        },
        clang = {
            excludeArgs = { "-frounding-math" },
        },
    },
    --    single_file_support = true;
}
nvim_lsp.clojure_lsp.setup {
    on_attach = on_attach,
}
nvim_lsp.cmake.setup {
    on_attach = on_attach,
}
nvim_lsp.dartls.setup {
    on_attach = on_attach,
}
nvim_lsp.dockerls.setup {
    on_attach = on_attach,
}
nvim_lsp.elixirls.setup {
    on_attach = on_attach,
}
nvim_lsp.elmls.setup {
    on_attach = on_attach,
}
nvim_lsp.erlangls.setup {
    on_attach = on_attach,
}
nvim_lsp.jsonnet_ls.setup {
    on_attach = on_attach,
}
nvim_lsp.nim_langserver.setup {
    on_attach = on_attach,
}
nvim_lsp.texlab.setup {
    on_attach = on_attach,
}
nvim_lsp.tsserver.setup {
    on_attach = on_attach,
}
nvim_lsp.terraformls.setup {
    on_attach = on_attach,
}
nvim_lsp.zls.setup {
    on_attach = on_attach,
}

local rt = require("rust-tools")

rt.setup({
    server = {
        on_attach = function(_, bufnr)
            vim.keymap.set("n", "<C-space>", rt.hover_actions.hover_actions, { buffer = bufnr })
            vim.keymap.set("n", "<Leader>a", rt.code_action_group.code_action_group, { buffer = bufnr })
        end
    }
})
