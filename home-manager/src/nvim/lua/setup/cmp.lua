return {
    "hrsh7th/nvim-cmp",
    dependencies = {
        "quangnguyen30192/cmp-nvim-ultisnips",
        "hrsh7th/cmp-buffer",
        "hrsh7th/cmp-path",
        "octaltree/cmp-look",
    },
    config = function ()
        local cmp = require("cmp")
        cmp.setup({
            sources = {
                { name = "nvim_lsp" },
                { name = "path" },
                { name = "ultisnips" },
                {
                    name = "look",
                    keyword_length = 2,
                    option = {
                        convert_case = true,
                        loud = true,
                        dict = os.getenv("WORDLIST")
                    }
                },
            },
            window = {
                completion = cmp.config.window.bordered(),
                documentation = cmp.config.window.bordered(),
            },
            mapping = cmp.mapping.preset.insert({
                ["<Down>"] = cmp.mapping(function (fallback)
                    cmp.close()
                    fallback()
                end, { "i" }),
                ["<Up>"] = cmp.mapping(function (fallback)
                    cmp.close()
                    fallback()
                end, { "i" }),
                ["<C-x>"] = cmp.mapping.select_next_item({}),
                ["<C-Down>"] = cmp.mapping.scroll_docs(1),
                ["<C-a>"] = cmp.mapping.select_prev_item({}),
                ["<C-Up>"] = cmp.mapping.scroll_docs(-1),
                ["<CR>"] = cmp.mapping.confirm({ select = false }),
                ["<C-Space>"] = cmp.mapping.complete(),
                ["<C-CR>"] = cmp.mapping(function(fallback)
                    vim.fn["UltiSnips#ExpandSnippetOrJump"]()
                end, { "i", "s" }),
            }),
            snippet = {
                expand = function(args)
                    -- luasnip.lsp_expand(args.body)
                    vim.fn["UltiSnips#Anon"](args.body)
                end,
            },
        })
    end
}
