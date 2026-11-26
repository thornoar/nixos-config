return {
    "kaarmu/typst.vim",
    ft = "typst",
    lazy = false,
    config = function ()
        vim.g.typst_syntax_highlight = true
        vim.g.typst_conceal = true
        vim.g.typst_conceal_math = false
    end
}
