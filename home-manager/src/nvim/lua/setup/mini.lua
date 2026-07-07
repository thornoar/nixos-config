return {
    "nvim-mini/mini.nvim",
    version = false,
    config = function ()
        require('mini.comment').setup({
            mappings = {
                -- Toggle comment (like `gcip` - comment inner paragraph) for both
                -- Normal and Visual modes
                comment = "\\",

                -- Toggle comment on current line
                comment_line = "\\\\",

                -- Toggle comment on visual selection
                comment_visual = "\\",

                -- Define 'comment' textobject (like `dgc` - delete whole comment block)
                -- Works also in Visual mode if mapping differs from `comment_visual`
                textobject = 'gc',
            },
        })
    end
}
