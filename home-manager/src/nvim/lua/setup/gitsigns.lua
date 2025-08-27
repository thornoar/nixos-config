-- $gitsigns
require('gitsigns').setup({
    numhl = true,
    current_line_blame = false,
    current_line_blame_opts = {
        delay = 1000,
        ignore_whitespace = true,
    },
    max_file_length = 10000,
    on_attach = function(bufnr)
        local gitsigns = require('gitsigns')

        local function map (mode, l, r, opts)
            opts = opts or {}
            opts.buffer = bufnr
            vim.keymap.set(mode, l, r, opts)
        end

        map("n", "]c", function ()
            if vim.wo.diff then
                vim.cmd.normal({"]c", bang = true})
            else
                gitsigns.nav_hunk("next")
            end
        end)
        map("n", "[c", function ()
            if vim.wo.diff then
                vim.cmd.normal({"[c", bang = true})
            else
                gitsigns.nav_hunk("prev")
            end
        end)
        map("v", "<leader>gs", function() gitsigns.stage_hunk({ vim.fn.line("."), vim.fn.line("v") }) end)
        map("v", "<leader>gr", function() gitsigns.reset_hunk({ vim.fn.line("."), vim.fn.line("v") }) end)
        map("n", "<leader>gs", gitsigns.stage_hunk)
        map("n", "<leader>gr", gitsigns.reset_hunk)
        map("n", "<leader>gS", gitsigns.stage_buffer)
        map("n", "<leader>gR", gitsigns.reset_buffer)
        map("n", "<leader>gq", gitsigns.setqflist)
        map("n", "<M-s>", gitsigns.preview_hunk_inline)
        map("n", "<C-M-s>", gitsigns.diffthis)
        map({ "o", "x" }, "ih", ":<C-U>Gitsigns select_hunk<CR>")
    end,
})
