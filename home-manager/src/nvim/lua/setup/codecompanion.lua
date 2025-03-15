require("codecompanion").setup({
    strategies = {
        chat = {
            adapter = "copilot",
            keymaps = {
                send = {
                    modes = { n = "<C-s>", i = "<C-p>" },
                },
                close = {
                    modes = { n = "<C-c>", i = "<C-p>" },
                },
            },
        },
        inline = {
            adapter = "copilot",
        },
    },
    display = {
        action_palette = {
            width = 95,
            height = 10,
            prompt = "Prompt: ", -- Prompt used for interactive LLM calls
            provider = "default", -- default|telescope|mini_pick
            opts = {
                show_default_actions = true, -- Show the default actions in the action palette?
                show_default_prompt_library = true, -- Show the default prompt library in the action palette?
            },
        },
        chat = {
            window = {
                layout = "vertical",
            },
        },
    },
})
vim.keymap.set("n", "<C-S-a>", "<cmd>CodeCompanionActions<CR>")
vim.keymap.set("n", "<C-S-s>", "<cmd>CodeCompanionChat<CR>")
