require("codecompanion").setup({
    adapters = {
        codeqwen = function()
            return require("codecompanion.adapters").extend("ollama", {
                name = "codeqwen", -- Give this adapter a different name to differentiate it from the default ollama adapter
                schema = {
                    model = {
                        default = "codeqwen:7b",
                    },
                    num_ctx = {
                        default = 16384,
                    },
                    num_predict = {
                        default = -1,
                    },
                },
            })
        end,
    },
    strategies = {
        chat = {
            adapter = "codeqwen",
        },
        inline = {
            adapter = "codeqwen",
        },
    },
    display = {
        action_palette = {
            width = 95,
            height = 10,
            prompt = "Prompt: ", -- Prompt used for interactive LLM calls
            provider = "telescope", -- default|telescope|mini_pick
            opts = {
                show_default_actions = true, -- Show the default actions in the action palette?
                show_default_prompt_library = true, -- Show the default prompt library in the action palette?
            },
        },
    },
})
