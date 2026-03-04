return {
    "nomnivore/ollama.nvim",
    dependencies = {
        "nvim-lua/plenary.nvim",
    },
    cmd = { "Ollama", "OllamaModel" },
    keys = { },
    opts = {
        model = "qwen3-coder-next:cloud",
    },
}
