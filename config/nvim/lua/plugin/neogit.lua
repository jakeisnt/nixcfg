require'neogit'.setup {
    disable_commit_confirmation = true,
    integrations = {
        diffview = true
    },

    mappings = {
        status = {
            ["<CR>"] = "DiffviewOpen"
        }
    }
}
