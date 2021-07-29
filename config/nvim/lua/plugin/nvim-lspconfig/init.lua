local lspconfig = require('lspconfig')
local languages = require('plugin.nvim-lspconfig.format')
local on_attach = require('plugin.nvim-lspconfig.on-attach')

local installed = { 'clangd', 'pyright', 'sumneko_lua', 'hls', 'ocamllsp', 'zls', 'tsserver', 'rnix', 'racket_langserver', 'clojure_lsp'}

local servers = {
  -- TODO make proper use of diagnosticls or efm language server
  -- diagnosticls = {
  --   init_options = {documentFormatting = true, codeAction = true},
  --   root_dir = lspconfig.util.root_pattern({'.git/', '.'}),
  --   filetypes = vim.tbl_keys(languages),
  --   settings = {languages = languages, log_level = 1, log_file = '~/diagnosticls.log'},
  -- },
  sumneko_lua = {
    cmd = {"lua-language-server"},
    on_attach = on_attach,
    settings = {
      Lua = {
        runtime = {
          version = 'LuaJIT',
          path = vim.split(package.path, ';'),
        },
        diagnostics = {
          globals = {'vim'},
        },
        workspace = {
          library = {
            [vim.fn.expand('$VIMRUNTIME/lua')] = true,
            [vim.fn.expand('$VIMRUNTIME/lua/vim/lsp')] = true,
          },
        },
        telemetry = {
          enable = false,
        },
      },
    },
  },
  lua = {
    settings = {
      Lua = {
        diagnostics = {globals = {'vim', 'packer_plugins'}},
        completion = {keywordSnippet = 'Both'},
        runtime = {version = 'LuaJIT', path = vim.split(package.path, ';')},
        workspace = {library = vim.list_extend({[vim.fn.expand('$VIMRUNTIME/lua')] = true}, {})},
      },
    },
  },
}

local function setup_servers()
  for _, server in pairs(installed) do
    local config = servers[server] or {root_dir = lspconfig.util.root_pattern({'.git/', '.'})}
    config.on_attach = on_attach
    lspconfig[server].setup(config)
  end
end

-- TODO use eslint alongside tsserver?
-- lspconfig.diagnosticls.setup {
--   filetypes = {"javascript", "typescript"},
--   init_options = {
--     linters = {
--       eslint = {
--         command = "./node_modules/.bin/eslint",
--         rootPatterns = {".git"},
--         debounce = 100,
--         args = {
--           "--stdin",
--           "--stdin-filename",
--           "%filepath",
--           "--format",
--           "json"
--         },
--         sourceName = "eslint",
--         parseJson = {
--           errorsRoot = "[0].messages",
--           line = "line",
--           column = "column",
--           endLine = "endLine",
--           endColumn = "endColumn",
--           message = "${message} [${ruleId}]",
--           security = "severity"
--         },
--         securities = {
--           [2] = "error",
--           [1] = "warning"
--         }
--       },
--       filetypes = {
--         javascript = "eslint",
--         typescript = "eslint"
--       }
--     }
--   }
-- }

setup_servers()
