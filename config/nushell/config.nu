$env.config = {
  show_banner: false
  table: {
    mode: none
  }
  hooks: {
    pre_prompt: [{
      code: "
        let direnv = (direnv export json | from json)
        let direnv = if ($direnv | length) == 1 { $direnv } else { {} }
        $direnv | load-env
      "
    }]
  }
}

source ~/.cache/starship/init.nu
