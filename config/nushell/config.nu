$env.config = {
  show_banner: false
  table: {
      mode: none
  }
  hooks: {
    pre_prompt: [{
        let direnv = (direnv export json | from json | default {})
        if ($direnv | is-empty) {
          return 
        }
        $direnv
        | items {|key, value|
          {
            key: $key
            value: (if $key in $env.ENV_CONVERSIONS {
              do ($env.ENV_CONVERSIONS | get $key | get from_string) $value
            } else {
              $value
            }) 
            } 
        } | transpose -ird | load-env
    }]
  }
  }

source ~/.cache/starship/init.nu
