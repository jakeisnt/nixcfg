{ options, config, lib, pkgs, ... }:

with lib;
with lib.my;
let cfg = config.modules.hardware.remarkable;
in {
  options.modules.hardware.remarkable = { enable = mkBoolOpt false; };
  config = mkIf cfg.enable {
    user.packages = with pkgs; [ rmapi ];


    programs.ssh.knownHosts = {
      # alias for ReMarkable IP!
      "remarkable" = {
        hostNames = ["10.110.139.108"];
        publicKey = "ssh-rsa AAAAB3NzaC1yc2EAAAADAQABAAABAQCY05XsAsq0wgBJvVQcT9MOh+U/rTlk0s7MHDZkFEwQiGuiGme3+96D8IrNGbtYwwWKDLZafxy9aAGLrTiT/UMTRC/7rO1aRRRCnpBbLfLiKX0JqWWL0ZzHdNNI3tPgIWUNw502GzcELbHjvw2SSgxvUF/Prqv6/CUrEN3tWG2ch/jjmXGlLkOcev/hHp6HR3J8OcSTTVlc9MZhYkrrfifi3nCtNOtbWWQfEx1KnAQ2L4t4LLfeATBqzVkNsmk3p8IZ97tZJdWFajEb44jROBpvei0JG+VTSW6CcolwluYx4vkk2mVOt+e520T7gjNQQnFjxeXJOy/5FxJfq9OFn7/T root@reMarkable";
      };
    };

  };
}
