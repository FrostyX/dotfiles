{ pkgs, jail, acpPkgs, ... }:

let
  caBundle = "${pkgs.cacert}/etc/ssl/certs/ca-bundle.crt";
  commandPath = "/home/jkadlcik/.nix-profile/bin:/nix/var/nix/profiles/default/bin:/usr/local/sbin:/usr/local/bin:/usr/sbin:/usr/bin:/bin";
  combinators = c: with c; [
    network
    mount-cwd
    no-new-session
    time-zone
    (readonly caBundle)
    (set-env "SSL_CERT_FILE" caBundle)
    (set-env "NIX_SSL_CERT_FILE" caBundle)
    (set-env "NODE_EXTRA_CA_CERTS" caBundle)
    (set-env "PATH" commandPath)
    (try-readonly "/usr")
    (try-readonly "/lib")
    (try-readonly "/lib64")
    (try-readonly "/nix")
    (try-readwrite (noescape "\"$HOME/.claude\""))
    (try-readwrite (noescape "\"$HOME/.cache/claude\""))
    (try-readwrite (noescape "\"$HOME/.local/share/claude\""))
  ];
in
{
  home.packages = [
    (jail "claude-jailed" pkgs.claude-code combinators)
    (jail "claude-agent-acp-jailed" acpPkgs.claude-agent-acp combinators)
  ];

  home.file.".claude/settings.json".text = builtins.toJSON {
    permissions = {
      allow = [
        "Read(/nix/**)"
        "WebFetch(domain:raw.githubusercontent.com)"
        "Bash(sleep:*)"
        "Bash(copr-cli status:*)"
        "Bash(koji taskinfo:*)"
      ];
    };
  };
}
