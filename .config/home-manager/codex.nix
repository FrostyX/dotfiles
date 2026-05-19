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
    (try-readwrite (noescape "\"$HOME/.codex\""))
    (try-readwrite (noescape "\"$HOME/.cache/codex\""))
    (try-readwrite (noescape "\"$HOME/.local/share/codex\""))
  ];
in
{
  home.packages = [
    (jail "codex-jailed" pkgs.codex combinators)
    (jail "codex-acp-jailed" acpPkgs.codex-acp combinators)
  ];

  home.file.".codex/AGENTS.md".text = ''
    # Codex Instructions

    ## Response Style

    - Keep answers concise by default.
    - Lead with the result or recommendation.
    - Avoid long explanations unless asked for details.
    - Prefer short paragraphs over long bullet lists.
    - Mention only the most relevant files, commands, and test results.
    - Do not repeat information already visible in the user request.

    ## Coding Work

    - Make the smallest change that solves the requested problem.
    - Preserve existing project style and naming.
    - Summarize changes briefly after editing.
    - Do not run tests, linters, formatters, or other verification commands unless explicitly requested.
    - Report verification commands only when they were explicitly requested and run.
  '';
}
