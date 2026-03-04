{ lib, hostname, ... }:

# For testing future changes before deploying them to my workstation
lib.mkIf (builtins.elem hostname [ "pop-os" "nova" ]) {
}
