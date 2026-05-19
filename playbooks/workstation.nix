{ ... }:

let
  dockerTasks = import ./tasks/docker.nix;
in
{
  collections = {
    ansible-posix = {
      version = "2.1.0";
      hash = "sha256-X/Iyz2lJKpkfKgmQ27I+8lpgC36SCd5uMzd6b/CKiik=";
    };
  };

  playbook = [
    {
      name = "Workstation system setup";
      # gather_facts = false;
      hosts = "localhost";
      become = true;

      vars.user = "jkadlcik";

      # roles = [
      #   { role = "syncthing"; }
      #
      #   My workstation has a beefy CPU (AMD Ryzen 9 7900X) and although massive,
      #   the CPU fan (Noctua NH-U12A chromax.black) doesn't keep up. We need to
      #   limit its power to keep the noise reasonable
      #   Actually no, this causes a noticeable performance decrease (even though
      #   it successfully decreases the noise). It is better to undervolt the CPU
      #   in BIOS instead
      #   {
      #     role = "amdryzen";
      #     when = ''ansible_facts["hostname"] == "hive"'';
      #   }
      # ];

      tasks =
        [
        {
          name = "Install workstation repos";
          dnf = {
            state = "latest";
            name = "fedora-workstation-repositories";
          };
        }
        {
          name = "Enable workstation repos";
          command = "dnf config-manager setopt google-chrome.enabled=1";
        }
        {
          name = "Enable rpmfusion repos";
          dnf = {
            state = "latest";
            name = "{{ item }}";
            disable_gpg_check = true;
          };
          with_items = [
            ''https://download1.rpmfusion.org/free/fedora/rpmfusion-free-release-{{ ansible_facts["distribution_version"] }}.noarch.rpm''
            ''https://download1.rpmfusion.org/nonfree/fedora/rpmfusion-nonfree-release-{{ ansible_facts["distribution_version"] }}.noarch.rpm''
          ];
        }
        {
          name = "Enable Copr repos";
          command = "dnf -y copr enable {{ item }}";
          with_items = [
            "frostyx/qtile"
            "frostyx/rofi-themes-base16"
            "frostyx/rofi-themes-catppuccin"
            "vishalvvr/fontawesome-fonts"
            "korkeala/clojure"
          ];
        }
        {
          name = "Install packages";
          dnf = {
            state = "latest";
            name = [
              "@base-x"
              "vim"
              "vim-X11"
              "google-chrome-stable"
              "vlc"
              "light"
              "hexchat"
              "make"
              "cmake"
              "libtool"
              "dnf5-plugin-automatic"
              "openssh-clients"
              "krb5-workstation"
              "qtile"
              "qtile-wayland"
              "qtile-extras"
              "feh"
              "xset"
              "i3lock"
              "xrandr"
              "xsetroot"
              "xinput"
              "blueman"
              "pass"
              "htop"
              "xprop"
              "pulseaudio-utils"
              "pavucontrol"
              "network-manager-applet"
              "NetworkManager-tui"
              "NetworkManager-openvpn-gnome"
              "gnome-keyring"
              "fontawesome-fonts"
              "fontawesome-free-fonts"
              "bitstream-vera-fonts-all"
              "lm_sensors"
              "acpi"
              "stow"
              "rxvt-unicode"
              "rofi"
              "rofi-themes-base16"
              "rofi-themes-catppuccin"
              "ulauncher"
              "alacritty"
              "fzf"
              "ripgrep"
              "fedpkg"
              "emacs"

              # We need this for `pass` command, see what `pinentry-program` is configured
              # in ~/.gnupg/gpg-agent.conf
              "pinentry-gnome3"

              # Python
              "poetry"
              "pytest"
              "rpmdevtools"
              "mock"
              "tito"
              "copr-cli"

              # Dependencies for my Emacs setup
              "vdirsyncer"
              "khal"
              "npm"
              "discount"
              "leiningen"
              "emacs-rpm-spec-mode"
              "plantuml"
              "python3-lsp-server+all"
              "rpm-spec-language-server"
              "gopls"
              "ruff"
              "udiskie"
              "libvterm"
              "w3m"
              "isync"
              "maildir-utils"
              "postgresql"
              "python3-virtualenv"
            ];
          };
        }
      ]
      ++ dockerTasks
      ++ [
        {
          name = "Enable OpenRazer repository";
          command = ''dnf -y config-manager addrepo --overwrite --from-repofile https://download.opensuse.org/repositories/hardware:/razer/Fedora_{{ ansible_facts["distribution_major_version"] }}/hardware:razer.repo'';
        }
        {
          name = "Install polychromatic (an alternative to Razer Synapse)";
          dnf = {
            state = "latest";
            name = "polychromatic";
          };
        }
        {
          name = "Add user to the plugdev group";
          user = {
            name = "jkadlcik";
            groups = "plugdev";
            append = true;
          };
        }

        # https://docs.fedoraproject.org/en-US/quick-docs/virtualization-getting-started/
        {
          name = "Install virtualization software (libvirt, virt-manager, etc)";
          dnf = {
            state = "latest";
            name = "@virtualization";
          };
        }
        {
          name = "Run libvirtd";
          systemd = {
            state = "started";
            name = "libvirtd";
            enabled = true;
          };
        }
        {
          name = "Add user to the libvirt group";
          user = {
            name = "jkadlcik";
            groups = "libvirt";
            append = true;
          };
        }
        # {
        #   name = "Install nix";
        #   dnf = {
        #     state = "latest";
        #     name = "nix";
        #   };
        # }
        # {
        #   name = "nix-daemon";
        #   systemd = {
        #     state = "started";
        #     name = "nix-daemon";
        #     enabled = true;
        #   };
        # }
        {
          name = "Create logind config directory";
          file = {
            path = "/etc/systemd/logind.conf.d/";
            state = "directory";
          };
        }
        {
          name = "Install logind config";
          copy = {
            src = "{{ git_root }}/playbooks/files/logind.conf";
            dest = "/etc/systemd/logind.conf.d/";
          };
        }
        {
          "ansible.posix.sysctl" = {
            name = "vm.swappiness";
            value = "5";
            state = "present";
          };
        }
      ];
    }

    {
      name = "Workstation user setup";
      hosts = "localhost";

      tasks = [
        {
          name = "Create directory structure";
          file = {
            path = "/home/jkadlcik/{{ item }}";
            state = "directory";
          };
          with_items = [
            "git"
          ];
        }
        # TODO Use stow to install the dotfiles
        # {
        #   name = "Use config from dotfiles repo";
        #   file = {
        #     src = "/home/jkadlcik/git/dotfiles/{{ item }}";
        #     dest = "/home/jkadlcik/{{ item }}";
        #     state = "link";
        #   };
        #   with_items = [
        #     ".gitconfig"
        #     ".vim"
        #     ".vimrc"
        #     ".local/share/applications/gvim.desktop"
        #     ".config/qtile"
        #     ".config/vlc/vlcrc"
        #   ];
        # }
      ];
    }
  ];
}
