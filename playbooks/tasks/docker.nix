[
  # https://developer.fedoraproject.org/tools/docker/docker-installation.html
  {
    name = "Install Docker packages";
    dnf = {
      state = "latest";
      name = "{{ item }}";
    };
    with_items = [
      "docker"
      "docker-compose"
    ];
  }
  {
    name = "Run Docker services";
    systemd = {
      state = "started";
      name = "{{ item }}";
      enabled = true;
    };
    with_items = [
      "docker"
    ];
  }
  {
    name = "Docker as user - create group";
    group = {
      name = "docker";
      state = "present";
    };
  }
  {
    name = "Docker as user - add user to group";
    user = {
      name = "{{ user }}";
      groups = "docker";
      append = true;
    };
    # Conditionaly restart docker here and run `newgrp docker`
  }
]
