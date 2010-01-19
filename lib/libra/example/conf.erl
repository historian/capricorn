
{load, "/Users/Simon/.libra/*.conf"}.

{watch, "apache", [
  {pid,     "/opt/local/apache2/logs/httpd.pid"},
  {start,   "/opt/local/apache2/bin/apachectl -k start", 5},
  {restart, "/opt/local/apache2/bin/apachectl -k restart", 10},
  {stop,    "/opt/local/apache2/bin/apachectl -k stop"}
]}.

{watch, "capricorn cluster", [
  {wcd,     "/usr/capricorn-cluster"},
  {pid,     "/usr/capricorn-cluster/var/run/capricorn/capricorn.pid"},
  {start,   "/usr/local/erlware/bin/capricornd -b", 5},
  {stop,    "/usr/local/erlware/bin/capricornd -d", 5}
]}.

{watch, "capricorn machine", [
  {wcd,     "/usr/capricorn-machine"},
  {pid,     "/usr/capricorn-machine/var/run/capricorn/capricorn.pid"},
  {start,   "/usr/local/erlware/bin/capricornd -b", 5},
  {stop,    "/usr/local/erlware/bin/capricornd -d", 5}
]}.
