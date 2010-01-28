Summary: Capricorn a rails cluster
Name: Capricorn
Version: 0.2
Release: 1
Source: capricorn-%{version}.tar.gz
Group: System Environment/Shells
Copyright: MIT-type
Prefix: /usr/capricorn
BuildRoot: %{_tmppath}/%{name}-root

%description
Capricorn is a deployment and management cluster for running allot of rails applications across many servers.

%prep
%setup -q

%build
make build

%install
rm -rf $RPM_BUILD_ROOT
mkdir -p $RPM_BUILD_ROOT/usr/capricorn
make install prefix=$RPM_BUILD_ROOT/usr/capricorn sysconfdir=$RPM_BUILD_ROOT/etc

%clean
rm -rf $RPM_BUILD_ROOT

%files
%defattr(-,bin,bin)
/usr/capricorn/bin/capricornd
/usr/capricorn/bin/capricornctl