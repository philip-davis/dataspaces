%define ver 0.9

Name: libsiw
Version: 0.9
Release: 1%{?dist}
Summary: Software iWARP user Lib

Group: System Environment/Libraries
License: GPL/BSD
Url: http://www.openfabrics.org/
Source: http://www.openfabrics.org/downloads/siw/%{name}-%{ver}.tar.gz
BuildRoot: %{_tmppath}/%{name}-%{version}-%{release}-root-%(%{__id_u} -n)

BuildRequires: libibverbs-devel

%description
libsiw provides a userspace driver for Linux Kernel SoftiWARP stack
for use with the libibverbs library.

%package devel
Summary: Development files for the libsiw driver
Group: System Environment/Libraries
Requires: %{name} = %{version}-%{release}

%description devel
Static version of libsiw that may be linked directly to an
application, which may be useful for debugging.

%prep
%setup -q -n %{name}-%{ver}

%build
%configure
make %{?_smp_mflags}

%install
rm -rf $RPM_BUILD_ROOT
%makeinstall
# remove unpackaged files from the buildroot
rm -f $RPM_BUILD_ROOT%{_libdir}/*.la

%clean
rm -rf $RPM_BUILD_ROOT

%files
%defattr(-,root,root,-)
%{_libdir}/libsiw*.so
%doc AUTHORS COPYING ChangeLog README
%config %{_sysconfdir}/libibverbs.d/libsiw.driver

%files devel
%defattr(-,root,root,-)
%{_libdir}/libsiw*.a

%changelog
