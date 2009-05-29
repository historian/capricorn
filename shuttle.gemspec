# -*- encoding: utf-8 -*-

Gem::Specification.new do |s|
  s.name = %q{shuttle}
  s.version = "0.1.03"

  s.required_rubygems_version = Gem::Requirement.new(">= 0") if s.respond_to? :required_rubygems_version=
  s.authors = ["Simon Menke"]
  s.date = %q{2009-05-29}
  s.default_executable = %q{shuttle}
  s.description = %q{Manage satellites}
  s.email = %q{simon.menke@gmail.com}
  s.executables = ["shuttle"]
  s.files = ["bin/shuttle", "lib/shuttle/adapter.rb", "lib/shuttle/adapters/apache_adapter.rb", "lib/shuttle/adapters/base_adapter.rb", "lib/shuttle/adapters/mysql_adapter.rb", "lib/shuttle/adapters/passenger_adapter.rb", "lib/shuttle/adapters/plesk_adapter.rb", "lib/shuttle/adapters/sqlite3_adapter.rb", "lib/shuttle/app.rb", "lib/shuttle/app_runner.rb", "lib/shuttle/client.rb", "lib/shuttle/config.rb", "lib/shuttle/satellite.rb", "lib/shuttle/server.rb", "lib/shuttle/system/macros.rb", "lib/shuttle/system/options.rb", "lib/shuttle/system/process_user.rb", "lib/shuttle/system/shell.rb", "lib/shuttle/system.rb", "lib/shuttle/system_profile.rb", "lib/shuttle/system_profiles/macosx_profile.rb", "lib/shuttle/system_profiles/macports_profile.rb", "lib/shuttle/systems/centos_plesk_system.rb", "lib/shuttle/systems/macports_system.rb", "lib/shuttle/thor_extentions.rb", "lib/shuttle.rb"]
  s.homepage = %q{http://github.com/simonmenke/shuttle}
  s.require_paths = ["lib"]
  s.rubygems_version = %q{1.3.3}
  s.summary = %q{Manage satellites}

  if s.respond_to? :specification_version then
    current_version = Gem::Specification::CURRENT_SPECIFICATION_VERSION
    s.specification_version = 3

    if Gem::Version.new(Gem::RubyGemsVersion) >= Gem::Version.new('1.2.0') then
      s.add_runtime_dependency(%q<thor>, [">= 0.9.9"])
      s.add_runtime_dependency(%q<simple-daemon>, [">= 0.1.2"])
    else
      s.add_dependency(%q<thor>, [">= 0.9.9"])
      s.add_dependency(%q<simple-daemon>, [">= 0.1.2"])
    end
  else
    s.add_dependency(%q<thor>, [">= 0.9.9"])
    s.add_dependency(%q<simple-daemon>, [">= 0.1.2"])
  end
end
