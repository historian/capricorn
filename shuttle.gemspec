# -*- encoding: utf-8 -*-

Gem::Specification.new do |s|
  s.name = %q{shuttle}
  s.version = "0.1.04"

  s.required_rubygems_version = Gem::Requirement.new(">= 0") if s.respond_to? :required_rubygems_version=
  s.authors = ["Simon Menke"]
  s.date = %q{2009-05-31}
  s.default_executable = %q{shuttle}
  s.description = %q{Manage satellites}
  s.email = %q{simon.menke@gmail.com}
  s.executables = ["shuttle"]
  s.files = ["bin/shuttle", "lib/rubygems_plugin.rb", "lib/shuttle/actor/actions.rb", "lib/shuttle/actor.rb", "lib/shuttle/actors/apache_actor.rb", "lib/shuttle/actors/base_actor.rb", "lib/shuttle/actors/mysql_actor.rb", "lib/shuttle/actors/passenger_actor.rb", "lib/shuttle/actors/plesk_actor.rb", "lib/shuttle/actors/sqlite3_actor.rb", "lib/shuttle/app.rb", "lib/shuttle/app_runner.rb", "lib/shuttle/client/auth_token.rb", "lib/shuttle/client.rb", "lib/shuttle/rubygems_plugin.rb", "lib/shuttle/satellite/actions.rb", "lib/shuttle/satellite/dependency_loader.rb", "lib/shuttle/satellite/persistence.rb", "lib/shuttle/satellite/state.rb", "lib/shuttle/satellite.rb", "lib/shuttle/server/daemon.rb", "lib/shuttle/server/proxy.rb", "lib/shuttle/server/security.rb", "lib/shuttle/server.rb", "lib/shuttle/system/macros.rb", "lib/shuttle/system/options.rb", "lib/shuttle/system/process_user.rb", "lib/shuttle/system/satellites.rb", "lib/shuttle/system/shell.rb", "lib/shuttle/system.rb", "lib/shuttle/systems/centos_plesk_system.rb", "lib/shuttle/systems/macports_system.rb", "lib/shuttle/thor_extentions.rb", "lib/shuttle.rb", "app_generators/engine/engine_generator.rb", "app_generators/engine/templates/config/routes.rb", "app_generators/engine/templates/init.rb", "app_generators/engine/templates/lib/engine.rb", "app_generators/engine/templates/rails/init.rb"]
  s.homepage = %q{http://github.com/simonmenke/shuttle}
  s.require_paths = ["lib"]
  s.rubygems_version = %q{1.3.3}
  s.summary = %q{Manage satellites}

  if s.respond_to? :specification_version then
    current_version = Gem::Specification::CURRENT_SPECIFICATION_VERSION
    s.specification_version = 3

    if Gem::Version.new(Gem::RubyGemsVersion) >= Gem::Version.new('1.2.0') then
      s.add_runtime_dependency(%q<thor>, [">= 0.9.9"])
      s.add_runtime_dependency(%q<rubigen>, [">= 1.5.2"])
      s.add_runtime_dependency(%q<simple-daemon>, [">= 0.1.2"])
    else
      s.add_dependency(%q<thor>, [">= 0.9.9"])
      s.add_dependency(%q<rubigen>, [">= 1.5.2"])
      s.add_dependency(%q<simple-daemon>, [">= 0.1.2"])
    end
  else
    s.add_dependency(%q<thor>, [">= 0.9.9"])
    s.add_dependency(%q<rubigen>, [">= 1.5.2"])
    s.add_dependency(%q<simple-daemon>, [">= 0.1.2"])
  end
end
