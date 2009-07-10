# -*- encoding: utf-8 -*-

Gem::Specification.new do |s|
  s.name = %q{capricorn}
  s.version = "0.2.20"

  s.required_rubygems_version = Gem::Requirement.new(">= 0") if s.respond_to? :required_rubygems_version=
  s.authors = ["Simon Menke"]
  s.date = %q{2009-07-10}
  s.default_executable = %q{capricorn}
  s.description = %q{Manage satellites}
  s.email = %q{simon.menke@gmail.com}
  s.executables = ["capricorn"]
  s.files = ["spec/actor/actions_spec.rb", "spec/spec_helper.rb", "bin/capricorn", "lib/capricorn/actor/actions.rb", "lib/capricorn/actor.rb", "lib/capricorn/actors/apache_actor.rb", "lib/capricorn/actors/base_actor.rb", "lib/capricorn/actors/host_file_actor.rb", "lib/capricorn/actors/mysql_actor.rb", "lib/capricorn/actors/passenger_actor.rb", "lib/capricorn/actors/plesk_actor.rb", "lib/capricorn/actors/sqlite3_actor.rb", "lib/capricorn/app_runner.rb", "lib/capricorn/apps/dev.rb", "lib/capricorn/apps/engines.rb", "lib/capricorn/apps/jobs.rb", "lib/capricorn/apps/satellite.rb", "lib/capricorn/apps/server.rb", "lib/capricorn/client/auth_token.rb", "lib/capricorn/client.rb", "lib/capricorn/daemon.rb", "lib/capricorn/exception_handler.rb", "lib/capricorn/extentions/rubygems_plugin.rb", "lib/capricorn/extentions/thor_extentions.rb", "lib/capricorn/job_queue.rb", "lib/capricorn/satellite/actions.rb", "lib/capricorn/satellite/dependency_loader.rb", "lib/capricorn/satellite/persistence.rb", "lib/capricorn/satellite.rb", "lib/capricorn/server/daemon.rb", "lib/capricorn/server/proxy.rb", "lib/capricorn/server/security.rb", "lib/capricorn/server.rb", "lib/capricorn/system/config.rb", "lib/capricorn/system/helper.rb", "lib/capricorn/system/options.rb", "lib/capricorn/system/process_user.rb", "lib/capricorn/system/satellites.rb", "lib/capricorn/system/shell.rb", "lib/capricorn/system.rb", "lib/capricorn.rb", "lib/rubygems_plugin.rb", "app_generators/engine/engine_generator.rb", "app_generators/engine/templates/config/initializers/rails_init.rb", "app_generators/engine/templates/config/routes.rb", "app_generators/engine/templates/gitignore", "app_generators/engine/templates/Gmfile", "app_generators/engine/templates/init.rb", "app_generators/engine/templates/lib/engine.rb", "app_generators/engine/templates/MIT-LICENSE.txt", "app_generators/engine/templates/rails/init.rb", "app_generators/engine/templates/README.rdoc", "app_generators/engine/templates/tasks/engine_tasks.rake"]
  s.homepage = %q{http://github.com/simonmenke/capricorn}
  s.require_paths = ["lib"]
  s.rubygems_version = %q{1.3.3}
  s.summary = %q{Manage satellites}
  s.test_files = ["spec/actor/actions_spec.rb", "spec/spec_helper.rb"]

  if s.respond_to? :specification_version then
    current_version = Gem::Specification::CURRENT_SPECIFICATION_VERSION
    s.specification_version = 3

    if Gem::Version.new(Gem::RubyGemsVersion) >= Gem::Version.new('1.2.0') then
    else
    end
  else
  end
end
