
module Shuttle
  module Actors # :nodoc:
    class BaseActor < Shuttle::Actor
      
      on_install_satellite   :create_rails_app
      on_link_satellite      :link_engines
      on_uninstall_satellite :destroy_rails_app
      
      def create_rails_app
        system.as_user(system.web_user, system.web_group) do
          FileUtils.mkdir_p(File.dirname(system.satellite_root), :verbose => true)
          FileUtils.mkdir_p(system.shared_root, :verbose => true)
        end
        
        Dir.chdir(File.dirname(system.satellite_root)) do
          system.user_run system.web_user, "rails --force #{File.basename(system.satellite_root)}"
        end
        
        system.as_user(system.web_user, system.web_group) do
          Dir.chdir(File.dirname(system.satellite_root)) do
            link(File.join(system.shared_root,    'public'),
                 File.join(system.satellite_root, 'public', 'system'))
            link(File.join(system.shared_root,    'pivate'),
                 File.join(system.satellite_root, 'db', 'system'))
            link(File.join(system.shared_root,    'log'),
                 File.join(system.satellite_root, 'log'))
            link(File.join(system.shared_root,    'settings'),
                 File.join(system.satellite_root, 'config', 'settings'))
          end
          
        end
      end
      
      def destroy_rails_app
        FileUtils.rm_rf system.satellite_root, :verbose => true
      end
      
      def link_engines
        Dir.chdir(system.satellite_root) do
          system.as_user(system.web_user, system.web_group) do
            
            write_environment
            clean_links
            @dependecies.reverse_each do |spec|
              link_engine(spec)
            end
            run_migrations
            
          end
        end
      end
      
    private
      
      def link(src, dst)
        FileUtils.mkdir_p(File.dirname(dst), :verbose => true)
        FileUtils.mkdir_p(src, :verbose => true)
        FileUtils.rm_rf dst, :verbose => true
        FileUtils.symlink(src, dst, :verbose => true)
      end
      
      def write_environment
        @dependecies = Shuttle::Satellite::DependencyLoader.load_for(satellite.engines)
        @dependecies.engines
        
        rails_header = "Rails::Initializer.run do |config|\n"
        gems_header  = "  # Gems added by engine_manager:\n  \n"
        
        gsub_file('config/environment.rb') do |content|
          content.gsub! /^\s*config.gem.+\n/, ''
          content.gsub! %r{#{Regexp.escape(rails_header)}(#{Regexp.escape(gems_header)})?},
            "#{rails_header}#{gems_header}"
          
          @dependecies.reverse_each do |spec|
            gem_options = @dependecies.engines[spec.name] || {}
            content.gsub! "engine_manager:\n",
              "engine_manager:\n  config.gem #{spec.name.inspect}, #{gem_options.inspect}\n"
          end
        end
      end
      
      def link_engine(spec)
        Shuttle.log "linking: #{spec.name}..."
        path = File.join(spec.full_gem_path, 'public')
        if File.directory?(path)
          FileUtils.mkdir_p('public/vendor', :verbose => true)
          FileUtils.ln_s(path, "public/vendor/#{spec.name}", :verbose => true)
        end
        
        path = File.join(spec.full_gem_path, 'tasks')
        if File.directory?(path)
          FileUtils.mkdir_p("lib/tasks/vendor/#{spec.name}", :verbose => true)
          Dir.glob("#{path}/*.rake").each do |rake_file|
            FileUtils.ln_s(rake_file,
              "lib/tasks/vendor/#{spec.name}/#{File.basename(rake_file)}", :verbose => true)
          end
        end
        
        path = File.join(spec.full_gem_path, 'db', 'migrate')
        if File.directory?(path)
          FileUtils.mkdir_p("db/migrate", :verbose => true)
          unlinked_migrations.concat(Dir.glob("#{path}/*.rb"))
          linked_migrations.each do |migration, target|
            if target.starts_with? spec.full_gem_path
              unlinked_migrations.delete(target)
              unused_migrations.delete(migration)
            end
          end
        end
      end
      
      def clean_links
        FileUtils.rm_rf("lib/tasks/vendor", :verbose => true)
        FileUtils.rm_rf("public/vendor", :verbose => true)
      end
      
      def run_migrations
        Shuttle.log "running your migrations..." unless unlinked_migrations.empty? and unused_migrations.empty?
        
        unlinked_migration_targets = unlinked_migrations.collect{ |migration|
          File.basename(migration) }
        
        unused_migrations.each do |migration, target|
          unless unlinked_migration_targets.include? File.basename(migration)
            migration =~ /(\d+)_[^.]+\.rb/
            system.user_run(system.web_user, "rake db:migrate:down VERSION=#{$1}")
          end
          FileUtils.rm_rf(migration, :verbose => true)
        end
        unlinked_migrations.each do |migration|
          FileUtils.ln_s(migration, "db/migrate/#{File.basename(migration)}", :verbose => true)
        end
        
        unless unlinked_migrations.empty?
          Shuttle.log system.user_run(system.web_user, "pwd")
          system.user_run(system.web_user, "rake db:migrate")
        end
      end
      
      def gsub_file(path, pattern=nil, replace=nil, &block)
        if File.exist? path
          content = File.read(path)
          if block
            block.call(content)
          else
            return false unless content.gsub!(pattern, replace)
          end
          File.open(path, 'w+') { |f| f.write content }
        end
      end
      
      def linked_migrations
        @linked_migrations ||= Dir.glob('db/migrate/*.rb').select { |migration| File.symlink?(migration) }.inject({}) { |m, migration| m[migration] = File.readlink(migration) ; m }
      end
      
      def unused_migrations
        @unused_migrations ||= linked_migrations.dup
      end
      
      def unlinked_migrations
        @unlinked_migrations ||= []
      end
      
      module Helper
        
        def gem_install(name, options={})
          gem_cmd('install', name, options)
        end
        
        def gem_installed(name, options)
          version = options[:version] || '0.0.0'
          options = { :version => version, :installed => true }
          (gem_cmd('list', name, options).strip == 'true')
        end
        
        def gem_update(name, options={})
          !(gem_cmd('update', name, options) =~ /Nothing to update/)
        end
        
        def ensure_precense_of_gem(name, options={})
          if !gem_installed(name, options)
            gem_install(name, options)
          end
        end
        
        def gem_cmd(cmd, args, options={})
          user   = options.delete(:user)
          user ||= (install_gems_with_web_user ?  web_user : system_user)
          
          args = [args].flatten.compact
          args.collect! { |a| (a and a.inspect) || '' }
          args += options.map do |k,v|
            if TrueClass === v
              "--#{k}"
            else
              "--#{k}=#{v.inspect}"
            end
          end
          user_run(user, "gem #{cmd} #{args.join(' ')}")
        end
        
      end
      
      module Config
        
        def ruby_path(&block)
          option(:ruby_path, block) { |s, v| v or find_bin('ruby', 'ruby1.8', 'ruby18') }
        end
        
        def gem_bin_path(&block)
          option(:gem_bin_path, block) { |s, v| v or find_bin('gem', 'gem1.8', 'gem18') }
        end
        
        def rails_path(&block)
          option(:rails_path, block) do |v|
            user = (install_gems_with_web_user ?  web_user : system_user)
            v or user_find_bin(user, 'rails')
          end
        end
        
        def web_group(&block)
          satellite_option(:web_group, block)
        end
        
        def web_user(&block)
          satellite_option(:web_user, block)
        end
        
        def system_user(&block)
          option(:system_user, block) { |v| v or 'root' }
        end
        
        def install_gems_with_web_user(&block)
          option(:install_gems_with_web_user, block)
        end
        
        def satellite_root(&block)
          satellite_option(:satellite_root, block)
        end
        
        def shared_root(&block)
          satellite_option(:shared_root, block)
        end
        
      end
      
    end
  end
end
