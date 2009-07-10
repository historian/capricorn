
module Capricorn
  module Actors # :nodoc:
    class BaseActor < Capricorn::Actor
      
      on_install_satellite   :create_rails_app
      on_link_satellite      :link_engines
      on_uninstall_satellite :destroy_rails_app
      
      # create a new rails app for the current satellite
      def create_rails_app
        system.as_user(system.web_user, system.web_group) do
          FileUtils.mkdir_p(File.dirname(system.satellite_root), :verbose => true)
          FileUtils.mkdir_p(system.shared_root, :verbose => true)
        end
        
        Dir.chdir(File.dirname(system.satellite_root)) do
          system.user_run system.web_user, "rails --force #{system.satellite_root}"
        end
        
        system.as_user(system.web_user, system.web_group) do
          Dir.chdir(File.dirname(system.satellite_root)) do
            link(File.join(system.shared_root,    'public'),
                 File.join(system.satellite_root, 'public', 'system'))
            link(File.join(system.shared_root,    'private'),
                 File.join(system.satellite_root, 'db', 'system'))
            link(File.join(system.shared_root,    'log'),
                 File.join(system.satellite_root, 'log'))
            link(File.join(system.shared_root,    'settings'),
                 File.join(system.satellite_root, 'config', 'settings'))
          end
          
        end
      end
      
      # destroy the rails app for the current satellite
      def destroy_rails_app
        FileUtils.rm_rf system.satellite_root, :verbose => true
      end
      
      # link the required engines for the current satellite
      def link_engines
        Dir.chdir(system.satellite_root) do
          system.as_user(system.web_user, system.web_group) do
            
            clean_index_html_file
            write_environment
            clean_links
            @dependecies.reverse_each do |spec|
              link_engine(spec)
            end
            
            FileUtils.symlink(
              File.join(system.satellite_root, "public"),
              File.join(system.satellite_root, "public/vendor", satellite.module_name),
              :verbose => true) rescue nil
            
            if File.exist? 'public/crossdomain.xml'
              File.unlink 'public/crossdomain.xml'
            end
            File.open('public/crossdomain.xml', 'w+') do |f|
              f.write %{<?xml version="1.0" encoding="UTF-8"?>
<cross-domain-policy xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance" xsi:noNamespaceSchemaLocation="http://www.adobe.com/xml/schemas/PolicyFile.xsd">
  <allow-access-from domain="*" />
  <site-control permitted-cross-domain-policies="master-only"/>
  <allow-http-request-headers-from domain="*" headers="*" secure="false"/>
</cross-domain-policy>}
            end
            
          end
          run_migrations
        end
      end
      
    private
      
      def clean_index_html_file
        index_html = File.join(system.satellite_root, 'public', 'index.html')
        if File.file?(index_html)
          FileUtils.rm_f(index_html, :verbose => true) rescue nil
        end
      end
      
      def link(src, dst)
        FileUtils.mkdir_p(File.dirname(dst), :verbose => true)
        FileUtils.mkdir_p(src, :verbose => true)
        FileUtils.rm_rf dst, :verbose => true
        FileUtils.symlink(src, dst, :verbose => true)
      end
      
      def write_environment
        @dependecies = Capricorn::Satellite::DependencyLoader.load_for(satellite.engines)
        @dependecies.engines
        
        rails_header = "Rails::Initializer.run do |config|\n"
        gems_header  = "  # Gems added by engine_manager:\n  \n"
        
        gsub_file('config/environment.rb') do |content|
          content.gsub!(/^\s*config.gem.+\n/, '')
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
        Capricorn.log "linking: #{spec.name}..."
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
        
        path = File.join(spec.full_gem_path, 'config/locales')
        if File.directory?(path)
          FileUtils.mkdir_p("config/locales", :verbose => true)
          Dir.glob("#{path}/*.{rb,yml,yaml}").each do |locale_file|
            FileUtils.ln_s(locale_file, "config/locales/#{File.basename(locale_file)}",
              :verbose => true) rescue nil
          end
        end
        
        path = File.join(spec.full_gem_path, 'db', 'migrate')
        if File.directory?(path)
          FileUtils.mkdir_p("db/migrate", :verbose => true)
          unlinked_migrations.concat(Dir.glob("#{path}/*.rb"))
          linked_migrations.each do |migration, target|
            if target[0,spec.full_gem_path.size] == spec.full_gem_path
              unlinked_migrations.delete(target)
              unused_migrations.delete(migration)
            end
          end
        end
      end
      
      def clean_links
        Dir.glob("config/locales/*").each do |path|
          FileUtils.rm_rf(path) if File.symlink?(path)
        end
        FileUtils.rm_rf("lib/tasks/vendor", :verbose => true)
        FileUtils.rm_rf("public/vendor", :verbose => true)
      end
      
      def run_migrations
        Capricorn.log "running your migrations..." unless unlinked_migrations.empty? and unused_migrations.empty?
        
        unlinked_migration_targets = unlinked_migrations.collect{ |migration|
          File.basename(migration) }
        
        unused_migrations.each do |migration, target|
          unless unlinked_migration_targets.include? File.basename(migration)
            migration =~ /(\d+)_[^.]+\.rb/
            system.user_run(system.web_user, "cd #{system.satellite_root} ; rake db:migrate:down RAILS_ENV=#{system.rails_environment} VERSION=#{$1}")
          end
          FileUtils.rm_rf(migration, :verbose => true)
        end
        system.as_user(system.web_user, system.web_group) do
          unlinked_migrations.each do |migration|
            
            begin
              FileUtils.ln_s(migration, "db/migrate/#{File.basename(migration)}", :verbose => true)
            rescue
              FileUtils.rm_rf("db/migrate/#{File.basename(migration)}", :verbose => true)
              retry
            end
              
          end
        end
        
        unless unlinked_migrations.empty?
          system.user_run(system.web_user, "cd #{system.satellite_root} ; rake db:migrate RAILS_ENV=#{system.rails_environment}")
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
        
        # install a gem.
        def gem_install(name, options={})
          gem_cmd('install', name, options)
          gem_refresh
        end
        
        def gem_refresh
          original_paths = [Gem.path].flatten
          Gem.refresh
          Gem.send(:set_paths, original_paths.compact.join(File::PATH_SEPARATOR))
        end
        
        # check if a gem is installed
        def gem_installed(name, options)
          version = options[:version] || '0.0.0'
          options = { :version => version, :installed => true }
          (gem_cmd('list', name, options).strip =~ /true/)
        end
        
        # update a gem
        def gem_update(name, options={})
          output = if name == :all
            gem_cmd('update', nil, options)
          else
            gem_cmd('update', name, options)
          end
          if !(output =~ /Nothing to update/)
            gem_refresh
            true
          else
            false
          end
        end
        
        # ensure the presence of a gem
        def ensure_presence_of_gem(name, options={})
          if !gem_installed(name, options)
            gem_install(name, options)
          end
        end
        
      private
        
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
          user_run(user, "#{gem_bin_path} #{cmd} #{args.join(' ')}")
        end
        
      end
      
      module Config
        
        # set the path to the ruby executable.
        def ruby_path(&block)
          option(:ruby_path, block) { |s, v| v or find_bin('ruby', 'ruby1.8', 'ruby18') }
        end
        
        # set the path to the gem executable.
        def gem_bin_path(&block)
          option(:gem_bin_path, block) { |s, v| v or find_bin('gem', 'gem1.8', 'gem18') }
        end
        
        # set the path to the rails executable.
        def rails_path(&block)
          option(:rails_path, block) do |v|
            user = (install_gems_with_web_user ?  web_user : system_user)
            v or user_find_bin(user, 'rails')
          end
        end
        
        # set the owner group of the current satellite.
        def web_group(&block)
          satellite_option(:web_group, block)
        end
        
        # set the owner of the current satellite.
        def web_user(&block)
          satellite_option(:web_user, block)
        end
        
        # set the system user (the user which runs the capricorn server).
        def system_user(&block)
          option(:system_user, block) { |v| v or 'root' }
        end
        
        # set whether gems should be installed with the web user.
        def install_gems_with_web_user(&block)
          option(:install_gems_with_web_user, block)
        end
        
        # set the path to the satellite's root path
        def satellite_root(&block)
          satellite_option(:satellite_root, block)
        end
        
        # set the path to the satellite's shared path
        def shared_root(&block)
          satellite_option(:shared_root, block)
        end
        
        # set the rails environment.
        def rails_environment(&block)
          satellite_option(:rails_environment, block) { |s,v| v or 'development' }
        end
        
      end
      
    end
  end
end
