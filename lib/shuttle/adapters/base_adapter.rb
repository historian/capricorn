
module Shuttle
  module Adapters # :nodoc:
    class BaseAdapter < Shuttle::Adapter
      
      on_install   :create_rails_app
      on_link      :link_engines
      on_uninstall :destroy_rails_app
      
      def create_rails_app
        system.as_user(system.web_user, system.web_group) do
          FileUtils.mkdir_p(File.dirname(system.satellite_root), :verbose => true)
          FileUtils.mkdir_p(system.shared_root, :verbose => true)
        end
        
        Dir.chdir(File.dirname(system.satellite_root)) do
          system.user_run system.web_user, "rails #{File.basename(system.satellite_root)}"
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
          
          system.user_popen(system.web_user, "#{system.engines_path} sync", "w") do |f|
            f.write YAML.dump(satellite.engines)
          end
          system.user_run system.web_user, "#{system.engines_path} link"
          
        end
      end
      
    private
      
      def link(src, dst)
        FileUtils.mkdir_p(File.dirname(dst), :verbose => true)
        FileUtils.mkdir_p(src, :verbose => true)
        FileUtils.rm_rf dst, :verbose => true
        FileUtils.symlink(src, dst, :verbose => true)
      end
      
    public
      
      module Macros
        
        def ruby_path(&block)
          option(:ruby_path, block) { |s, v| v or find_bin('ruby', 'ruby1.8', 'ruby18') }
        end
        
        def gem_bin_path(&block)
          option(:gem_bin_path, block) { |s, v| v or find_bin('gem', 'gem1.8', 'gem18') }
        end
        
        def gem_paths(&block)
          option(:gem_paths, block) do |s, value|
            value = [value].flatten.compact.uniq
            value = Shuttle.system_profile.gem_paths if value.empty?
            value
          end
        end
        
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
        
        def gem_bin_paths
          @gem_bin_paths ||= gem_paths.collect { |p| File.join(p, 'bin') }
        end
        
        def rails_path(&block)
          option(:rails_path, block) do |s, v|
            user = (install_gems_with_web_user ?  web_user : system_user)
            v or user_find_bin(user, 'rails')
          end
        end
        
        def engines_path(&block)
          option(:engines_path, block) do |s, v|
            user = (install_gems_with_web_user ?  web_user : system_user)
            v or user_find_bin(user, 'engines')
          end
        end
        
        def web_group(&block)
          option(:web_group, block)
        end
        
        def web_user(&block)
          option(:web_user, block)
        end
        
        def system_user(&block)
          option(:system_user, block) { |s,v| v or 'root' }
        end
        
        def install_gems_with_web_user(&block)
          option(:install_gems_with_web_user, block)
        end
        
        def satellite_root(&block)
          option(:satellite_root, block)
        end
        
        def shared_root(&block)
          option(:shared_root, block)
        end
        
      end
      
    end
  end
end
