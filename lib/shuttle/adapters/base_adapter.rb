
module Shuttle
  module Adapters
    class BaseAdapter < Shuttle::Adapter
      
      on_install   :create_rails_app
      on_link      :link_engines
      on_uninstall :destroy_rails_app
      
      def create_rails_app
        switch_to_user(system.web_user, system.web_group) do
          
          FileUtils.mkdir_p(File.dirname(system.satellite_root))
          FileUtils.mkdir_p(system.shared_root)
          Dir.chdir(File.dirname(system.satellite_root)) do
            run "rails #{File.basename(system.satellite_root)}"
            
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
        FileUtils.rm_rf system.satellite_root
      end
      
      def link_engines
        switch_to_user(system.web_user, system.web_group) do
          Dir.chdir(system.satellite_root) do
            
            IO.popen(build_cmd("#{system.engines_path} sync"), "w") do |f|
              f.write YAML.dump(satellite.engines)
            end
            run "#{system.engines_path} link"
            
          end
        end
      end
      
    private
      
      def link(src, dst)
        FileUtils.mkdir_p(File.dirname(dst))
        FileUtils.mkdir_p(src)
        FileUtils.rm_rf dst
        File.symlink(src, dst)
      end
      
      def run(cmd)
        Object.send :system, build_cmd(cmd)
      end
      
      def build_cmd(cmd)
        if system.gem_search_paths and !system.gem_search_paths.empty?
          cmd = "export GEM_PATH=#{system.gem_search_paths.join(':')} ;" + cmd
        end
        cmd
      end
      
    public
      
      module Macros
        
        def ruby_path(&block)
          option(:ruby_path, block)
        end
        
        def gem_path(&block)
          option(:gem_path, block)
        end
        
        def gem_search_paths(&block)
          option(:gem_search_paths, block) { |s, value| [value].flatten.compact.uniq }
        end
        
        def rails_path(&block)
          option(:rails_path, block)
        end
        
        def engines_path(&block)
          option(:engines_path, block)
        end
        
        def web_group(&block)
          option(:web_group, block)
        end
        
        def web_user(&block)
          option(:web_user, block)
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
