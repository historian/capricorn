
module Capricorn
  class System
    module Shell
      
      def run(cmd)
        as_user 'root', 'wheel' do
          Capricorn.log "[#{get_user_name}]> "+cmd
          cmd = "bash -l -c #{cmd.inspect}"
          output = %x[#{cmd} 2>&1]
          Capricorn.log output if development?
          output
        end
      end
      
      def user_run(username, cmd)
        as_user 'root', 'wheel' do
          Capricorn.log "[#{username}]> "+cmd
          cmd = "su #{username} -l -c #{cmd.inspect}"
          output = %x[#{cmd} 2>&1]
          Capricorn.log output if development?
          output
        end
      end
      
      def popen(cmd, *args, &block)
        as_user 'root', 'wheel' do
          Capricorn.log cmd
          cmd = "bash -l -c #{cmd.inspect}"
          IO.popen(cmd, *args, &block)
        end
      end
      
      def user_popen(username, cmd, *args, &block)
        as_user 'root', 'wheel' do
          Capricorn.log cmd
          cmd = "su #{username} -l -c #{cmd.inspect}"
          IO.popen(cmd, *args, &block)
        end
      end
      
      def find_bin(*names)
        names = names.flatten.compact.uniq
        unless names.empty?
          names = names.join(' ')
          popen("which #{names}", 'r') do |f|
            
            until f.eof?
              path = f.readline
              next unless path
              path = path.strip
              next if path =~ /(Last login)|(You have new)|(Using ruby)/
              return path
            end
            
          end
        end
      end
      
      def user_find_bin(username, *names)
        names = names.flatten.compact.uniq
        unless names.empty?
          names = names.join(' ')
          user_popen(username, "which #{names}", 'r') do |f|
            
            until f.eof?
              path = f.readline
              next unless path
              path = path.strip
              next if path =~ /(Last login)|(You have new)|(Using ruby)/
              return path
            end
            
          end
        end
      end
      
    end
  end
end
