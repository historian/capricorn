module Capr::Helpers::Shell

  # def checkout_branches(repo_path, msg_pipe, url, branches)
  #   branch = branches.shift
  #
  #   unless branch
  #     msg_pipe.succeed
  #     return
  #   end
  #
  #   branch_path = work_tree(url, branch)
  #   if File.directory?(branch_path)
  #     FileUtils.rm_rf(branch_path)
  #   end
  #
  #   FileUtils.mkdir_p(branch_path)
  #
  #   cmd = exec('git', "--git-dir=#{repo_path.inspect}",
  #                     "--work-tree=#{branch_path.inspect}",
  #                     'reset', '--hard', "origin/#{branch}")
  #   cmd.callback do
  #     msg_pipe.receive_message(
  #       :success => true,
  #       :message => "Unpacked #{branch} from #{url}",
  #       :verbose => cmd.output)
  #
  #     checkout_branches(repo_path, msg_pipe, url, branches)
  #   end
  #
  #   cmd.errback do
  #     FileUtils.rm_rf(branch_path)
  #
  #     msg_pipe.receive_message(
  #       :success => false,
  #       :message => "Failed to unpack #{branch} from #{url}",
  #       :verbose => cmd.output)
  #
  #     checkout_branches(repo_path, msg_pipe, url, branches)
  #   end
  # end

  def exec(*parts)
    parts = parts.flatten.compact

    options = parts.pop if Hash === parts.last
    options ||= { :pwd => File.expand_path('.') }

    cmd = Shellwords.join(parts)
    cmd = "cd #{options[:pwd].inspect} && #{cmd} 2>&1"
    puts cmd
    EM.popen("bash -c #{cmd.inspect}", ShellPipe)
  end

  class ShellPipe < EM::Connection
    include EM::Deferrable

    attr_reader :output

    def post_init
      @output = ""
    end

    def stream(&block)
      @stream_clb = block
    end

    def receive_data(data)
      @output.concat data
      @stream_clb.call(data) if @stream_clb
    end

    def unbind
      if get_status.exitstatus == 0
        succeed
      else
        fail
      end
    end
  end

end