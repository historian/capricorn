module Capr::ShellHelpers
  
  def update_repo(url, branches)
    msg_pipe  = MessagePipe.new
    repo_path = repo_path_for_url(url)

    if File.directory?(repo_path)
      cmd = ['git', '--git-dir', repo_path, 'fetch', 'origin']
      msg_succ = "Fetched #{url}"
      msg_erro = "Failed to fetch #{url}"
    else
      cmd = ['git', '--bare', 'clone', url, repo_path]
      msg_succ = "Cloned #{url}"
      msg_erro = "Failed to clone #{url}"
    end

    cmd = exec(cmd)
    cmd.callback do
      msg_pipe.receive_message(
        :success => true,
        :message => msg_succ,
        :verbose => cmd.output)
      
      checkout_branches(repo_path, msg_pipe, url, branches.dup)
    end
    cmd.errback do
      msg_pipe.receive_message(
        :success => false,
        :message => msg_erro,
        :verbose => cmd.output)
      msg_erro.fail
    end
    
    msg_pipe
  end
  
  def checkout_branches(repo_path, msg_pipe, url, branches)
    branch = branches.shift
    
    unless branch
      msg_pipe.succeed
      return
    end
    
    branch_path = branch_path_for_and_branch(url, branch)
    if File.directory?(branch_path)
      FileUtils.rm_rf(branch_path)
    end
    
    FileUtils.mkdir_p(branch_path)
    
    cmd = exec('git', '--git-dir', repo_path, '--work-tree', branch_path, 'reset', '--hard', "origin/#{branch}")
    cmd.callback do
      msg_pipe.receive_message(
        :success => true,
        :message => "Unpacked #{branch} from #{url}",
        :verbose => cmd.output)
        
      checkout_branches(repo_path, msg_pipe, url, branches)
    end
    
    cmd.errback do
      FileUtils.rm_rf(branch_path)
      
      msg_pipe.receive_message(
        :success => false,
        :message => "Failed to unpack #{branch} from #{url}",
        :verbose => cmd.output)
      
      checkout_branches(repo_path, msg_pipe, url, branches)
    end
  end

  def exec(*parts)
    parts = parts.flatten.compact
    cmd = Shellwords.join(*parts)
    EM.popen("(#{cmd}) 2>&1", ShellPipe)
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
      @output << data
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
  
  class MessagePipe
    
    def initialize
      @on_message = []
    end
    
    include EM::Deferrable
    
    def on_message(&block)
      @on_message << block
    end
    
    def receive_message(message)
      @on_message.each { |clb| clb.call(message) }
    end
    
  end
  
end