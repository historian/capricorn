class Capr::Git::ResetRefs < Capr::Do::Action(:reset_ref)

  include Capr::Helpers::Shell
  include Capr::Helpers::Config
  include Capr::Helpers::Shared

  define_callback :message

  def initialize(url, refs={})
    @url, @refs = url, refs.to_a
  end

  def reset_ref
    if @refs.empty?
      success
    else
      ref, sha1 = @refs.shift
      
      git_dir   = git_dir(@url)
      work_tree = work_tree(@url, ref)
      cmd = exec('git', '--git-dir',   git_dir,
                        '--work_tree', work_tree,
                        'reset', '--hard', sha1)

      cmd.callback do
        fire_message(
          :success => true,
          :message => "Reset #{@repo}##{ref} to #{sha1}",
          :verbose => cmd.output)
        reset_ref
      end
      
      cmd.errback do
        fire_message(
          :success => false,
          :message => "Failed to reset #{@repo}##{ref}",
          :verbose => cmd.output)
        reset_ref
      end
    end
  end

end

