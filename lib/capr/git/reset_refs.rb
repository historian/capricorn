class Capr::Git::ResetRefs < Capr::Do::Action(:reset_ref)

  include Capr::Helpers::Shell
  include Capr::Helpers::Config
  include Capr::Helpers::Shared

  define_callback :message

  def initialize(url, refs, options={})
    @url, @branch, @refs = url, options[:branch], refs.to_a
  end

  def reset_ref
    if @refs.empty?
      if @branch
        reset_work_tree
      else
        succeed
      end
    else
      ref, sha1 = @refs.shift
      cmd = exec('git', 'update-ref', ref, sha1,
                        :pwd => work_tree(@url, @branch))

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

  def reset_work_tree
    cmd = exec('git', 'reset', '--hard', @branch,
                      :pwd => work_tree(@url, @branch))

    cmd.callback do
      fire_message(
        :success => true,
        :message => "Reset #{@repo}##{@branch}",
        :verbose => cmd.output)
      succeed
    end

    cmd.errback do
      fire_message(
        :success => false,
        :message => "Failed to reset #{@repo}##{@branch}",
        :verbose => cmd.output)
      fail
    end
  end

end