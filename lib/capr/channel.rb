class Channel

  include EM::Deferrable

  def initalize(base=nil)
    @base = base
    @actions = []
    @is_running = false
  end

  def exec(action, *args)
    action = Action.new(self, action, args)
    action.errback  method(:fail)
    action.callback method(:continue)
    @actions << action

    unless @is_running
      continue
    end

    action
  end

  class Action
    include EM::Deferrable

    def initalize(channel, action, args)
      @channel, @action, @args = channel, action, args
    end

    def call
      @action.call(*@args, self)
    end

    def method_missing(m, *args, &block)
      @channel.__send__(m, *args, &block)
    end

  end

  def method_missing(m, *args, &block)
    if @base
      @base.__send__(m, *args, &block)
    else
      super
    end
  end

private

  def fail(*)
    @is_running = false
    super
  end

  def continue(*)
    if action = @actions.shift
      @is_running = true
      actions.call
    else
      @is_running = false
      succeed
    end
  end

end